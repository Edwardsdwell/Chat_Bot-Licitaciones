suppressPackageStartupMessages({
    library(jsonlite)
})

# Zona horaria fija: evita que el catálogo "venza" a media tarde si el servidor está en UTC.
Sys.setenv(TZ = Sys.getenv("LA_PALA_TZ", "America/La_Paz"))

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0L || all(is.na(x))) y else x
}

texto_seguro <- function(x, por_defecto = "") {
  if (is.null(x) || !length(x) || is.list(x) || is.na(x[[1]])) return(por_defecto)
  valor <- trimws(as.character(x[[1]]))
  if (nzchar(valor)) valor else por_defecto
}

encontrar_raiz <- function() {
  variable <- Sys.getenv("LA_PALA_ROOT", unset = "")
  candidatos <- c(variable, getwd(), dirname(getwd()))
  for (marco in rev(sys.frames())) {
    ruta <- marco$ofile %||% marco$srcfile$filename %||% ""
    if (length(ruta) && nzchar(ruta)) {
      candidatos <- c(candidatos, dirname(normalizePath(ruta, winslash = "/", mustWork = FALSE)))
    }
  }
  for (candidato in unique(candidatos[nzchar(candidatos)])) {
    actual <- normalizePath(candidato, winslash = "/", mustWork = FALSE)
    for (paso in seq_len(10L)) {
      if (file.exists(file.path(actual, "frontend", "index.html")) &&
          file.exists(file.path(actual, "scraping", "sources.json"))) return(actual)
      padre <- dirname(actual)
      if (identical(padre, actual)) break
      actual <- padre
    }
  }
  normalizePath(getwd(), winslash = "/", mustWork = FALSE)
}

RAIZ <- encontrar_raiz()
DIR_DATOS <- Sys.getenv("LA_PALA_DATA_DIR", file.path(RAIZ, "data"))
DIR_LOGS <- Sys.getenv("LA_PALA_LOG_DIR", file.path(RAIZ, "logs"))
dir.create(DIR_DATOS, recursive = TRUE, showWarnings = FALSE)
dir.create(DIR_LOGS, recursive = TRUE, showWarnings = FALSE)
ARCHIVO_CATALOGO <- Sys.getenv("LA_PALA_CATALOG_FILE", file.path(DIR_DATOS, "catalogo.json"))
ARCHIVO_FUENTES <- Sys.getenv("LA_PALA_SOURCES_FILE", file.path(RAIZ, "scraping", "sources.json"))
ARCHIVO_ESTADO_RECOLECCION <- file.path(DIR_LOGS, "estado_recoleccion.json")
ARCHIVO_BLOQUEO_RECOLECCION <- file.path(DIR_LOGS, "recoleccion.lock")
ARCHIVO_RECOLECCION_ENCOLADA <- file.path(DIR_LOGS, "recoleccion.encolada")
ARCHIVO_FALLAS <- file.path(DIR_LOGS, "fallas.txt")

marca_reciente <- function(ruta, limite, unidad = "mins") {
  if (!file.exists(ruta)) return(FALSE)
  marca <- tryCatch(file.info(ruta)$mtime, error = function(e) NA)
  if (length(marca) != 1L || is.na(marca)) return(FALSE)
  edad <- suppressWarnings(as.numeric(difftime(Sys.time(), marca, units = unidad)))
  is.finite(edad) && edad >= 0 && edad < limite
}
source(file.path(RAIZ, "analisis", "motor.R"), local = .GlobalEnv)

# Módulos del chat: preprocesamiento/extracción/redacción (obligatorio) y agente IA (opcional).
cargar_modulo <- function(nombre, obligatorio = FALSE) {
  candidatos <- unique(c(file.path(RAIZ, "backend", nombre), file.path(RAIZ, nombre), file.path(getwd(), nombre), file.path(getwd(), "backend", nombre)))
  ruta <- candidatos[file.exists(candidatos)][1L]
  if (is.na(ruta)) {
    if (obligatorio) stop("No se encontró el módulo ", nombre, " junto a nucleo.R.")
    return(invisible(FALSE))
  }
  source(ruta, local = .GlobalEnv)
  invisible(TRUE)
}

registrar_falla <- function(origen, error) {
  dir.create(dirname(ARCHIVO_FALLAS), recursive = TRUE, showWarnings = FALSE)
  detalle <- gsub("[\r\n\t]+", " ", conditionMessage(error))
  linea <- sprintf("%s [ERROR] %s: %s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"), origen, detalle)
  try(cat(linea, file = ARCHIVO_FALLAS, append = TRUE), silent = TRUE)
  invisible(NULL)
}

ruta_rscript <- function() {
  b <- Sys.which("Rscript")
  if (nzchar(b)) b else file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")
}

# Lanza un script en segundo plano. Las variables de entorno se fijan con Sys.setenv
# (no con env= de system2, que no las entrecomilla y permite inyección de comandos).
# --- Límite de memoria (Render gratis: 512 MB) ---------------------------------
# Lee el consumo real (sin caché de disco) del cgroup; devuelve Inf si no se puede medir.
MIN_MEMORIA_MB <- suppressWarnings(as.numeric(Sys.getenv("LA_PALA_MIN_MEMORIA_MB", "220")))
if (is.na(MIN_MEMORIA_MB)) MIN_MEMORIA_MB <- 220

memoria_libre_mb <- function() {
  numero <- function(ruta) {
    if (!file.exists(ruta)) return(NA_real_)
    v <- suppressWarnings(as.numeric(tryCatch(readLines(ruta, n = 1L, warn = FALSE), error = function(e) NA)))
    if (length(v) == 1L) v else NA_real_
  }
  limite <- numero("/sys/fs/cgroup/memory.max")
  if (is.na(limite)) limite <- numero("/sys/fs/cgroup/memory/memory.limit_in_bytes")
  if (is.na(limite) || limite > 1e13) return(Inf)
  uso <- NA_real_
  for (par in list(c("/sys/fs/cgroup/memory.stat", "anon"), c("/sys/fs/cgroup/memory/memory.stat", "total_rss"))) {
    if (!file.exists(par[[1]])) next
    lineas <- tryCatch(readLines(par[[1]], warn = FALSE), error = function(e) character())
    fila <- grep(paste0("^", par[[2]], " "), lineas, value = TRUE)
    if (length(fila)) { uso <- suppressWarnings(as.numeric(sub(".* ", "", fila[[1]]))); break }
  }
  if (is.na(uso)) uso <- numero("/sys/fs/cgroup/memory.current")
  if (is.na(uso)) uso <- numero("/sys/fs/cgroup/memory/memory.usage_in_bytes")
  if (is.na(uso)) return(Inf)
  (limite - uso) / 1048576
}
memoria_suficiente <- function() memoria_libre_mb() >= MIN_MEMORIA_MB

# Solo un proceso pesado de R a la vez: cada Rscript hijo ocupa cientos de MB.
hay_analisis_en_curso <- function() {
  bloqueos <- list.files(DIR_TRABAJOS, pattern = "\\.lock$", full.names = TRUE)
  if (!length(bloqueos)) return(FALSE)
  edad <- as.numeric(difftime(Sys.time(), file.info(bloqueos)$mtime, units = "mins"))
  any(!is.na(edad) & edad < 10)
}

lanzar_rscript <- function(archivo, args = character(), entorno = character()) {
  if (!memoria_suficiente()) stop("Memoria insuficiente para lanzar un proceso en segundo plano.")
  nombres <- names(entorno)
  if (length(nombres)) {
    previos <- Sys.getenv(nombres, unset = NA_character_, names = FALSE)
    on.exit({
      for (i in seq_along(nombres)) {
        if (is.na(previos[[i]])) Sys.unsetenv(nombres[[i]])
        else do.call(Sys.setenv, stats::setNames(list(previos[[i]]), nombres[[i]]))
      }
    }, add = TRUE)
    do.call(Sys.setenv, as.list(entorno))
  }
  system2(ruta_rscript(), args = shQuote(c(archivo, as.character(args))),
          stdout = FALSE, stderr = FALSE, wait = FALSE)
}

leer_json <- function(ruta, alternativo = list()) {
  if (!file.exists(ruta) || isTRUE(file.info(ruta)$isdir) || file.info(ruta)$size == 0) return(alternativo)
  tryCatch(jsonlite::fromJSON(ruta, simplifyVector = FALSE), error = function(e) alternativo)
}

leer_fuentes <- local({
  marca <- NULL; valor <- list()
  function() {
    i <- file.info(ARCHIVO_FUENTES); k <- paste(i$mtime, i$size)
    if (!identical(marca, k)) { valor <<- leer_json(ARCHIVO_FUENTES, list()); marca <<- k }
    valor
  }
})

# Recorta textos muy largos del catálogo para que ocupe menos RAM.
aligerar_catalogo <- function(v) {
  if (!is.list(v) || !is.list(v$oportunidades)) return(v)
  limites <- c(descripcion = 2000L, requisitos = 4000L)
  v$oportunidades <- lapply(v$oportunidades, function(o) {
    if (!is.list(o)) return(o)
    for (campo in names(limites)) {
      x <- o[[campo]]
      if (is.character(x) && length(x) == 1L && !is.na(x) && nchar(x) > limites[[campo]]) o[[campo]] <- substr(x, 1L, limites[[campo]])
    }
    # Extracción de texto: departamento, plazo y datos clave (haber, vacantes, contacto, requisitos).
    tryCatch(enriquecer_oportunidad(o), error = function(e) o)
  })
  v
}

catalogo <- local({
  marca <- NULL; valor <- list()
  function() {
    i <- file.info(ARCHIVO_CATALOGO); k <- paste(i$mtime, i$size)
    if (!identical(marca, k)) {
      v <- leer_json(ARCHIVO_CATALOGO, list()); valor <<- if (is.list(v)) aligerar_catalogo(v) else list(); marca <<- k
      rm(v); invisible(gc())
    }
    valor
  }
})

fecha_segura <- function(valor) {
  texto <- texto_seguro(valor)
  if (!nzchar(texto)) return(as.Date(NA))
  as.Date(substr(texto, 1L, 10L), format = "%Y-%m-%d")
}

estado_bloqueos <- function() {
  configuracion <- leer_fuentes()
  todas <- fuentes_configuradas()
  fuentes_noticias <- Filter(function(f) is.list(f) && isTRUE(f$habilitada), configuracion$fuentes_noticias %||% list())
  informes <- catalogo()$fuentes %||% list()
  hallazgos <- lapply(todas, function(f) {
    encontrados <- Filter(function(i) identical(texto_seguro(i$id), texto_seguro(f$id)), informes)
    if (!length(encontrados)) return(list(id = f$id, nombre = f$nombre, estado = "sin_revisar", motivo = "Pendiente de revisión"))
    i <- encontrados[[length(encontrados)]]
    tecnico <- texto_seguro(i$estado); motivo <- texto_seguro(i$motivo)
    normalizado <- if (grepl("robots.txt no_verificable", motivo, fixed = TRUE)) "no_verificada" else if (tecnico == "omitida" && grepl("robots.txt bloqueado", motivo, fixed = TRUE)) "bloqueada_por_reglas" else if (tecnico == "omitida") "no_consultada" else tecnico
    list(id = f$id, nombre = f$nombre, url = f$url, estado = normalizado, motivo = motivo, revisado_en = texto_seguro(i$revisado_en))
  })
  bloqueadas <- Filter(function(x) x$estado %in% c("bloqueada_por_reglas", "requiere_javascript", "requiere_autenticacion", "bloqueada_por_el_portal"), hallazgos)
  no_verificadas <- Filter(function(x) x$estado %in% c("no_verificada", "no_disponible", "error", "sin_revisar", "no_consultada"), hallazgos)
  novedades_normativas <- Filter(function(o) identical(texto_seguro(o$categoria), "normativa"), catalogo()$oportunidades %||% list())
  fechas_normativas <- vapply(novedades_normativas, function(o) texto_seguro(o$fecha_publicacion), character(1))
  novedades_normativas <- head(novedades_normativas[order(fechas_normativas, decreasing = TRUE)], 12L)
  gaceta <- Filter(function(f) identical(texto_seguro(f$id), "gaceta-oficial"), todas)
  list(total = length(todas), con_barrera = length(bloqueadas), no_verificadas = length(no_verificadas), fuentes = hallazgos, fuentes_noticias = fuentes_noticias, noticias = catalogo()$alertas_bloqueos %||% list(consultado_en = "", ventana_horas = 72L, hallazgos = list(), fuentes = list()), normativa = novedades_normativas, gaceta_url = if (length(gaceta)) texto_seguro(gaceta[[1]]$url) else "https://www.gacetaoficialdebolivia.gob.bo/")
}

fuentes_configuradas <- function() {
  valor <- leer_fuentes()
  fuentes <- valor$fuentes %||% valor$sources %||% list()
  if (!is.list(fuentes)) return(list())
  Filter(function(fuente) is.list(fuente) && isTRUE(fuente$habilitada %||% fuente$enabled %||% FALSE), fuentes)
}

fuentes_recomendadas <- function(categoria = "", limite = 3L) {
  registro <- leer_fuentes()
  fuentes <- registro$fuentes %||% registro$sources %||% list()
  if (!is.list(fuentes)) return(list())
  if (nzchar(categoria)) {
    coincidentes <- Filter(function(fuente) identical(texto_seguro(fuente$categoria), categoria), fuentes)
    if (length(coincidentes)) fuentes <- coincidentes
  }
  fuentes <- fuentes[order(!vapply(fuentes, function(fuente) isTRUE(fuente$oficial), logical(1)))]
  lapply(head(fuentes, limite), function(fuente) list(
    nombre = texto_seguro(fuente$nombre, "Fuente pública"),
    url = texto_seguro(fuente$url),
    categoria = texto_seguro(fuente$categoria),
    oficial = isTRUE(fuente$oficial),
    habilitada = isTRUE(fuente$habilitada)
  ))
}

iniciar_recoleccion_si_necesaria <- function(categoria = "") {
  dir.create(dirname(ARCHIVO_BLOQUEO_RECOLECCION), recursive = TRUE, showWarnings = FALSE)
  en_curso <- marca_reciente(ARCHIVO_BLOQUEO_RECOLECCION, 8, "hours")
  en_cola <- marca_reciente(ARCHIVO_RECOLECCION_ENCOLADA, 5, "mins")
  if (en_curso) return(list(estado = "en_curso", iniciado = FALSE))
  if (en_cola) return(list(estado = "en_cola", iniciado = FALSE))
  if (hay_analisis_en_curso()) return(list(estado = "en_cola", iniciado = FALSE))
  if (file.exists(ARCHIVO_BLOQUEO_RECOLECCION)) unlink(ARCHIVO_BLOQUEO_RECOLECCION, recursive = TRUE)
  if (file.exists(ARCHIVO_RECOLECCION_ENCOLADA)) unlink(ARCHIVO_RECOLECCION_ENCOLADA, recursive = TRUE)

  registro <- leer_json(ARCHIVO_ESTADO_RECOLECCION, list())
  fecha_registro <- fecha_segura(registro$actualizado_en)
  if (identical(texto_seguro(registro$estado), "completada") && !identical(texto_seguro(registro$modo), "consulta") && !is.na(fecha_registro) && fecha_registro == Sys.Date()) {
    return(list(estado = "completada", iniciado = FALSE))
  }
  if (identical(texto_seguro(registro$estado), "fallida") && !is.na(fecha_registro) && fecha_registro == Sys.Date() &&
      as.numeric(difftime(Sys.time(), file.info(ARCHIVO_ESTADO_RECOLECCION)$mtime, units = "mins")) < 30) {
    return(list(estado = "fallida", iniciado = FALSE))
  }

  binario <- ruta_rscript()
  archivo <- file.path(RAIZ, "scraping", "run_daily.R")
  if (!nzchar(binario) || !file.exists(binario) || !file.exists(archivo) || !isTRUE(dir.create(ARCHIVO_RECOLECCION_ENCOLADA, showWarnings = FALSE))) {
    detalle <- if (!nzchar(binario) || !file.exists(binario)) "No se encontró Rscript en la instalación activa." else if (!file.exists(archivo)) "No se encontró scraping/run_daily.R." else "No se pudo crear la marca de cola del worker."
    registrar_falla("worker/no_disponible", simpleError(detalle))
    return(list(estado = "no_disponible", iniciado = FALSE))
  }

  solicitud_id <- paste0("consulta-", format(Sys.time(), "%Y%m%d%H%M%S"), "-", Sys.getpid())
  inicio <- tryCatch(lanzar_rscript(archivo, entorno = c(
    LA_PALA_SOLICITUD_ID = solicitud_id,
    LA_PALA_CATEGORIA_PRIORITARIA = categoria
  )), error = function(error) error)
  if (inherits(inicio, "error")) {
    unlink(ARCHIVO_RECOLECCION_ENCOLADA, recursive = TRUE)
    registrar_falla("worker/inicio", inicio)
    return(list(estado = "no_disponible", iniciado = FALSE))
  }
  list(estado = "en_cola", iniciado = TRUE)
}

# Consulta en línea: revisa en vivo las fuentes más relacionadas con la pregunta.
# Usa el mismo recolector prudente (robots.txt, pausas, sin saltar barreras).
iniciar_consulta_en_linea <- function(consulta, categoria = "", forzar = FALSE) {
  consulta <- substr(trimws(texto_seguro(consulta)), 1L, 200L)
  if (!nzchar(consulta)) return(list(estado = "sin_consulta", iniciado = FALSE))
  dir.create(dirname(ARCHIVO_BLOQUEO_RECOLECCION), recursive = TRUE, showWarnings = FALSE)
  if (marca_reciente(ARCHIVO_BLOQUEO_RECOLECCION, 8, "hours")) return(list(estado = "en_curso", iniciado = FALSE))
  if (marca_reciente(ARCHIVO_RECOLECCION_ENCOLADA, 5, "mins")) return(list(estado = "en_cola", iniciado = FALSE))
  if (hay_analisis_en_curso()) return(list(estado = "en_cola", iniciado = FALSE))
  # Evita repetir la misma consulta en línea muchas veces seguidas.
  clave <- substr(digest::digest(paste(normalizar_texto(consulta), categoria), "sha256"), 1L, 16L)
  marca <- file.path(dirname(ARCHIVO_BLOQUEO_RECOLECCION), paste0("consulta-", clave, ".marca"))
  if (!isTRUE(forzar) && marca_reciente(marca, 30, "mins")) return(list(estado = "reciente", iniciado = FALSE))
  binario <- ruta_rscript()
  archivo <- file.path(RAIZ, "scraping", "run_daily.R")
  if (!nzchar(binario) || !file.exists(archivo) || !isTRUE(dir.create(ARCHIVO_RECOLECCION_ENCOLADA, showWarnings = FALSE))) {
    registrar_falla("consulta_en_linea/no_disponible", simpleError("No se pudo preparar el trabajo de consulta en línea."))
    return(list(estado = "no_disponible", iniciado = FALSE))
  }
  solicitud_id <- paste0("enlinea-", format(Sys.time(), "%Y%m%d%H%M%S"), "-", Sys.getpid())
  consulta_limpia <- trimws(gsub("[[:cntrl:]]+", " ", consulta))
  inicio <- tryCatch(lanzar_rscript(archivo, entorno = c(
    LA_PALA_SOLICITUD_ID = solicitud_id,
    LA_PALA_MODO = "consulta",
    LA_PALA_CONSULTA = consulta_limpia,
    LA_PALA_CATEGORIA_PRIORITARIA = categoria
  )), error = function(e) e)
  if (inherits(inicio, "error")) {
    unlink(ARCHIVO_RECOLECCION_ENCOLADA, recursive = TRUE)
    registrar_falla("consulta_en_linea/inicio", inicio)
    return(list(estado = "no_disponible", iniciado = FALSE))
  }
  try(writeLines(as.character(Sys.time()), marca), silent = TRUE)
  list(estado = "en_cola", iniciado = TRUE, en_linea = TRUE)
}

categorias <- list(
  list(id = "convocatorias", nombre = "Convocatorias públicas", descripcion = "Llamados, programas y oportunidades institucionales."),
  list(id = "empleo_publico", nombre = "Empleo del Estado", descripcion = "Procesos de selección y contratación pública."),
  list(id = "contrataciones", nombre = "Compras y SICOES", descripcion = "Licitaciones, bienes, servicios y contrataciones estatales."),
  list(id = "merito", nombre = "Concursos de mérito", descripcion = "Procesos académicos y profesionales con evaluación."),
  list(id = "becas", nombre = "Becas y pasantías", descripcion = "Estudio, movilidad, investigación y prácticas."),
  list(id = "fondos", nombre = "Fondos y financiamiento", descripcion = "Fondos concursables para emprendimientos y organizaciones."),
  list(id = "precios", nombre = "Precios locales", descripcion = "Observaciones con lugar, unidad y fecha de registro."),
  list(id = "normativa", nombre = "Normativa y Gaceta", descripcion = "Leyes, decretos y otras publicaciones oficiales.")
)

oportunidad_abierta <- function(oportunidad) {
  if (!is.list(oportunidad)) return(FALSE)
  categoria <- texto_seguro(oportunidad$categoria)
  titulo <- texto_seguro(oportunidad$titulo)
  url <- texto_seguro(oportunidad$url)
  estado <- texto_seguro(oportunidad$estado, "abierta")
  if (!nzchar(categoria) || !nzchar(titulo) || !grepl("^https://", url, ignore.case = TRUE)) return(FALSE)
  if (estado %in% c("cerrada", "vencida", "eliminada", "duplicada", "rechazada")) return(FALSE)
  verificacion <- fecha_segura(oportunidad$verificado_en %||% oportunidad$extraido_en)
  if (is.na(verificacion) || verificacion != Sys.Date()) return(FALSE)
  limite <- fecha_segura(oportunidad$fecha_limite)
  if (!is.na(limite) && limite < Sys.Date()) return(FALSE)
  TRUE
}

oportunidades_activas <- local({
  marca <- NULL; valor <- list()
  function() {
    k <- paste(file.info(ARCHIVO_CATALOGO)$mtime, file.info(ARCHIVO_CATALOGO)$size, Sys.Date())
    if (!identical(marca, k)) { v <- catalogo()$oportunidades %||% list(); valor <<- if (is.list(v)) Filter(oportunidad_abierta, v) else list(); marca <<- k }
    valor
  }
})

precios_actuales <- function() {
  valor <- catalogo()$observaciones_precios %||% list()
  if (!is.list(valor)) return(list())
  Filter(function(dato) {
    is.list(dato) && nzchar(texto_seguro(dato$producto)) &&
      is.finite(suppressWarnings(as.numeric(texto_seguro(dato$precio, NA_character_)))) &&
      suppressWarnings(as.numeric(texto_seguro(dato$precio, NA_character_))) > 0
  }, valor)
}

limitar <- function(valor, por_defecto = 6L) {
  numero <- suppressWarnings(as.integer(valor))
  if (length(numero) != 1L || is.na(numero) || numero < 1L) return(por_defecto)
  min(numero, 50L)
}

fecha_fin_mes <- function(fecha = Sys.Date()) {
  siguiente_mes <- as.Date(format(fecha, "%Y-%m-01")) + 32
  as.Date(format(siguiente_mes, "%Y-%m-01")) - 1
}

detectar_categoria <- function(consulta, categoria = "") {
  if (nzchar(categoria) && categoria %in% vapply(categorias, `[[`, character(1), "id")) return(categoria)
  texto <- normalizar_texto(consulta)
  reglas <- list(
    precios = "precio|alquiler|farmacia|canasta|repuesto|cuesta|costo|valor",
    becas = "beca(s)?|pasantia(s)?|estudiante|universidad|academica|movilidad|practica(s)?|posgrado|maestria|doctorado|intercambio|estudiar",
    empleo_publico = "empleo(s)?|trabajo(s)?|chamba(s)?|vacante(s)?|puesto(s)?|laboral(es)?|personal|contratacion de personal|cargo publico|servidor publico|recursos humanos|sueldo|salario|remuneracion|haber mensual|postular a un cargo",
    contrataciones = "sicoes|licitacion(es)?|contratacion(es)?|contrato(s)?|compras? publicas?|proveer|proveedor|adquisicion(es)?|bienes y servicios",
    merito = "concurso de merito|merito|evaluacion|seleccion academica",
    fondos = "fondo|financiamiento|financiar|emprend|(^| )ong( |$)|organizacion|capital semilla",
    normativa = "gaceta|normativa|ley(es)?|decreto(s)?|resolucion(es)?|reglamento(s)?|norma(s)?",
    convocatorias = "convocatoria(s)?|llamado(s)?|concurso(s)?|seleccion|proceso de merito"
  )
  for (id in names(reglas)) if (grepl(reglas[[id]], texto)) return(id)
  ""
}

buscar_oportunidades <- function(consulta = "", categoria = "", hasta = NULL, limite = 6L, pre = NULL) {
  if (is.null(pre)) pre <- preprocesar_consulta(consulta)
  resultados <- oportunidades_activas()
  elegida <- detectar_categoria(consulta, categoria)
  if (nzchar(elegida)) resultados <- Filter(function(x) identical(texto_seguro(x$categoria), elegida), resultados)

  # Filtro por departamento: primero coincidencias explícitas; si no hay, las de alcance nacional o sin ubicación.
  relajado <- FALSE
  if (nzchar(pre$departamento)) {
    exactos <- Filter(function(x) coincide_departamento(x, pre$departamento), resultados)
    if (length(exactos)) {
      resultados <- exactos
    } else {
      amplios <- Filter(sin_dato_departamento, resultados)
      relajado <- length(amplios) > 0L
      resultados <- amplios
    }
  }

  fecha_tope <- if (!is.null(hasta) && nzchar(texto_seguro(hasta))) fecha_segura(hasta) else as.Date(NA)
  if (is.na(fecha_tope) && !is.na(pre$fecha_tope)) fecha_tope <- pre$fecha_tope
  if (is.na(fecha_tope) && grepl("fin de mes|final de mes|fin del mes|final del mes|fin mes|final mes|este mes|antes de terminar el mes", pre$normalizada)) {
    fecha_tope <- fecha_fin_mes()
  }
  if (!is.na(fecha_tope)) {
    resultados <- Filter(function(x) {
      limite_oferta <- fecha_segura(x$fecha_limite)
      !is.na(limite_oferta) && limite_oferta <= fecha_tope
    }, resultados)
  }

  texto <- pre$texto_busqueda
  puntajes <- numeric(length(resultados))
  terminos <- expandir_terminos(terminos_consulta(texto))
  if (length(terminos) && length(resultados)) {
    puntajes <- puntuar_bm25(resultados, terminos)
    # Si la persona ya indicó una categoría, conserva sus publicaciones aunque
    # use palabras distintas a las del título; el puntaje solo las ordena.
    if (any(puntajes > 0)) {
      resultados <- resultados[puntajes > 0]
      puntajes <- puntajes[puntajes > 0]
    } else if (!nzchar(elegida) || length(tokenizar(texto)) > 2L) {
      resultados <- list()
      puntajes <- numeric()
    }
  }
  fechas <- vapply(resultados, function(item) { f <- fecha_segura(item$fecha_limite); if (is.na(f)) Inf else as.numeric(f) }, numeric(1))
  if (length(resultados)) resultados <- resultados[order(-puntajes, fechas)]
  list(
    oportunidades = head(resultados, limitar(limite)),
    total = length(resultados),
    categoria = elegida,
    fecha_tope = if (is.na(fecha_tope)) "" else as.character(fecha_tope),
    departamento = pre$departamento,
    relajado = relajado
  )
}

oportunidades_recientes <- function(limite = 6L, categoria = "") {
  resultados <- oportunidades_activas()
  if (nzchar(categoria)) resultados <- Filter(function(item) identical(texto_seguro(item$categoria), categoria), resultados)
  if (!length(resultados)) return(list())
  momentos <- vapply(resultados, function(item) {
    for (campo in c("fecha_publicacion", "extraido_en", "verificado_en")) {
      valor <- texto_seguro(item[[campo]])
      if (!nzchar(valor)) next
      fecha <- tryCatch(suppressWarnings(as.POSIXct(valor, tz = "America/La_Paz", tryFormats = c(
        "%Y-%m-%dT%H:%M:%OS%z", "%Y-%m-%d %H:%M:%S", "%Y-%m-%d"
      ))), error = function(e) NA)
      if (length(fecha) == 1L && !is.na(fecha)) return(as.numeric(fecha))
    }
    0
  }, numeric(1))
  resultados[order(momentos, decreasing = TRUE)] |> head(limitar(limite))
}

buscar_precios <- function(consulta = "", departamento = "", limite = 12L) {
  resultados <- precios_actuales()
  texto <- normalizar_texto(consulta)
  lugar <- normalizar_texto(departamento)
  if (nzchar(texto)) {
    resultados <- Filter(function(dato) grepl(texto, normalizar_texto(paste(dato$producto, dato$marca, dato$descripcion)), fixed = TRUE), resultados)
  }
  if (nzchar(lugar)) {
    resultados <- Filter(function(dato) grepl(lugar, normalizar_texto(paste(dato$ciudad, dato$departamento)), fixed = TRUE), resultados)
  }
  fechas <- vapply(resultados, function(dato) texto_seguro(dato$observado_en), character(1))
  resultados[order(fechas, decreasing = TRUE)] |> head(limitar(limite, 12L))
}

normalizar_contexto_chat <- function(contexto) {
  if (is.list(contexto)) return(contexto)
  texto <- texto_seguro(contexto)
  if (!nzchar(texto) || nchar(texto) > 5000L) return(list())
  tryCatch(jsonlite::fromJSON(texto, simplifyVector = FALSE), error = function(e) list())
}

pedido_detalle <- function(consulta) {
  grepl("mas detalle|mas informacion|informacion detallada|dime mas|amplia la informacion|requisit|document|perfil|fecha limite|plazo|cuando cierra|ubicacion|donde queda|lugar de trabajo|sueldo|salario|remuneracion|como postular|como me inscrib|correo de contacto|enlace exacto|ficha completa", normalizar_texto(consulta))
}

resolver_oportunidad_contextual <- function(consulta, contexto, candidatas = list()) {
  disponibles <- oportunidades_activas()
  buscar_id <- function(id) {
    if (!nzchar(id)) return(NULL)
    hallada <- Filter(function(o) identical(texto_seguro(o$id), id), disponibles)
    if (length(hallada)) hallada[[1L]] else NULL
  }
  seleccion <- texto_seguro(contexto$seleccion)
  item <- buscar_id(seleccion)
  if (!is.null(item)) return(item)
  ids <- as.character(unlist(contexto$ultimos_ids %||% character(), use.names = FALSE))
  if (!length(ids) && length(contexto$turnos)) {
    ultimo <- contexto$turnos[[length(contexto$turnos)]]
    ids <- as.character(unlist(ultimo$ids %||% character(), use.names = FALSE))
  }
  ordinal <- regmatches(normalizar_texto(consulta), regexpr("(^| )(primera|primer|1|segunda|segundo|2|tercera|tercer|3)( |$)", normalizar_texto(consulta)))
  if (length(ordinal) && nzchar(ordinal)) {
    numero <- if (grepl("segund|(^| )2( |$)", ordinal)) 2L else if (grepl("tercer|(^| )3( |$)", ordinal)) 3L else 1L
    if (length(ids) >= numero) item <- buscar_id(ids[[numero]])
    if (is.null(item) && length(candidatas) >= numero) item <- candidatas[[numero]]
    if (!is.null(item)) return(item)
  }
  if (length(ids) == 1L) {
    item <- buscar_id(ids[[1L]])
    if (!is.null(item)) return(item)
  }
  if (length(candidatas) == 1L) return(candidatas[[1L]])
  NULL
}

lector_detalle_publicacion <- local({
  lector <- new.env(parent = .GlobalEnv)
  cache <- new.env(parent = emptyenv())
  cargado <- FALSE
  function(oportunidad, consulta = "") {
    url <- texto_seguro(oportunidad$url)
    clave <- paste0(texto_seguro(oportunidad$id), "|", url)
    entrada <- cache[[clave]]
    if (!is.null(entrada) && as.numeric(difftime(Sys.time(), entrada$hora, units = "mins")) < 20) return(entrada$valor)
    if (!cargado) {
      archivo <- file.path(RAIZ, "scraping", "collect.R")
      if (!file.exists(archivo)) stop("No está disponible el extractor de publicaciones.")
      sys.source(archivo, envir = lector)
      cargado <<- TRUE
    }
    fuente <- Filter(function(x) identical(texto_seguro(x$id), texto_seguro(oportunidad$fuente_id)), fuentes_configuradas())
    if (!length(fuente)) return(list(estado = "no_consultable"))
    valor <- lector$extraer_detalle_publicacion(url, fuente[[1L]], consulta, texto_seguro(oportunidad$titulo))
    if (length(ls(cache)) >= 20L) rm(list = ls(cache), envir = cache)
    cache[[clave]] <- list(hora = Sys.time(), valor = valor)
    valor
  }
})

respuesta_detalle <- function(oportunidad, detalle, consulta) {
  titulo <- texto_seguro(oportunidad$titulo, "la publicación seleccionada")
  entidad <- texto_seguro(oportunidad$entidad, texto_seguro(oportunidad$fuente_nombre, "la fuente pública"))
  if (!identical(texto_seguro(detalle$estado), "ok")) {
    dc <- oportunidad$datos_clave
    resumen <- describir_datos_clave(oportunidad)
    req <- if (is.list(dc) && length(dc$requisitos)) paste0(" Requisitos que figuran en el texto guardado: ", sub("[.]+$", "", paste(as.character(unlist(dc$requisitos)), collapse = " ")), ".") else ""
    plazo <- texto_dias_restantes(fecha_segura(oportunidad$fecha_limite))
    return(paste0("La oferta «", titulo, "» corresponde a ", entidad, " y ", plazo, ".",
      if (nzchar(resumen)) paste0(" Datos que pude extraer: ", resumen, ".") else "", req,
      " No pude leer la ficha completa en esta consulta; abre el enlace para verificar requisitos y forma de postulación en la fuente oficial."))
  }
  campos <- detalle$campos %||% list()
  es_requisito <- grepl("requisit|document|perfil", normalizar_texto(consulta))
  es_plazo <- grepl("plazo|fecha|cierre|cuando vence", normalizar_texto(consulta))
  es_lugar <- grepl("ubicacion|lugar|donde", normalizar_texto(consulta))
  grupos <- if (es_requisito) c("requisitos") else if (es_plazo) c("plazo") else if (es_lugar) c("ubicacion") else c("requisitos", "ubicacion", "postulacion", "remuneracion", "plazo")
  partes <- c(paste0("En «", titulo, "» de ", entidad, ":"))
  for (grupo in grupos) {
    valores <- if (identical(grupo, "plazo")) {
      fecha <- fecha_segura(detalle$fecha_limite)
      if (!is.na(fecha)) paste0("Fecha límite registrada: ", format(fecha, "%d/%m/%Y"), ".") else character()
    } else as.character(unlist(campos[[grupo]] %||% character(), use.names = FALSE))
    valores <- valores[nzchar(valores)]
    if (length(valores)) partes <- c(partes, paste0(switch(grupo, requisitos = "Requisitos", ubicacion = "Ubicación o modalidad", postulacion = "Postulación y contacto", remuneracion = "Remuneración", plazo = "Plazo"), ": ", paste(head(valores, 2L), collapse = " ")))
  }
  if (length(partes) == 1L) {
    partes <- c(partes, "La lectura no identificó un dato explícito para ese punto en el contenido publicado. Revisa el documento oficial antes de postular.")
  } else {
    partes <- c(partes, "Estos datos se extrajeron de la ficha consultada; confirma cualquier cambio en la publicación oficial.")
  }
  paste(partes, collapse = " ")
}

respuesta_chat <- function(consulta, categoria = "", limite = 6L, contexto = NULL) {
  contexto <- normalizar_contexto_chat(contexto)
  texto_estado <- normalizar_texto(consulta)
  pre <- preprocesar_consulta(consulta)
  if (grepl("(^| )hay bloqueos( |$)|bloqueos en las fuentes|fuentes bloqueadas", texto_estado)) {
    fuentes <- catalogo()$fuentes %||% list()
    activas <- Filter(function(x) is.list(x) && isTRUE(x$habilitada %||% TRUE), fuentes)
    barreras <- Filter(function(x) grepl("barrera|bloquead|robots|autentic|javascript|dns|no_verificable", paste(texto_seguro(x$estado), texto_seguro(x$motivo)), ignore.case = TRUE), activas)
    respuesta <- if (length(barreras)) sprintf("Sí. %d de %d fuentes habilitadas tienen una barrera o no pudieron verificarse. Se mantienen en pausa y no se intenta eludir sus controles.", length(barreras), length(activas)) else "En el último registro disponible no aparecen fuentes con barreras confirmadas. Si aún no hay un ciclo registrado, todavía no se puede confirmar el estado."
    return(list(respuesta = respuesta, oportunidades = list(), total = length(barreras), categoria = "", filtro_fecha = "", conversacional = TRUE, fuentes_recomendadas = list(), fuentes_bloqueadas = lapply(barreras, function(x) list(nombre = texto_seguro(x$nombre, x$id), estado = texto_seguro(x$estado), motivo = texto_seguro(x$motivo)))))
  }
  analitica <- respuesta_analitica(consulta)
  if (!is.null(analitica)) return(analitica)
  texto_normalizado <- normalizar_texto(consulta)
  es_saludo <- grepl("^(hola|buenas|buenos dias|buenas tardes|buenas noches|que tal|gracias|muchas gracias)[[:space:][:punct:]]*$", texto_normalizado)
  es_ayuda <- grepl("que puedes hacer|como funciona|como te uso|que es la pala|para que sirves|necesito ayuda|ayudame", texto_normalizado)
  if (es_saludo || es_ayuda) {
    texto_respuesta <- if (grepl("gracias", texto_normalizado)) {
      "¡De nada! Puedo ayudarte a encontrar convocatorias, empleo público, becas, compras, fondos y normativa publicados en fuentes públicas de Bolivia."
    } else if (es_saludo && !es_ayuda) {
      "¡Hola! Puedo buscar oportunidades públicas en Bolivia y mostrarte su fuente original. ¿Qué te interesa: empleo, becas, convocatorias, compras, fondos o normativa?"
    } else {
      "Puedo buscar oportunidades públicas por tema, entidad o fecha límite; por ejemplo: «becas antes de fin de mes», «empleo público en La Paz» o «licitaciones de SICOES». También puedo analizar una dirección HTTPS si la incluyes en tu consulta."
    }
    return(list(respuesta = texto_respuesta, categoria = "", oportunidades = list(), total = 0L,
                filtro_fecha = "", conversacional = TRUE,
                fuentes_recomendadas = fuentes_recomendadas("", 3L)))
  }
  categoria_detectada <- detectar_categoria(consulta, categoria)
  contexto_anterior <- contexto$turnos %||% list()
  # Preguntas de seguimiento ("¿y en Cochabamba?") heredan la categoría del turno anterior.
  if (!nzchar(categoria_detectada) && isTRUE(pre$es_seguimiento) && length(contexto_anterior)) {
    previas <- vapply(contexto_anterior, function(t) texto_seguro(t$categoria), character(1))
    previas <- previas[nzchar(previas)]
    if (length(previas)) categoria_detectada <- previas[[length(previas)]]
  }
  if (identical(pre$intencion, "resumen") && !nzchar(categoria_detectada)) return(respuesta_resumen())
  es_referencial <- grepl("(^| )(esa|ese|esta|este|la primera|el primero|la segunda|el segundo|esa oferta|ese empleo)( |$)", texto_estado)
  if (!nzchar(categoria_detectada) && es_referencial) {
    seleccion_contextual <- resolver_oportunidad_contextual(consulta, contexto)
    if (!is.null(seleccion_contextual)) categoria_detectada <- texto_seguro(seleccion_contextual$categoria)
  }
  if (identical(categoria_detectada, "precios")) {
    precios <- buscar_precios(consulta, limite = 12L)
    respuesta <- if (length(precios)) {
      paste0("Encontré ", length(precios), " observaciones de precios. Cada valor incluye el lugar, la unidad, la fecha y la fuente disponibles.")
    } else {
      "Todavía no hay observaciones que coincidan. La Pala no estima precios: mostrará datos cuando se registren con lugar, unidad, fecha y fuente."
    }
    return(list(respuesta = respuesta, categoria = categoria_detectada, oportunidades = list(), precios = precios, total = length(precios), filtro_fecha = ""))
  }

  coincidencias <- buscar_oportunidades(consulta, categoria_detectada, limite = limite, pre = pre)
  categoria_respuesta <- coincidencias$categoria
  if (pedido_detalle(consulta)) {
    seleccion <- resolver_oportunidad_contextual(consulta, contexto, coincidencias$oportunidades)
    if (is.null(seleccion)) {
      if (length(coincidencias$oportunidades) > 1L) {
        return(list(respuesta = "Encontré varias publicaciones relacionadas. Elige una con «Ver detalles» o dime «la primera», «la segunda» o el título para consultar su ficha exacta.", categoria = categoria_respuesta, oportunidades = coincidencias$oportunidades, total = coincidencias$total, filtro_fecha = coincidencias$fecha_tope, detalle_solicitado = TRUE))
      }
      return(list(respuesta = "No encuentro una publicación concreta para abrir. Dime el título, la entidad o la ciudad; así puedo buscar la ficha y extraer sus requisitos, plazo o forma de postulación.", categoria = categoria_respuesta, oportunidades = list(), total = 0L, filtro_fecha = coincidencias$fecha_tope, detalle_solicitado = TRUE))
    }
    detalle <- tryCatch(lector_detalle_publicacion(seleccion, consulta), error = function(e) {
      registrar_falla("chat/detalle_publicacion", e)
      list(estado = "no_consultable")
    })
    return(list(respuesta = respuesta_detalle(seleccion, detalle, consulta), categoria = texto_seguro(seleccion$categoria), oportunidades = list(seleccion), total = 1L, filtro_fecha = texto_seguro(seleccion$fecha_limite), detalle = detalle, detalle_solicitado = TRUE))
  }

  alternativa_reciente <- FALSE
  categoria_respuesta <- coincidencias$categoria
  if (coincidencias$total) {
    texto_categoria <- if (nzchar(coincidencias$categoria)) {
      nombre <- vapply(categorias, function(item) {
        if (identical(item$id, coincidencias$categoria)) item$nombre else ""
      }, character(1))
      paste0(" de ", tolower(nombre[nzchar(nombre)][1L]))
    } else ""
    terminos_frag <- tryCatch(terminos_consulta(pre$texto_busqueda), error = function(e) character())
    respuesta <- redactar_respuesta(pre, coincidencias, terminos_frag, relajado = isTRUE(coincidencias$relajado))
  } else if (nzchar(coincidencias$fecha_tope)) {
    respuesta <- paste0("No encontré oportunidades registradas", if (nzchar(pre$departamento)) paste0(" en ", pre$departamento) else "", " con fecha límite hasta el ", format(fecha_segura(coincidencias$fecha_tope), "%d/%m/%Y"), ". Puedes ampliar el plazo, quitar el filtro de ciudad o consultar otra categoría.")
  } else {
    if (!nzchar(coincidencias$categoria) && length(tokenizar(consulta)) <= 2L) {
      respuesta <- "Puedo ayudarte, pero necesito acotar el tema. ¿Buscas empleo, becas, compras públicas, fondos o normativa? También puedes añadir ciudad, entidad o una palabra del título."
    } else {
      tema <- if (nzchar(coincidencias$categoria)) {
        categoria <- Filter(function(x) identical(x$id, coincidencias$categoria), categorias)
        if (length(categoria)) tolower(categoria[[1L]]$nombre) else "esa categoría"
      } else "esa combinación de términos"
      respuesta <- paste0("No encontré publicaciones vigentes que coincidan con ", tema, if (nzchar(pre$departamento)) paste0(" en ", pre$departamento) else "", ". No mostraré ofertas de otro tema como si respondieran a tu pregunta. Prueba con una entidad, ciudad o término del título.")
    }
  }
  list(
    respuesta = respuesta,
    categoria = categoria_respuesta,
    oportunidades = coincidencias$oportunidades,
    total = coincidencias$total,
    filtro_fecha = coincidencias$fecha_tope,
    alternativa_reciente = "",
    departamento = pre$departamento,
    sugerencias = sugerencias_seguimiento(pre, coincidencias)
  )
}

# Panorama del catálogo vigente: conteo por categoría y cierres próximos.
respuesta_resumen <- function() {
  activas <- oportunidades_activas()
  if (!length(activas)) {
    return(list(respuesta = "Todavía no hay publicaciones vigentes verificadas hoy. Cuando termine la próxima revisión de fuentes podré darte un panorama completo.", categoria = "", oportunidades = list(), total = 0L, filtro_fecha = "", conversacional = TRUE, fuentes_recomendadas = fuentes_recomendadas("", 3L)))
  }
  cats <- vapply(activas, function(o) texto_seguro(o$categoria), character(1))
  tabla <- sort(table(cats), decreasing = TRUE)
  nombres <- vapply(names(tabla), function(id) if (id %in% names(NOMBRES_CATEGORIA)) NOMBRES_CATEGORIA[[id]] else id, character(1))
  conteo <- paste0(nombres, " (", as.integer(tabla), ")", collapse = ", ")
  fechas <- vapply(activas, function(o) { f <- fecha_segura(o$fecha_limite); if (is.na(f)) Inf else as.numeric(f) }, numeric(1))
  proximos <- activas[order(fechas)][seq_len(min(6L, sum(is.finite(fechas))))]
  semana <- sum(is.finite(fechas) & fechas <= as.numeric(Sys.Date() + 7))
  texto <- paste0("Hoy hay ", length(activas), " publicaciones vigentes verificadas: ", conteo, ".")
  if (semana > 0L) texto <- paste0(texto, "\n\n", semana, " cierran en los próximos siete días; te muestro las más cercanas.")
  texto <- paste0(texto, "\n\nPuedes pedirme el detalle de una categoría, por ejemplo «becas en La Paz» o «empleo del Estado que cierre esta semana».")
  list(respuesta = texto, categoria = "", oportunidades = proximos, total = length(activas), filtro_fecha = "", alternativa_reciente = "",
       sugerencias = c("¿Cuáles cierran esta semana?", "Becas abiertas", "Empleo público en La Paz"))
}

resumen_analitico <- local({
  memo <- list(marca = "", valor = NULL)
  function() {
    marca <- paste(file.info(ARCHIVO_CATALOGO)$mtime, Sys.Date())
    if (!identical(memo$marca, marca)) memo <<- list(marca = marca, valor = analizar_catalogo(oportunidades_activas()))
    memo$valor
  }
})

DIR_TRABAJOS <- file.path(DIR_LOGS, "trabajos")
lanzar_trabajo <- function(tipo, ...) {
  dir.create(DIR_TRABAJOS, recursive = TRUE, showWarnings = FALSE)
  viejos <- list.files(DIR_TRABAJOS, full.names = TRUE)
  if (length(viejos)) {
    edad <- as.numeric(difftime(Sys.time(), file.info(viejos)$mtime, units = "hours"))
    caducos <- viejos[!is.na(edad) & edad > 24]
    if (length(caducos)) unlink(caducos, recursive = TRUE)
  }
  if (hay_analisis_en_curso() || marca_reciente(ARCHIVO_BLOQUEO_RECOLECCION, 8, "hours") ||
      marca_reciente(ARCHIVO_RECOLECCION_ENCOLADA, 5, "mins")) return(NULL)
  id <- substr(digest::digest(paste(Sys.time(), Sys.getpid(), stats::runif(1)), "sha256"), 1L, 16L)
  # El worker crea el lock de forma atómica. No reservarlo aquí: eso impedía
  # que el propio worker arrancara al encontrar el archivo ya existente.
  ok <- tryCatch({
    lanzar_rscript(file.path(RAIZ, "analisis", "trabajo.R"), c(tipo, id, ...), c(LA_PALA_ROOT = RAIZ))
    TRUE
  }, error = function(e) FALSE)
  if (!ok) return(NULL)
  id
}
estado_trabajo <- function(id = "") {
  if (!grepl("^[a-f0-9]{16}$", id)) return(list(estado = "invalido"))
  f <- file.path(DIR_TRABAJOS, paste0(id, ".json")); if (file.exists(f)) return(leer_json(f, list(estado = "fallido")))
  l <- sub("json$", "lock", f); if (marca_reciente(l, 10, "mins")) list(estado = "en_curso") else list(estado = "desconocido")
}
CABECERAS <- c("Content-Security-Policy" = paste("default-src 'self';", "img-src 'self' https: data:;", "style-src 'self';", "script-src 'self';", "font-src 'self';", "connect-src 'self';", "base-uri 'self';", "frame-ancestors 'self';", "form-action 'self'"), "X-Content-Type-Options" = "nosniff", "Referrer-Policy" = "strict-origin-when-cross-origin")

respuesta_analitica <- function(consulta) {
  base <- list(categoria = "", oportunidades = list(), precios = list(), total = 0L, filtro_fecha = "", vista = "analitica")
  url <- regmatches(consulta, regexpr("https://[^[:space:]]+", consulta))
  if (length(url)) {
    id <- lanzar_trabajo("sitio", url)
    if (is.null(id)) return(c(list(respuesta = "Hay análisis en curso; espera unos segundos e inténtalo de nuevo."), base))
    return(c(list(respuesta = paste0("Estoy analizando ", url, " en segundo plano. El resultado aparecerá en el panel de analítica.")), base, list(trabajo = id)))
  }
  if (!grepl("(^| )(seo|pagerank|page rank)( |$)|fraude|estafa|estadistic|comparativ|grafo|enlaces|distribucion|analitic", normalizar_texto(consulta))) return(NULL)
  r <- resumen_analitico()
  if (isTRUE(r$vacio)) return(c(list(respuesta = "Aún no hay datos vigentes para analizar. Inicia la minería web y vuelve a consultar."), base))
  t <- r$totales
  c(list(respuesta = sprintf("Analicé %d publicaciones de %d sitios: grafo de %d nodos y %d aristas (%d comunidades), riesgo medio de fraude %s/100 y %d publicaciones con riesgo alto. Abrí el panel de analítica con el detalle.", t$oportunidades, t$sitios, t$nodos, t$aristas, t$comunidades, t$riesgo_medio, t$riesgo_alto)), base)
}

RUTAS <- list(
  "/api/health" = function() {
    list(estado = "disponible", servicio = "la-pala", catalogo = file.exists(ARCHIVO_CATALOGO), actualizado_en = catalogo()$actualizado_en %||% NULL)
  },
  "/api/categories" = function() list(categorias = categorias),
  "/api/opportunities" = function(categoria = "", limit = 6L) {
    resultado <- buscar_oportunidades(categoria = categoria, limite = limit)
    list(total = resultado$total, oportunidades = resultado$oportunidades)
  },
  "/api/jobs" = function(limit = 6L) {
    resultado <- buscar_oportunidades(categoria = "empleo_publico", limite = limit)
    list(total = resultado$total, jobs = resultado$oportunidades)
  },
  "/api/search" = function(q = "", categoria = "", limit = 6L) {
    resultado <- buscar_oportunidades(q, categoria, limite = limit)
    list(consulta = q, total = resultado$total, categoria = resultado$categoria,
         filtro_fecha = resultado$fecha_tope, oportunidades = resultado$oportunidades,
         jobs = resultado$oportunidades)
  },
  "/api/chat" = function(q = "", limit = 6L, en_linea = "", contexto = "", seguimiento = "") {
    tryCatch({
      resultado <- respuesta_chat(q, limite = limit, contexto = contexto)
      # Agente IA opcional: redacta la respuesta con los datos ya filtrados (se omite en los refrescos automáticos).
      if (!identical(texto_seguro(seguimiento), "1") && exists("mejorar_con_agente", mode = "function")) {
        resultado <- tryCatch(mejorar_con_agente(resultado, q, normalizar_contexto_chat(contexto)), error = function(e) { registrar_falla("agente/gancho", e); resultado })
      }
      if (isTRUE(resultado$detalle_solicitado)) return(resultado)
      if (!is.null(resultado$vista) || isTRUE(resultado$conversacional)) return(resultado)
      worker <- iniciar_recoleccion_si_necesaria(resultado$categoria %||% "")
      # Consulta en línea: sin coincidencias reales (o por petición expresa), revisa en vivo
      # las fuentes más relacionadas con la pregunta aunque la recolección diaria ya haya terminado.
      sin_coincidencia <- !length(resultado$oportunidades) || nzchar(texto_seguro(resultado$alternativa_reciente))
      pide_en_linea <- identical(texto_seguro(en_linea), "1")
      if (worker$estado %in% c("completada", "fallida", "no_disponible") && (sin_coincidencia || pide_en_linea) &&
          !identical(resultado$categoria %||% "", "precios")) {
        enlinea <- iniciar_consulta_en_linea(q, resultado$categoria %||% "", forzar = pide_en_linea)
        if (enlinea$estado %in% c("en_cola", "en_curso")) {
          worker <- list(estado = enlinea$estado, iniciado = isTRUE(enlinea$iniciado), en_linea = TRUE)
        } else if (identical(enlinea$estado, "reciente")) {
          resultado$consulta_en_linea_reciente <- TRUE
        }
      }
      resultado$exploracion <- list(
        estado = worker$estado,
        mensaje = switch(worker$estado,
          en_curso = if (isTRUE(worker$en_linea)) "Consultando en línea las fuentes más relacionadas con tu pregunta; los resultados aparecerán aquí." else "Estoy revisando fuentes públicas y actualizaré los resultados a medida que aparezcan.",
          en_cola = if (isTRUE(worker$en_linea)) "Preparando la consulta en línea de las fuentes más relacionadas." else "Estoy preparando la búsqueda en fuentes públicas.",
          completada = "La revisión de fuentes de hoy terminó.",
          fallida = "No pude actualizar la búsqueda ahora; puedes volver a intentarlo más tarde.",
          "La búsqueda no está disponible ahora; inténtalo de nuevo más tarde."
        )
      )
      if (!length(resultado$oportunidades) && worker$estado %in% c("en_curso", "en_cola")) {
        resultado$respuesta <- if (isTRUE(worker$en_linea)) "No tengo coincidencias guardadas, así que estoy consultando en línea las fuentes más relacionadas con tu pregunta. Los resultados aparecerán a medida que se lean." else "Todavía no encontré coincidencias en las fuentes revisadas. La búsqueda sigue activa y actualizaré los resultados a medida que aparezcan."
      }
      if (!length(resultado$oportunidades)) {
        resultado$fuentes_recomendadas <- fuentes_recomendadas(resultado$categoria %||% "")
      }
      resultado
    }, error = function(error) {
      registrar_falla("api/chat", error)
      list(respuesta = "No pude completar tu consulta ahora. Inténtalo de nuevo en unos minutos.",
           categoria = "", oportunidades = list(), total = 0L, filtro_fecha = "", fallo = TRUE)
    })
  },
  "/api/prices" = function(q = "", departamento = "", limit = 12L) {
    datos <- buscar_precios(q, departamento, limit)
    list(total = length(datos), precios = datos)
  },
  "/api/stats" = function() {
    abiertas <- oportunidades_activas()
    cierres <- vapply(abiertas, function(item) {
      fecha <- fecha_segura(item$fecha_limite)
      !is.na(fecha) && fecha >= Sys.Date() && fecha <= Sys.Date() + 7
    }, logical(1))
    categorias_con_datos <- unique(vapply(abiertas, function(item) texto_seguro(item$categoria), character(1)))
    list(
      oportunidades_activas = length(abiertas),
      cierres_proximos = sum(cierres),
      categorias_con_datos = length(categorias_con_datos),
      precios_registrados = length(precios_actuales()),
      actualizado_en = catalogo()$actualizado_en %||% NULL,
      fuentes_activas = length(fuentes_configuradas())
    )
  },
  "/api/analisis/resumen" = function() resumen_analitico(),
  "/api/analisis/sitio" = function(urls = "", res) {
    u <- trimws(strsplit(urls, "[,;[:space:]]+")[[1L]]); u <- head(unique(u[grepl("^https://[^[:space:]]+$", u)]), 7L)
    if (!length(u)) { res$status <- 400L; return(list(mensaje = "Indica direcciones HTTPS válidas.")) }
    id <- lanzar_trabajo("sitio", paste(u, collapse = ","))
    if (is.null(id)) { res$status <- 429L; return(list(mensaje = "Hay análisis en curso; espera unos segundos.")) }
    res$status <- 202L; list(trabajo = id)
  },
  "/api/analisis/rastreo" = function(url = "", max = 8L, res) {
    if (!grepl("^https://[^[:space:]]+$", url)) { res$status <- 400L; return(list(mensaje = "Indica una dirección HTTPS válida.")) }
    id <- lanzar_trabajo("rastreo", url, limitar(max, 8L))
    if (is.null(id)) { res$status <- 429L; return(list(mensaje = "Hay análisis en curso; espera unos segundos.")) }
    res$status <- 202L; list(trabajo = id)
  },
  "/api/admin/collect" = function(req, res) {
    clave_configurada <- Sys.getenv("MAPACHE_ACCESS_PASSWORD", unset = Sys.getenv("LA_PALA_ADMIN_PASSWORD", unset = ""))
    if (!nzchar(clave_configurada)) {
      res$status <- 503L
      return(list(mensaje = "La actualización manual no está configurada en el servidor."))
    }

    clave_recibida <- sub("^Bearer[[:space:]]+", "", req$HTTP_AUTHORIZATION %||% "", ignore.case = TRUE)
    if (!identical(clave_recibida, clave_configurada)) {
      res$status <- 401L
      return(list(mensaje = "La contraseña no es correcta."))
    }

    if (file.exists(ARCHIVO_BLOQUEO_RECOLECCION)) {
      antiguedad <- as.numeric(difftime(Sys.time(), file.info(ARCHIVO_BLOQUEO_RECOLECCION)$mtime, units = "hours"))
      if (is.finite(antiguedad) && antiguedad < 8) {
        res$status <- 409L
        return(list(mensaje = "Ya hay una recolección en curso."))
      }
      unlink(ARCHIVO_BLOQUEO_RECOLECCION, recursive = TRUE)
    }

    if (file.exists(ARCHIVO_RECOLECCION_ENCOLADA)) {
      antiguedad <- as.numeric(difftime(Sys.time(), file.info(ARCHIVO_RECOLECCION_ENCOLADA)$mtime, units = "mins"))
      if (is.finite(antiguedad) && antiguedad < 5) {
        res$status <- 409L
        return(list(mensaje = "La Pala ya está preparando la revisión."))
      }
      unlink(ARCHIVO_RECOLECCION_ENCOLADA, recursive = TRUE)
    }

    if (hay_analisis_en_curso()) {
      res$status <- 409L
      return(list(mensaje = "Hay un análisis en curso; espera a que termine para no agotar la memoria."))
    }

    binario_rscript <- ruta_rscript()
    archivo_recolector <- file.path(RAIZ, "scraping", "run_daily.R")
    if (!nzchar(binario_rscript) || !file.exists(binario_rscript) || !file.exists(archivo_recolector)) {
      detalle <- if (!nzchar(binario_rscript) || !file.exists(binario_rscript)) "No se encontró Rscript en la instalación activa." else "No se encontró scraping/run_daily.R."
      registrar_falla("worker/manual/no_disponible", simpleError(detalle))
      res$status <- 500L
      return(list(mensaje = "No pude iniciar la actualización ahora. Inténtalo de nuevo más tarde."))
    }

    dir.create(dirname(ARCHIVO_ESTADO_RECOLECCION), recursive = TRUE, showWarnings = FALSE)
    dir.create(DIR_LOGS, recursive = TRUE, showWarnings = FALSE)
    if (!isTRUE(dir.create(ARCHIVO_RECOLECCION_ENCOLADA, showWarnings = FALSE))) {
      res$status <- 409L
      return(list(mensaje = "Otra exploración acaba de ponerse en marcha."))
    }
    solicitud_id <- paste0("interfaz-", format(Sys.time(), "%Y%m%d%H%M%S"), "-", Sys.getpid())
    inicio <- tryCatch(lanzar_rscript(archivo_recolector,
      entorno = c(LA_PALA_SOLICITUD_ID = solicitud_id)), error = function(error) error)
    if (inherits(inicio, "error")) {
      unlink(ARCHIVO_RECOLECCION_ENCOLADA, recursive = TRUE)
      registrar_falla("worker/manual/inicio", inicio)
      res$status <- 500L
      return(list(mensaje = "No pude iniciar la actualización ahora. Inténtalo de nuevo más tarde."))
    }

    res$status <- 202L
    list(estado = "iniciada", solicitud_id = solicitud_id, mensaje = "Se inició la revisión de todas las fuentes habilitadas.")
  },
  "/api/collection-status" = function(solicitud_id = "") {
    estado <- leer_json(ARCHIVO_ESTADO_RECOLECCION, list(estado = "sin_registro", mensaje = "Todavía no se ha registrado una recolección."))
    cola_reciente <- marca_reciente(ARCHIVO_RECOLECCION_ENCOLADA, 5, "mins")
    if (cola_reciente && !file.exists(ARCHIVO_BLOQUEO_RECOLECCION)) {
      return(list(estado = "en_cola", solicitud_id = solicitud_id, mensaje = "La Pala está preparando la revisión."))
    }
    bloqueo_activo <- marca_reciente(ARCHIVO_BLOQUEO_RECOLECCION, 8, "hours")
    if (bloqueo_activo) {
      if (identical(texto_seguro(estado$estado), "en_curso") &&
          (!nzchar(solicitud_id) || identical(texto_seguro(estado$solicitud_id), solicitud_id))) return(estado)
      return(list(estado = "en_curso", solicitud_id = texto_seguro(estado$solicitud_id, solicitud_id), mensaje = "Estoy revisando fuentes públicas."))
    }
    if (nzchar(solicitud_id) && !identical(texto_seguro(estado$solicitud_id), solicitud_id)) {
      return(list(estado = "fallida", solicitud_id = solicitud_id, mensaje = "No pude iniciar la búsqueda ahora. Inténtalo de nuevo más tarde."))
    }
    if (identical(texto_seguro(estado$estado), "fallida")) {
      estado$mensaje <- "No pude completar la actualización. Inténtalo de nuevo más tarde."
    }
    estado
  },
  "/api/sources" = function() {
    valor <- leer_fuentes()
    fuentes <- valor$fuentes %||% valor$sources %||% list()
    fuentes <- Filter(function(fuente) is.list(fuente), fuentes)
    informes <- catalogo()$fuentes %||% list()
    estados <- list(
      revisada_con_datos = "Consultada; se encontraron publicaciones",
      revisada_sin_datos = "Consultada; no se encontraron publicaciones interpretables",
      revisada = "Consultada correctamente",
      exportacion_revisada = "Archivo público consultado; se encontraron registros",
      feed_revisado = "Canal de publicaciones consultado",
      mapa_revisado = "Mapa del sitio consultado",
      omitida = "Consulta omitida por las reglas del portal",
      pospuesta_por_barrera = "Consulta en pausa temporal tras detectar una barrera del portal",
      no_disponible = "El portal no respondió con una página disponible",
      redireccion_externa = "El portal dirige a otro dominio; revisión omitida",
      html_no_interpretable = "El formato de la página no se pudo interpretar",
      feed_no_interpretable = "El canal de publicaciones no se pudo interpretar",
      feed_sin_publicaciones = "Canal consultado; sin publicaciones compatibles",
      exportacion_sin_datos = "Archivo consultado; sin registros compatibles",
      mapa_sin_coincidencias = "Mapa del sitio consultado; sin coincidencias",
      error = "La revisión tuvo un error; se conserva la fuente como referencia"
    )
    list(fuentes = lapply(fuentes, function(fuente) {
      id <- texto_seguro(fuente$id)
      informe <- Filter(function(item) identical(texto_seguro(item$id), id), informes)
      if (length(informe)) informe <- informe[length(informe)]  # el más reciente, igual que estado_bloqueos
      habilitada <- isTRUE(fuente$habilitada %||% fuente$enabled %||% FALSE)
      estado <- if (!habilitada) {
        "Referencia guardada; revisión automática desactivada"
      } else if (!length(informe)) {
        "Pendiente de la primera revisión automática"
      } else {
        estado_tecnico <- texto_seguro(informe[[1L]]$estado)
        motivo <- texto_seguro(informe[[1L]]$motivo)
        if (motivo == "requiere_javascript") {
          "El portal requiere JavaScript; se conserva como referencia"
        } else if (motivo == "requiere_autenticacion") {
          "El portal requiere autenticación; se conserva como referencia"
        } else if (motivo == "bloqueada_por_el_portal") {
          "El portal no admite esta consulta automática; se conserva como referencia"
        } else if (grepl("robots\\.txt bloqueado", motivo)) {
          "El portal no autoriza la consulta según sus reglas públicas"
        } else if (grepl("robots\\.txt no_verificable", motivo)) {
          "No se pudieron verificar las reglas públicas del portal"
        } else {
          texto_seguro(estados[[estado_tecnico]], "Consultada; estado disponible en el registro local")
        }
      }
      list(id = id, nombre = texto_seguro(fuente$nombre %||% fuente$name),
           url = texto_seguro(fuente$url), categoria = texto_seguro(fuente$categoria %||% fuente$category),
           estado = estado)
    }))
  },
  "/api/mapache/estado" = function() estado_bloqueos(),
  "/api/analisis/trabajo" = function(id = "") estado_trabajo(id)
)
RUTAS_POST <- c("/api/admin/collect")

cargar_modulo("chat_nlp.R", obligatorio = TRUE)
cargar_modulo("agente_ia.R")
