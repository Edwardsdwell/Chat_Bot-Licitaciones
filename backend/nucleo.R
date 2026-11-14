suppressPackageStartupMessages({
    library(jsonlite)
})

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
ARCHIVO_RANKING <- file.path(RAIZ, "search", "ranking.R")

marca_reciente <- function(ruta, limite, unidad = "mins") {
  if (!file.exists(ruta)) return(FALSE)
  marca <- tryCatch(file.info(ruta)$mtime, error = function(e) NA)
  if (length(marca) != 1L || is.na(marca)) return(FALSE)
  edad <- suppressWarnings(as.numeric(difftime(Sys.time(), marca, units = unidad)))
  is.finite(edad) && edad >= 0 && edad < limite
}
if (file.exists(ARCHIVO_RANKING)) source(ARCHIVO_RANKING, local = .GlobalEnv)
source(file.path(RAIZ, "analisis", "motor.R"), local = .GlobalEnv)

registrar_falla <- function(origen, error) {
  dir.create(dirname(ARCHIVO_FALLAS), recursive = TRUE, showWarnings = FALSE)
  detalle <- gsub("[\\r\\n\\t]+", " ", conditionMessage(error))
  linea <- sprintf("%s [ERROR] %s: %s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"), origen, detalle)
  try(cat(linea, file = ARCHIVO_FALLAS, append = TRUE), silent = TRUE)
  invisible(NULL)
}

leer_json <- function(ruta, alternativo = list()) {
  if (!file.exists(ruta) || isTRUE(file.info(ruta)$isdir) || file.info(ruta)$size == 0) return(alternativo)
  tryCatch(jsonlite::fromJSON(ruta, simplifyVector = FALSE), error = function(e) alternativo)
}

catalogo <- local({
  marca <- NULL; valor <- list()
  function() {
    i <- file.info(ARCHIVO_CATALOGO); k <- paste(i$mtime, i$size)
    if (!identical(marca, k)) { v <- leer_json(ARCHIVO_CATALOGO, list()); valor <<- if (is.list(v)) v else list(); marca <<- k }
    valor
  }
})

estado_bloqueos <- function() {
  configuracion <- leer_json(ARCHIVO_FUENTES, list())
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
  valor <- leer_json(ARCHIVO_FUENTES, list(fuentes = list()))
  fuentes <- valor$fuentes %||% valor$sources %||% list()
  if (!is.list(fuentes)) return(list())
  Filter(function(fuente) is.list(fuente) && isTRUE(fuente$habilitada %||% fuente$enabled %||% FALSE), fuentes)
}

fuentes_recomendadas <- function(categoria = "", limite = 3L) {
  registro <- leer_json(ARCHIVO_FUENTES, list(fuentes = list()))
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
  if (file.exists(ARCHIVO_BLOQUEO_RECOLECCION)) unlink(ARCHIVO_BLOQUEO_RECOLECCION, recursive = TRUE)
  if (file.exists(ARCHIVO_RECOLECCION_ENCOLADA)) unlink(ARCHIVO_RECOLECCION_ENCOLADA, recursive = TRUE)

  registro <- leer_json(ARCHIVO_ESTADO_RECOLECCION, list())
  fecha_registro <- suppressWarnings(as.Date(substr(texto_seguro(registro$actualizado_en), 1L, 10L)))
  if (identical(texto_seguro(registro$estado), "completada") && !identical(texto_seguro(registro$modo), "consulta") && !is.na(fecha_registro) && fecha_registro == Sys.Date()) {
    return(list(estado = "completada", iniciado = FALSE))
  }
  if (identical(texto_seguro(registro$estado), "fallida") && !is.na(fecha_registro) && fecha_registro == Sys.Date() &&
      as.numeric(difftime(Sys.time(), file.info(ARCHIVO_ESTADO_RECOLECCION)$mtime, units = "mins")) < 30) {
    return(list(estado = "fallida", iniciado = FALSE))
  }

  binario <- Sys.which("Rscript")
  if (!nzchar(binario)) {
    candidato <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")
    if (file.exists(candidato)) binario <- candidato
  }
  archivo <- file.path(RAIZ, "scraping", "run_daily.R")
  if (!nzchar(binario) || !file.exists(archivo) || !isTRUE(dir.create(ARCHIVO_RECOLECCION_ENCOLADA, showWarnings = FALSE))) {
    detalle <- if (!nzchar(binario)) "No se encontró Rscript en la instalación activa." else if (!file.exists(archivo)) "No se encontró scraping/run_daily.R." else "No se pudo crear la marca de cola del worker."
    registrar_falla("worker/no_disponible", simpleError(detalle))
    return(list(estado = "no_disponible", iniciado = FALSE))
  }

  solicitud_id <- paste0("consulta-", format(Sys.time(), "%Y%m%d%H%M%S"), "-", Sys.getpid())
  inicio <- tryCatch(system2(
    binario,
    args = shQuote(archivo),
    stdout = FALSE,
    stderr = FALSE,
    wait = FALSE,
    env = c(paste0("LA_PALA_SOLICITUD_ID=", solicitud_id), paste0("LA_PALA_CATEGORIA_PRIORITARIA=", categoria))
  ), error = function(error) error)
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
  inicio <- tryCatch(system2(
    binario, args = shQuote(archivo), stdout = FALSE, stderr = FALSE, wait = FALSE,
    env = c(paste0("LA_PALA_SOLICITUD_ID=", solicitud_id), "LA_PALA_MODO=consulta",
            paste0("LA_PALA_CONSULTA=", gsub("[\r\n\"']", " ", consulta)),
            paste0("LA_PALA_CATEGORIA_PRIORITARIA=", categoria))
  ), error = function(e) e)
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

fecha_segura <- function(valor) {
  texto <- texto_seguro(valor)
  if (!nzchar(texto)) return(as.Date(NA))
  fecha <- suppressWarnings(as.Date(substr(texto, 1L, 10L)))
  if (length(fecha) == 1L && !is.na(fecha)) fecha else as.Date(NA)
}

oportunidad_abierta <- function(oportunidad) {
  if (!is.list(oportunidad)) return(FALSE)
  categoria <- texto_seguro(oportunidad$categoria)
  titulo <- texto_seguro(oportunidad$titulo)
  url <- texto_seguro(oportunidad$url)
  estado <- texto_seguro(oportunidad$estado, "abierta")
  if (!nzchar(categoria) || !nzchar(titulo) || !grepl("^https://", url, ignore.case = TRUE)) return(FALSE)
  if (estado %in% c("cerrada", "vencida", "eliminada", "duplicada", "rechazada")) return(FALSE)
  verificacion <- suppressWarnings(as.Date(substr(texto_seguro(oportunidad$verificado_en %||% oportunidad$extraido_en), 1L, 10L)))
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
    precios = "precio|alquiler|farmacia|canasta|repuesto|cuesta|costo",
    becas = "beca|pasantia|estudiante|universidad|academica|movilidad|practica|posgrado|maestria|doctorado|intercambio",
    empleo_publico = "empleo(s)?|trabajo(s)?|chamba(s)?|vacante(s)?|puesto(s)?|laboral(es)?|personal|contratacion de personal|cargo publico|servidor publico|recursos humanos",
    contrataciones = "sicoes|licitacion(es)?|contratacion(es)?|contrato(s)?|compras? publicas?|proveer|proveedor",
    merito = "concurso de merito|merito|evaluacion|seleccion academica",
    fondos = "fondo|financiamiento|financiar|emprend|ong|organizacion|capital semilla",
    normativa = "gaceta|normativa|ley|decreto|resolucion|reglamento",
    convocatorias = "convocatoria|llamado|concurso"
  )
  for (id in names(reglas)) if (grepl(reglas[[id]], texto)) return(id)
  ""
}

buscar_oportunidades <- function(consulta = "", categoria = "", hasta = NULL, limite = 6L) {
  resultados <- oportunidades_activas()
  elegida <- detectar_categoria(consulta, categoria)
  if (nzchar(elegida)) resultados <- Filter(function(x) identical(texto_seguro(x$categoria), elegida), resultados)

  fecha_tope <- if (!is.null(hasta) && nzchar(texto_seguro(hasta))) fecha_segura(hasta) else as.Date(NA)
  texto <- normalizar_texto(consulta)
  if (is.na(fecha_tope) && grepl("fin de mes|final de mes|fin del mes|final del mes|fin mes|final mes|este mes|antes de terminar el mes", texto)) {
    fecha_tope <- fecha_fin_mes()
  }
  if (!is.na(fecha_tope)) {
    resultados <- Filter(function(x) {
      limite_oferta <- fecha_segura(x$fecha_limite)
      !is.na(limite_oferta) && limite_oferta <= fecha_tope
    }, resultados)
  }

  puntajes <- numeric(length(resultados))
  terminos <- terminos_consulta(texto)
  if (length(terminos) && length(resultados)) {
    puntajes <- puntuar_bm25(resultados, terminos)
    # Si la persona ya indicó una categoría, conserva sus publicaciones aunque
    # use palabras distintas a las del título; el puntaje solo las ordena.
    if (any(puntajes > 0)) {
      resultados <- resultados[puntajes > 0]
      puntajes <- puntajes[puntajes > 0]
    } else if (!nzchar(elegida)) {
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
    fecha_tope = if (is.na(fecha_tope)) "" else as.character(fecha_tope)
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
      fecha <- suppressWarnings(as.POSIXct(valor, tz = "America/La_Paz", tryFormats = c(
        "%Y-%m-%dT%H:%M:%OS%z", "%Y-%m-%d %H:%M:%S", "%Y-%m-%d"
      )))
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

respuesta_chat <- function(consulta, categoria = "", limite = 6L) {
  texto_estado <- normalizar_texto(consulta)
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
  if (identical(categoria_detectada, "precios")) {
    precios <- buscar_precios(consulta, limite = 12L)
    respuesta <- if (length(precios)) {
      paste0("Encontré ", length(precios), " observaciones de precios. Cada valor incluye el lugar, la unidad, la fecha y la fuente disponibles.")
    } else {
      "Todavía no hay observaciones que coincidan. La Pala no estima precios: mostrará datos cuando se registren con lugar, unidad, fecha y fuente."
    }
    return(list(respuesta = respuesta, categoria = categoria_detectada, oportunidades = list(), precios = precios, total = length(precios), filtro_fecha = ""))
  }

  coincidencias <- buscar_oportunidades(consulta, categoria_detectada, limite = limite)
  alternativa_reciente <- FALSE
  categoria_respuesta <- coincidencias$categoria
  if (coincidencias$total) {
    texto_categoria <- if (nzchar(coincidencias$categoria)) {
      nombre <- vapply(categorias, function(item) {
        if (identical(item$id, coincidencias$categoria)) item$nombre else ""
      }, character(1))
      paste0(" de ", tolower(nombre[nzchar(nombre)][1L]))
    } else ""
    fecha <- if (nzchar(coincidencias$fecha_tope)) paste0(" con fecha límite hasta el ", format(as.Date(coincidencias$fecha_tope), "%d/%m/%Y")) else ""
    respuesta <- paste0("Encontré ", coincidencias$total, " oportunidades", texto_categoria, fecha, ". Revisa requisitos y vigencia en la publicación original.")
  } else if (nzchar(coincidencias$fecha_tope)) {
    respuesta <- paste0("No encontré oportunidades registradas con fecha límite hasta el ", format(as.Date(coincidencias$fecha_tope), "%d/%m/%Y"), ". Puedes ampliar el plazo o consultar otra categoría.")
  } else {
    recientes <- if (nzchar(coincidencias$categoria)) {
      oportunidades_recientes(limite, coincidencias$categoria)
    } else {
      oportunidades_recientes(limite, "empleo_publico")
    }
    tipo_alternativa <- if (length(recientes) && !nzchar(coincidencias$categoria)) "empleos" else "publicaciones"
    if (!length(recientes) && !nzchar(coincidencias$categoria)) recientes <- oportunidades_recientes(limite)
    if (length(recientes)) {
      coincidencias$oportunidades <- recientes
      coincidencias$total <- length(recientes)
      alternativa_reciente <- TRUE
      if (identical(tipo_alternativa, "empleos")) {
        categoria_respuesta <- "empleo_publico"
        respuesta <- "Aquí tienes los empleos públicos más recientes."
      } else if (nzchar(coincidencias$categoria)) {
        nombre <- vapply(categorias, function(item) if (identical(item$id, coincidencias$categoria)) item$nombre else "", character(1))
        respuesta <- paste0("Aquí tienes las publicaciones más recientes de ", tolower(nombre[nzchar(nombre)][1L]), ".")
      } else {
        respuesta <- "Aquí tienes las publicaciones más recientes disponibles."
      }
      coincidencias$alternativa <- tipo_alternativa
    } else {
      respuesta <- "Aún no hay publicaciones vigentes registradas para esa consulta. Cuando las fuentes aporten resultados aparecerán aquí; La Pala no completa datos supuestos."
    }
  }
  list(
    respuesta = respuesta,
    categoria = categoria_respuesta,
    oportunidades = coincidencias$oportunidades,
    total = coincidencias$total,
    filtro_fecha = coincidencias$fecha_tope,
    alternativa_reciente = if (isTRUE(alternativa_reciente)) coincidencias$alternativa else ""
  )
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
ruta_rscript <- function() { b <- Sys.which("Rscript"); if (nzchar(b)) b else file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript") }
lanzar_trabajo <- function(tipo, ...) {
  dir.create(DIR_TRABAJOS, recursive = TRUE, showWarnings = FALSE)
  viejos <- list.files(DIR_TRABAJOS, full.names = TRUE)
  if (length(viejos)) {
    edad <- as.numeric(difftime(Sys.time(), file.info(viejos)$mtime, units = "hours"))
    caducos <- viejos[!is.na(edad) & edad > 24]
    if (length(caducos)) unlink(caducos, recursive = TRUE)
  }
  bloqueos <- list.files(DIR_TRABAJOS, pattern = "\\.lock$", full.names = TRUE)
  if (length(bloqueos)) {
    edad_bloqueos <- as.numeric(difftime(Sys.time(), file.info(bloqueos)$mtime, units = "mins"))
    if (sum(!is.na(edad_bloqueos) & edad_bloqueos < 10) >= 2L) return(NULL)
  }
  id <- substr(digest::digest(paste(Sys.time(), Sys.getpid(), stats::runif(1)), "sha256"), 1L, 16L)
  # El worker crea el lock de forma atómica. No reservarlo aquí: eso impedía
  # que el propio worker arrancara al encontrar el archivo ya existente.
  argumentos <- c(file.path(RAIZ, "analisis", "trabajo.R"), tipo, id, ...)
  ok <- tryCatch({
    system2(ruta_rscript(), shQuote(argumentos), stdout = FALSE, stderr = FALSE,
            wait = FALSE, env = paste0("LA_PALA_ROOT=", RAIZ))
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
  if (!grepl("seo|pagerank|page rank|fraude|estafa|estadistic|comparativ|grafo|enlaces|distribucion|analitic", normalizar_texto(consulta))) return(NULL)
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
  "/api/chat" = function(q = "", limit = 6L, en_linea = "") {
    tryCatch({
      resultado <- respuesta_chat(q, limite = limit)
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
      unlink(ARCHIVO_BLOQUEO_RECOLECCION)
    }
  
    if (file.exists(ARCHIVO_RECOLECCION_ENCOLADA)) {
      antiguedad <- as.numeric(difftime(Sys.time(), file.info(ARCHIVO_RECOLECCION_ENCOLADA)$mtime, units = "mins"))
      if (is.finite(antiguedad) && antiguedad < 5) {
        res$status <- 409L
        return(list(mensaje = "El Chamebador ya está preparando una exploración."))
      }
      unlink(ARCHIVO_RECOLECCION_ENCOLADA, recursive = TRUE)
    }
  
    binario_rscript <- Sys.which("Rscript")
    if (!nzchar(binario_rscript)) {
      candidato <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")
      if (file.exists(candidato)) binario_rscript <- candidato
    }
    archivo_recolector <- file.path(RAIZ, "scraping", "run_daily.R")
    if (!nzchar(binario_rscript) || !file.exists(archivo_recolector)) {
      detalle <- if (!nzchar(binario_rscript)) "No se encontró Rscript en la instalación activa." else "No se encontró scraping/run_daily.R."
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
    inicio <- tryCatch(system2(
      binario_rscript,
      args = shQuote(archivo_recolector),
      stdout = FALSE,
      stderr = FALSE,
      wait = FALSE,
      env = paste0("LA_PALA_SOLICITUD_ID=", solicitud_id)
    ), error = function(error) error)
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
      return(list(estado = "en_cola", solicitud_id = solicitud_id, mensaje = "El Chamebador está preparando la exploración."))
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
    valor <- leer_json(ARCHIVO_FUENTES, list(fuentes = list()))
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
