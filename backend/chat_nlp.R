# chat_nlp.R — preprocesamiento de la consulta, extracción de datos del texto
# publicado y redacción de respuestas elaboradas. Sin dependencias extra.
# Se carga desde nucleo.R; usa normalizar_texto() y texto_seguro() de ahí/motor.R.

if (!exists("normalizar_texto", mode = "function")) {
  normalizar_texto <- function(x) {
    x <- tolower(as.character(x))
    x <- chartr("áéíóúüñ", "aeiouun", x)
    trimws(gsub("[[:space:]]+", " ", gsub("[^a-z0-9@./:,\\- ]", " ", x)))
  }
}

# ---------------------------------------------------------------------------
# 1) Preprocesamiento de la consulta
# ---------------------------------------------------------------------------

DEPARTAMENTOS_BO <- list(
  "La Paz"     = "la paz|el alto|lapaz",
  "Cochabamba" = "cochabamba|quillacollo|sacaba",
  "Santa Cruz" = "santa cruz|santacruz",
  "Oruro"      = "oruro",
  "Potosí"     = "potosi",
  "Chuquisaca" = "chuquisaca|sucre",
  "Tarija"     = "tarija",
  "Beni"       = "beni|trinidad",
  "Pando"      = "pando|cobija"
)

MESES_BO <- c(enero = 1, febrero = 2, marzo = 3, abril = 4, mayo = 5, junio = 6, julio = 7,
              agosto = 8, septiembre = 9, setiembre = 9, octubre = 10, noviembre = 11, diciembre = 12)

NOMBRES_MESES <- c("enero", "febrero", "marzo", "abril", "mayo", "junio", "julio", "agosto",
                   "septiembre", "octubre", "noviembre", "diciembre")

# Devuelve el departamento mencionado en un texto ya normalizado (o NULL).
detectar_departamento <- function(texto_norm) {
  for (nombre in names(DEPARTAMENTOS_BO)) {
    patron <- paste0("(^| )(", DEPARTAMENTOS_BO[[nombre]], ")( |$)")
    m <- regexpr(patron, texto_norm)
    if (m > 0) {
      frase <- trimws(regmatches(texto_norm, m))
      return(list(nombre = nombre, frase = frase))
    }
  }
  NULL
}

fecha_desde_dia_mes <- function(dia, mes, anio = NA_integer_) {
  hoy <- Sys.Date()
  if (is.na(anio)) {
    anio <- as.integer(format(hoy, "%Y"))
    f <- suppressWarnings(as.Date(sprintf("%04d-%02d-%02d", anio, mes, dia), format = "%Y-%m-%d"))
    if (!is.na(f) && f < hoy) f <- suppressWarnings(as.Date(sprintf("%04d-%02d-%02d", anio + 1L, mes, dia), format = "%Y-%m-%d"))
    return(f)
  }
  suppressWarnings(as.Date(sprintf("%04d-%02d-%02d", anio, mes, dia), format = "%Y-%m-%d"))
}

# Fecha tope expresada en lenguaje natural. Devuelve list(fecha, frase) o NULL.
detectar_fecha_tope <- function(texto_norm) {
  hoy <- Sys.Date()
  reglas <- list(
    list("(^| )(para )?hoy( |$)", function(m) hoy),
    list("(^| )(para )?manana( |$)", function(m) hoy + 1),
    list("(^| )(esta semana|fin de semana|final de semana|antes del domingo)( |$)", function(m) hoy + (7 - as.integer(format(hoy, "%u")))),
    list("(^| )(la )?proxima semana( |$)", function(m) hoy + 7),
    list("(^| )(las )?proximas dos semanas( |$)", function(m) hoy + 14),
    list("(^| )(en |dentro de |proximos |proximas |siguientes |los proximos |las proximas )([0-9]{1,3}) dias( |$)", function(m) hoy + as.integer(sub(".* ([0-9]{1,3}) dias.*", "\\1", m))),
    list("(^| )(urgente|urgentes|cierran pronto|ultimos dias|por vencer)( |$)", function(m) hoy + 7)
  )
  for (r in reglas) {
    m <- regexpr(r[[1]], texto_norm)
    if (m > 0) {
      frase <- trimws(regmatches(texto_norm, m))
      return(list(fecha = r[[2]](frase), frase = frase))
    }
  }
  meses <- paste(names(MESES_BO), collapse = "|")
  m <- regexpr(paste0("(antes del|antes de|hasta el|hasta|para el|al) ([0-9]{1,2}) de (", meses, ")( de ([0-9]{4}))?"), texto_norm)
  if (m > 0) {
    frase <- regmatches(texto_norm, m)
    dia <- as.integer(sub(".* ([0-9]{1,2}) de .*", "\\1", frase))
    mes <- MESES_BO[[sub(paste0(".* de (", meses, ").*"), "\\1", frase)]]
    anio <- suppressWarnings(as.integer(sub(".* de ([0-9]{4})$", "\\1", frase)))
    if (grepl(" de [0-9]{4}$", frase)) anio <- anio else anio <- NA_integer_
    f <- fecha_desde_dia_mes(dia, mes, anio)
    if (!is.na(f)) return(list(fecha = f, frase = trimws(frase)))
  }
  m <- regexpr("(antes del|hasta el|hasta) ([0-9]{1,2})/([0-9]{1,2})(/([0-9]{4}))?", texto_norm)
  if (m > 0) {
    frase <- regmatches(texto_norm, m)
    partes <- regmatches(frase, gregexpr("[0-9]+", frase))[[1]]
    anio <- if (length(partes) >= 3) as.integer(partes[[3]]) else NA_integer_
    f <- fecha_desde_dia_mes(as.integer(partes[[1]]), as.integer(partes[[2]]), anio)
    if (!is.na(f)) return(list(fecha = f, frase = trimws(frase)))
  }
  NULL
}

detectar_intencion <- function(texto_norm) {
  if (grepl("compar|diferencia entre|cual conviene|cual es mejor", texto_norm)) return("comparar")
  if (grepl("resumen|panorama|cuantas|cuantos|estadistic|como esta el catalogo", texto_norm)) return("resumen")
  if (grepl("urgente|cierran pronto|ultimos dias|por vencer", texto_norm)) return("urgentes")
  "buscar"
}

# Todo el preprocesamiento en una sola llamada: devuelve filtros y texto limpio.
preprocesar_consulta <- function(consulta) {
  norm <- normalizar_texto(consulta)
  depto <- detectar_departamento(norm)
  fecha <- detectar_fecha_tope(norm)
  limpio <- norm
  quitar <- c(if (!is.null(depto)) depto$frase, if (!is.null(fecha)) fecha$frase)
  for (frase in quitar) limpio <- sub(frase, " ", limpio, fixed = TRUE)
  limpio <- gsub("(^| )(en|de|del|para|y|ahora|solo|solamente|tambien|pero|mas|hay|algo|que|me|las|los|la|el|un|una)( |$)", " ", limpio)
  limpio <- gsub("(^| )(en|de|del|para|y|ahora|solo|solamente|tambien|pero|mas|hay|algo|que|me|las|los|la|el|un|una)( |$)", " ", limpio)
  limpio <- trimws(gsub("[[:space:]]+", " ", limpio))
  list(
    original = consulta,
    normalizada = norm,
    texto_busqueda = limpio,
    departamento = if (is.null(depto)) "" else depto$nombre,
    fecha_tope = if (is.null(fecha)) as.Date(NA) else fecha$fecha,
    frase_fecha = if (is.null(fecha)) "" else fecha$frase,
    intencion = detectar_intencion(norm),
    es_seguimiento = grepl("^(y|ahora|solo|solamente|tambien|pero|mas|otra|otras|otros|ademas)( |$)", norm) ||
      (length(strsplit(limpio, " ")[[1]][nzchar(strsplit(limpio, " ")[[1]])]) <= 1 && (!is.null(depto) || !is.null(fecha)))
  )
}

GRUPOS_SINONIMOS <- list(
  c("beca", "becas", "pasantia", "pasantias", "maestria", "posgrado", "doctorado", "intercambio", "estudios"),
  c("empleo", "empleos", "trabajo", "cargo", "cargos", "vacante", "vacantes", "puesto", "puestos", "personal", "convocatoria"),
  c("licitacion", "licitaciones", "contratacion", "contrataciones", "sicoes", "contrato", "adquisicion", "proveedor", "compra"),
  c("fondo", "fondos", "financiamiento", "subvencion", "capital", "proyecto", "proyectos"),
  c("norma", "normas", "ley", "leyes", "decreto", "decretos", "resolucion", "reglamento"),
  c("concurso", "concursos", "merito", "seleccion", "convocatoria", "convocatorias")
)

expandir_terminos <- function(terminos) {
  if (!length(terminos)) return(terminos)
  extra <- character()
  for (g in GRUPOS_SINONIMOS) if (any(terminos %in% g)) extra <- c(extra, g)
  unique(c(terminos, extra))
}

# ---------------------------------------------------------------------------
# 2) Extracción de datos desde el texto publicado
# ---------------------------------------------------------------------------

primera_coincidencia <- function(texto, patron, grupo = 0L, perl = TRUE) {
  m <- regexec(patron, texto, perl = perl, ignore.case = TRUE)
  r <- regmatches(texto, m)[[1]]
  if (length(r) > grupo) trimws(r[[grupo + 1L]]) else ""
}

limpiar_espacios <- function(x) trimws(gsub("[[:space:]]+", " ", x))

# Fecha límite escrita en el texto, solo si va precedida de una palabra clave de plazo.
inferir_fecha_limite <- function(texto) {
  t <- tolower(texto)
  clave <- "(fecha l.{1,2}mite|plazo|hasta el|hasta|cierre|vence|vencimiento|recepci.{1,2}n de (?:propuestas|postulaciones|ofertas|documentos)|presentaci.{1,2}n de (?:propuestas|ofertas|postulaciones|documentos))"
  meses <- paste(names(MESES_BO), collapse = "|")
  m <- regexec(paste0(clave, "[^0-9]{0,40}([0-9]{1,2}) de (", meses, ")(?: de ([0-9]{4}))?"), t, perl = TRUE)
  r <- regmatches(t, m)[[1]]
  if (length(r) >= 4) {
    anio <- if (length(r) >= 5 && nzchar(r[[5]])) as.integer(r[[5]]) else NA_integer_
    f <- fecha_desde_dia_mes(as.integer(r[[3]]), MESES_BO[[r[[4]]]], anio)
    if (!is.na(f)) return(f)
  }
  m <- regexec(paste0(clave, "[^0-9]{0,40}([0-9]{1,2})[/-]([0-9]{1,2})[/-]([0-9]{4})"), t, perl = TRUE)
  r <- regmatches(t, m)[[1]]
  if (length(r) >= 5) {
    f <- fecha_desde_dia_mes(as.integer(r[[3]]), as.integer(r[[4]]), as.integer(r[[5]]))
    if (!is.na(f)) return(f)
  }
  as.Date(NA)
}

extraer_datos_clave <- function(o) {
  texto <- limpiar_espacios(paste(texto_seguro(o$titulo), texto_seguro(o$descripcion), texto_seguro(o$requisitos)))
  if (!nzchar(texto)) return(list())
  datos <- list()
  salario <- primera_coincidencia(texto, "(?:haber(?:es)? mensual(?:es)?|salario|sueldo|remuneraci.{1,2}n|haber)[^0-9]{0,40}?((?:bs\\.?|bolivianos)?\\s*[0-9][0-9.,]{2,9})", 1L)
  if (!nzchar(salario)) salario <- primera_coincidencia(texto, "\\b(bs\\.?\\s?[0-9][0-9.,]{2,9})", 1L)
  if (nzchar(salario)) {
    salario <- limpiar_espacios(sub("[.,]+$", "", salario))
    if (!grepl("^(bs|bolivianos)", salario, ignore.case = TRUE)) salario <- paste("Bs", salario)
    datos$salario <- salario
  }
  vacantes <- primera_coincidencia(texto, "\\b([0-9]{1,3})\\s+(?:vacantes?|plazas?|cargos?|.{1,2}tems?|puestos?)\\b", 1L)
  if (nzchar(vacantes)) datos$vacantes <- vacantes
  correo <- primera_coincidencia(texto, "([[:alnum:]._%+-]+@[[:alnum:].-]+\\.[[:alpha:]]{2,})", 1L)
  if (nzchar(correo)) datos$correo <- correo
  tel <- primera_coincidencia(texto, "(?:tel.{1,2}fono|celular|cel\\.?|whatsapp|fono)[^0-9+]{0,15}(\\+?[0-9][0-9 -]{6,14})", 1L)
  if (nzchar(tel)) datos$telefono <- limpiar_espacios(tel)
  niveles <- unique(tolower(regmatches(texto, gregexpr("licenciatura|t.{1,2}cnico superior|t.{1,2}cnico medio|bachiller|maestr.{1,2}a|doctorado|egresad[oa]|profesional", texto, ignore.case = TRUE, perl = TRUE))[[1]]))
  if (length(niveles)) datos$nivel <- paste(head(niveles, 3), collapse = ", ")
  modalidad <- unique(tolower(regmatches(texto, gregexpr("presencial|remoto|virtual|h.{1,2}brido|tiempo completo|medio tiempo|jornada completa", texto, ignore.case = TRUE, perl = TRUE))[[1]]))
  if (length(modalidad)) datos$modalidad <- paste(head(modalidad, 2), collapse = ", ")
  req <- texto_seguro(o$requisitos)
  if (nzchar(req)) {
    items <- trimws(unlist(strsplit(req, "(?<=[.;])\\s+|\\n+|\\s-\\s", perl = TRUE)))
    items <- items[nchar(items) >= 12]
    if (length(items)) datos$requisitos <- vapply(head(items, 3), function(i) if (nchar(i) > 140) paste0(substr(i, 1L, 137L), "…") else i, character(1), USE.NAMES = FALSE)
  }
  datos
}

# Completa el registro con lo que se pueda deducir del texto (departamento, plazo, datos clave).
enriquecer_oportunidad <- function(o) {
  if (!is.list(o)) return(o)
  base <- paste(texto_seguro(o$titulo), texto_seguro(o$descripcion), texto_seguro(o$entidad), texto_seguro(o$ciudad))
  if (!nzchar(texto_seguro(o$departamento)) || tolower(texto_seguro(o$departamento)) %in% c("bolivia", "nacional")) {
    d <- detectar_departamento(normalizar_texto(substr(base, 1L, 1500L)))
    if (!is.null(d)) { o$departamento <- d$nombre; o$departamento_inferido <- TRUE }
  }
  if (is.na(fecha_segura(o$fecha_limite))) {
    f <- inferir_fecha_limite(substr(paste(texto_seguro(o$titulo), texto_seguro(o$descripcion), texto_seguro(o$requisitos)), 1L, 3000L))
    if (!is.na(f)) { o$fecha_limite <- as.character(f); o$fecha_limite_inferida <- TRUE }
  }
  dc <- extraer_datos_clave(o)
  if (length(dc)) o$datos_clave <- dc
  o
}

coincide_departamento <- function(o, nombre) {
  dep <- texto_seguro(o$departamento)
  if (nzchar(dep)) return(identical(normalizar_texto(dep), normalizar_texto(nombre)))
  FALSE
}

sin_dato_departamento <- function(o) {
  dep <- tolower(texto_seguro(o$departamento))
  !nzchar(dep) || dep %in% c("bolivia", "nacional")
}

# Frase de la descripción que mejor responde a los términos de la consulta.
fragmento_relevante <- function(texto, terminos, max_caracteres = 220L) {
  texto <- limpiar_espacios(texto)
  if (!nzchar(texto) || !length(terminos)) return("")
  frases <- unlist(strsplit(texto, "(?<=[.!?;])\\s+", perl = TRUE))
  frases <- frases[nchar(frases) >= 25]
  if (!length(frases)) return("")
  puntos <- vapply(frases, function(f) { fn <- normalizar_texto(f); sum(vapply(terminos, function(t) grepl(t, fn, fixed = TRUE), logical(1))) }, numeric(1))
  if (max(puntos) <= 0) return("")
  mejor <- frases[[which.max(puntos)]]
  if (nchar(mejor) > max_caracteres) paste0(substr(mejor, 1L, max_caracteres - 1L), "…") else mejor
}

# ---------------------------------------------------------------------------
# 3) Redacción de respuestas elaboradas
# ---------------------------------------------------------------------------

fecha_corta <- function(f) format(f, "%d/%m/%Y")

texto_dias_restantes <- function(f) {
  if (is.na(f)) return("sin fecha límite confirmada")
  d <- as.integer(f - Sys.Date())
  if (d < 0) return(paste("venció el", fecha_corta(f)))
  if (d == 0) return("cierra hoy")
  if (d == 1) return("cierra mañana")
  if (d <= 14) return(sprintf("cierra el %s (en %d días)", fecha_corta(f), d))
  sprintf("cierra el %s", fecha_corta(f))
}

describir_datos_clave <- function(o) {
  dc <- o$datos_clave
  if (!is.list(dc) || !length(dc)) return("")
  partes <- c(
    if (!is.null(dc$salario)) paste("remuneración", dc$salario),
    if (!is.null(dc$vacantes)) paste(dc$vacantes, "vacante(s)"),
    if (!is.null(dc$nivel)) paste("nivel:", dc$nivel),
    if (!is.null(dc$modalidad)) paste("modalidad:", dc$modalidad),
    if (!is.null(dc$correo)) paste("contacto:", dc$correo)
  )
  if (length(partes)) paste(partes, collapse = "; ") else ""
}

NOMBRES_CATEGORIA <- c(
  convocatorias = "convocatorias públicas", empleo_publico = "empleo del Estado", contrataciones = "compras y contrataciones",
  merito = "concursos de mérito", becas = "becas y pasantías", fondos = "fondos y financiamiento",
  precios = "precios locales", normativa = "normativa y Gaceta"
)

redactar_respuesta <- function(pre, coincidencias, terminos = character(), relajado = FALSE) {
  ops <- coincidencias$oportunidades
  total <- coincidencias$total
  cat_nombre <- if (nzchar(coincidencias$categoria) && coincidencias$categoria %in% names(NOMBRES_CATEGORIA)) NOMBRES_CATEGORIA[[coincidencias$categoria]] else ""
  donde <- if (nzchar(pre$departamento)) paste0(" en ", pre$departamento) else ""
  cuando <- if (!is.na(fecha_segura(coincidencias$fecha_tope))) paste0(" con cierre hasta el ", fecha_corta(fecha_segura(coincidencias$fecha_tope))) else ""
  encabezado <- sprintf("Encontré %d %s%s%s%s.", total, if (total == 1L) "oportunidad vigente" else "oportunidades vigentes", if (nzchar(cat_nombre)) paste0(" de ", cat_nombre) else "", donde, cuando)
  if (relajado) encabezado <- paste0(encabezado, " Ninguna indica ", pre$departamento, " de forma explícita, así que te muestro las de alcance nacional o sin ubicación registrada.")

  lineas <- vapply(seq_along(head(ops, 3L)), function(i) {
    o <- ops[[i]]
    entidad <- texto_seguro(o$entidad, texto_seguro(o$fuente_nombre))
    lugar <- texto_seguro(o$departamento)
    plazo <- texto_dias_restantes(fecha_segura(o$fecha_limite))
    if (isTRUE(o$fecha_limite_inferida)) plazo <- paste0(plazo, " (plazo leído del texto)")
    cabeza <- paste0(i, ") «", texto_seguro(o$titulo, "Publicación sin título"), "»", if (nzchar(entidad)) paste0(" — ", entidad) else "", if (nzchar(lugar)) paste0(" (", lugar, ")") else "", ": ", plazo, ".")
    datos <- describir_datos_clave(o)
    frag <- if (!identical(texto_seguro(o$confianza_extraccion), "requiere_revision")) fragmento_relevante(texto_seguro(o$descripcion), terminos) else ""
    paste0(cabeza, if (nzchar(datos)) paste0(" Datos: ", datos, ".") else "", if (nzchar(frag)) paste0(" Coincide porque la ficha dice: «", frag, "»") else "")
  }, character(1))

  bloques <- c(encabezado, paste(lineas, collapse = "\n"))

  if (total > 3L) {
    cierres <- vapply(ops, function(o) { f <- fecha_segura(o$fecha_limite); if (is.na(f)) Inf else as.numeric(f) }, numeric(1))
    extra <- sprintf("Hay %d más en el listado. ", total - 3L)
    if (any(is.finite(cierres))) {
      urgente <- ops[[which.min(cierres)]]
      extra <- paste0(extra, "La que cierra primero es «", texto_seguro(urgente$titulo), "» (", texto_dias_restantes(fecha_segura(urgente$fecha_limite)), ").")
    }
    bloques <- c(bloques, extra)
  }
  if (identical(pre$intencion, "comparar") && length(ops) >= 2L) {
    a <- ops[[1L]]; b <- ops[[2L]]
    bloques <- c(bloques, sprintf("Comparación rápida: «%s» %s; «%s» %s. Revisa requisitos y remuneración en cada ficha antes de decidir.",
      texto_seguro(a$titulo), texto_dias_restantes(fecha_segura(a$fecha_limite)), texto_seguro(b$titulo), texto_dias_restantes(fecha_segura(b$fecha_limite))))
  }
  bloques <- c(bloques, "Puedes pedirme los requisitos de alguna, filtrar por ciudad o por fecha límite. Confirma siempre las condiciones en el documento oficial y no pagues por postular.")
  paste(bloques, collapse = "\n\n")
}

sugerencias_seguimiento <- function(pre, coincidencias) {
  s <- character()
  if (length(coincidencias$oportunidades)) {
    s <- c(s, "Requisitos y detalles de la primera")
    if (is.na(pre$fecha_tope) && !nzchar(pre$frase_fecha)) s <- c(s, "¿Cuáles cierran esta semana?")
    if (!nzchar(pre$departamento)) s <- c(s, "¿Y solo en La Paz?", "¿Y en Santa Cruz?")
    if (length(coincidencias$oportunidades) >= 2L) s <- c(s, "Compara la primera y la segunda")
  } else {
    s <- c("Convocatorias públicas abiertas", "Becas que cierren antes de fin de mes", "Empleo público en La Paz")
  }
  head(unique(s), 4L)
}
