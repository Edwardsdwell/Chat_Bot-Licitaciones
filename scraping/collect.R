# Recopilador prudente de oportunidades y precios publicados en sitios web.
# Solo usa HTTPS, respeta robots.txt y omite autenticacion, bloqueos y paginas dinamicas.

`%||%` <- function(x, y) {
  if (is.null(x) || !length(x) || all(is.na(x))) y else x
}

paquetes_requeridos <- c("httr", "xml2", "jsonlite", "digest")
paquetes_faltantes <- paquetes_requeridos[
  !vapply(paquetes_requeridos, requireNamespace, logical(1), quietly = TRUE)
]
if (length(paquetes_faltantes)) {
  stop("Faltan paquetes de R: ", paste(paquetes_faltantes, collapse = ", "),
       ". Instala con install.packages(c(",
       paste(sprintf("'%s'", paquetes_faltantes), collapse = ", "), ")).")
}

argumentos <- commandArgs(trailingOnly = FALSE)
argumento_script <- grep("^--file=", argumentos, value = TRUE)
rutas_candidatas <- c(getwd(), dirname(getwd()))
if (length(argumento_script)) {
  rutas_candidatas <- c(rutas_candidatas, dirname(sub("^--file=", "", argumento_script[[1L]])))
}
for (marco in rev(sys.frames())) {
  ruta <- marco$ofile %||% ""
  if (length(ruta) && nzchar(ruta)) rutas_candidatas <- c(rutas_candidatas, dirname(ruta))
}

buscar_raiz <- function(rutas) {
  for (ruta in unique(rutas)) {
    actual <- normalizePath(ruta, winslash = "/", mustWork = FALSE)
    for (paso in seq_len(10L)) {
      if (file.exists(file.path(actual, "scraping", "sources.json")) &&
          file.exists(file.path(actual, "frontend", "index.html"))) return(actual)
      padre <- dirname(actual)
      if (identical(padre, actual)) break
      actual <- padre
    }
  }
  stop("No se encuentra la carpeta principal de La Pala.", call. = FALSE)
}

RAIZ_PROYECTO <- buscar_raiz(rutas_candidatas)
ARCHIVO_FUENTES <- Sys.getenv("LA_PALA_SOURCES_FILE", file.path(RAIZ_PROYECTO, "scraping", "sources.json"))
DIR_DATOS <- Sys.getenv("LA_PALA_DATA_DIR", file.path(RAIZ_PROYECTO, "data"))
DIR_LOGS <- Sys.getenv("LA_PALA_LOG_DIR", file.path(RAIZ_PROYECTO, "logs"))
ARCHIVO_CATALOGO <- Sys.getenv("LA_PALA_CATALOG_FILE", file.path(DIR_DATOS, "catalogo.json"))
ARCHIVO_REGISTRO <- file.path(DIR_LOGS, "recoleccion.log")
AGENTE_USUARIO <- Sys.getenv("LA_PALA_USER_AGENT", unset = "LaPalaBot/2.0 (contacto: administrador del proyecto)")
ESPERA_MINIMA <- suppressWarnings(as.numeric(Sys.getenv("LA_PALA_DELAY_SECONDS", unset = "8")))
if (!is.finite(ESPERA_MINIMA) || ESPERA_MINIMA < 8) ESPERA_MINIMA <- 8
TIEMPO_ESPERA_SEGUNDOS <- suppressWarnings(as.numeric(Sys.getenv("LA_PALA_TIMEOUT_SECONDS", unset = "12")))
if (!is.finite(TIEMPO_ESPERA_SEGUNDOS)) TIEMPO_ESPERA_SEGUNDOS <- 12
TIEMPO_ESPERA_SEGUNDOS <- min(60, max(5, TIEMPO_ESPERA_SEGUNDOS))
PAGINAS_POR_FUENTE <- suppressWarnings(as.integer(Sys.getenv("LA_PALA_MAX_PAGES", unset = "12")))
if (is.na(PAGINAS_POR_FUENTE) || PAGINAS_POR_FUENTE < 1L) PAGINAS_POR_FUENTE <- 12L
PAGINAS_POR_FUENTE <- min(20L, PAGINAS_POR_FUENTE)
CATEGORIA_PRIORITARIA <- Sys.getenv("LA_PALA_CATEGORIA_PRIORITARIA", unset = "")
# Consulta en línea: revisa solo las fuentes más relacionadas con una pregunta.
MODO_CONSULTA <- identical(Sys.getenv("LA_PALA_MODO", unset = ""), "consulta")
CONSULTA_ENLINEA <- Sys.getenv("LA_PALA_CONSULTA", unset = "")
MAX_FUENTES_CONSULTA <- suppressWarnings(as.integer(Sys.getenv("LA_PALA_MAX_FUENTES_CONSULTA", unset = "6")))
if (is.na(MAX_FUENTES_CONSULTA) || MAX_FUENTES_CONSULTA < 1L) MAX_FUENTES_CONSULTA <- 6L
MAX_FUENTES_CONSULTA <- min(MAX_FUENTES_CONSULTA, 10L)

dir.create(dirname(ARCHIVO_CATALOGO), recursive = TRUE, showWarnings = FALSE)
dir.create(dirname(ARCHIVO_REGISTRO), recursive = TRUE, showWarnings = FALSE)
ESPERAS_DOMINIO <- new.env(parent = emptyenv())
REGLAS_ROBOTS <- new.env(parent = emptyenv())
ESPERAS_ROBOTS <- new.env(parent = emptyenv())

registrar <- function(..., nivel = "INFO") {
  linea <- paste0(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " [", nivel, "] ", paste(..., collapse = ""))
  try(cat(linea, "\n"), silent = TRUE)
  try(cat(linea, "\n", file = ARCHIVO_REGISTRO, append = TRUE), silent = TRUE)
  if (identical(nivel, "ERROR")) {
    try(cat(linea, "\n", file = file.path(DIR_LOGS, "fallas.txt"), append = TRUE), silent = TRUE)
  }
  invisible(linea)
}

normalizar_texto <- function(valor) {
  valores <- as.character(unlist(valor, use.names = FALSE))
  texto <- paste(valores[!is.na(valores)], collapse = " ")
  texto <- iconv(texto, from = "", to = "ASCII//TRANSLIT", sub = "")
  tolower(trimws(gsub("[[:space:]]+", " ", texto)))
}

texto_seguro <- function(valor, por_defecto = "") {
  if (is.null(valor) || !length(valor) || is.list(valor) || is.na(valor[[1L]])) return(por_defecto)
  texto <- trimws(as.character(valor[[1L]]))
  if (nzchar(texto)) texto else por_defecto
}

limpiar_texto <- function(valor, maximo = 12000L) {
  if (is.null(valor) || !length(valor) || (length(valor) == 1L && is.na(valor))) return("")
  valores <- as.character(unlist(valor, use.names = FALSE))
  texto <- paste(valores[!is.na(valores)], collapse = " ")
  texto <- gsub("<[^>]+>", " ", texto)
  texto <- gsub("&nbsp;|&#160;", " ", texto, ignore.case = TRUE)
  texto <- gsub("[\r\n\t]+", " ", texto)
  texto <- trimws(gsub("[[:space:]]+", " ", texto))
  substr(texto, 1L, maximo)
}

partes_url <- function(url) {
  if (length(url) != 1L || is.na(url) || !nzchar(trimws(url))) return(NULL)
  parsed <- tryCatch(xml2::url_parse(url), error = function(e) NULL)
  if (is.null(parsed)) return(NULL)
  esquema <- tolower(as.character(parsed$scheme %||% ""))
  host <- parsed$hostname %||% parsed$server %||% parsed$host %||% ""
  host <- if (length(host) && !is.na(host[[1L]])) tolower(trimws(as.character(host[[1L]]))) else ""
  ruta <- parsed$path %||% "/"
  ruta <- if (length(ruta) && !is.na(ruta[[1L]]) && nzchar(ruta[[1L]])) as.character(ruta[[1L]]) else "/"
  list(esquema = esquema, host = host, ruta = ruta)
}

url_https_valida <- function(url) {
  partes <- partes_url(url)
  !is.null(partes) && identical(partes$esquema, "https") && nzchar(partes$host)
}

mismo_dominio <- function(a, b) {
  partes_a <- partes_url(a)
  partes_b <- partes_url(b)
  !is.null(partes_a) && !is.null(partes_b) &&
    identical(partes_a$esquema, "https") && identical(partes_b$esquema, "https") &&
    nzchar(partes_a$host) && identical(sub("^www\\.", "", partes_a$host), sub("^www\\.", "", partes_b$host))
}

url_absoluta <- function(enlace, base) {
  enlace <- trimws(as.character(enlace %||% ""))
  if (!nzchar(enlace) || grepl("^(javascript:|mailto:|tel:|data:|#)", enlace, ignore.case = TRUE)) return("")
  resultado <- tryCatch(xml2::url_absolute(enlace, base), error = function(e) "")
  if (!url_https_valida(resultado)) "" else resultado
}

leer_json <- function(ruta, alternativo) {
  if (!file.exists(ruta) || isTRUE(file.info(ruta)$isdir) || file.info(ruta)$size == 0) return(alternativo)
  tryCatch(jsonlite::fromJSON(ruta, simplifyVector = FALSE), error = function(e) {
    stop("No se pudo leer ", basename(ruta), ": ", conditionMessage(e), call. = FALSE)
  })
}

guardar_json <- function(valor, ruta) {
  dir.create(dirname(ruta), recursive = TRUE, showWarnings = FALSE)
  temporal <- tempfile(pattern = paste0(".", basename(ruta), "-"), tmpdir = dirname(ruta))
  respaldo <- paste0(ruta, ".bak")
  on.exit(if (file.exists(temporal)) unlink(temporal), add = TRUE)
  jsonlite::write_json(
    valor, temporal, auto_unbox = TRUE, pretty = TRUE,
    na = "null", null = "null", dataframe = "rows"
  )
  if (!file.exists(temporal) || is.na(file.info(temporal)$size) || file.info(temporal)$size == 0) {
    stop("No se pudo generar el JSON temporal de ", basename(ruta), call. = FALSE)
  }
  # En POSIX rename reemplaza atómicamente; Windows puede exigir mover antes el destino.
  if (file.rename(temporal, ruta)) return(invisible(TRUE))
  if (file.exists(respaldo)) unlink(respaldo)
  habia_destino <- file.exists(ruta)
  if (habia_destino && !file.rename(ruta, respaldo)) {
    stop("No se pudo respaldar ", basename(ruta), call. = FALSE)
  }
  if (!file.rename(temporal, ruta)) {
    if (habia_destino && file.exists(respaldo)) file.rename(respaldo, ruta)
    stop("No se pudo publicar ", basename(ruta), call. = FALSE)
  }
  if (file.exists(respaldo)) unlink(respaldo)
  invisible(TRUE)
}

leer_fuentes <- function() {
  datos <- leer_json(ARCHIVO_FUENTES, list(fuentes = list()))
  fuentes <- datos$fuentes %||% datos$sources %||% list()
  if (!is.list(fuentes)) stop("sources.json debe contener una lista llamada fuentes.", call. = FALSE)
  Filter(function(fuente) {
    is.list(fuente) && isTRUE(fuente$habilitada %||% fuente$enabled %||% FALSE) &&
      url_https_valida(as.character(fuente$url %||% "")) &&
      nzchar(as.character(fuente$categoria %||% fuente$category %||% ""))
  }, fuentes)
}

obtener <- function(url, accept = "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8", intento = 1L) {
  if (!url_https_valida(url)) return(NULL)
  inicial <- url
  actual <- url
  for (salto in seq_len(4L)) {
    dominio <- partes_url(actual)$host
    demora_requerida <- if (exists(dominio, envir = ESPERAS_ROBOTS, inherits = FALSE)) {
      get(dominio, envir = ESPERAS_ROBOTS, inherits = FALSE)
    } else ESPERA_MINIMA
    anterior <- if (exists(dominio, envir = ESPERAS_DOMINIO, inherits = FALSE)) get(dominio, envir = ESPERAS_DOMINIO) else 0
    restante <- max(ESPERA_MINIMA, demora_requerida) - as.numeric(difftime(Sys.time(), anterior, units = "secs"))
    if (is.finite(restante) && restante > 0) Sys.sleep(restante)
    assign(dominio, Sys.time(), envir = ESPERAS_DOMINIO)
    if (exists("informar_solicitud_recoleccion", mode = "function", inherits = TRUE)) {
      try(informar_solicitud_recoleccion(actual), silent = TRUE)
    }

    respuesta <- tryCatch(httr::GET(
      actual,
      httr::user_agent(AGENTE_USUARIO),
      httr::accept(accept),
      httr::timeout(TIEMPO_ESPERA_SEGUNDOS),
      httr::config(followlocation = FALSE)
    ), error = function(e) e)
    transitorio <- inherits(respuesta, "error") || is.null(respuesta)
    if (inherits(respuesta, "error") && grepl(
      "could not resolve host|name or service not known|temporary failure in name resolution|no such host",
      conditionMessage(respuesta), ignore.case = TRUE
    )) {
      registrar("Fallo DNS en ", actual, ": ", conditionMessage(respuesta), nivel = "ERROR")
      return(NULL)
    }
    if (transitorio && intento < 3L) {
      Sys.sleep(min(8, ESPERA_MINIMA * 2^(intento - 1L)))
      return(obtener(actual, accept, intento + 1L))
    }
    if (transitorio) {
      detalle <- if (inherits(respuesta, "error")) conditionMessage(respuesta) else "respuesta de red vacía"
      registrar("Fallo de conexión en ", actual, ": ", detalle, nivel = "ERROR")
      return(NULL)
    }
    codigo <- codigo_respuesta(respuesta)
    if (codigo %in% c(408L, 425L, 500L, 502L, 503L, 504L) && intento < 3L) {
      cabeceras <- httr::headers(respuesta)
      retry_after <- suppressWarnings(as.numeric(cabeceras[["retry-after"]] %||% NA_real_))
      demora <- if (is.finite(retry_after)) min(30, max(ESPERA_MINIMA, retry_after)) else min(8, ESPERA_MINIMA * 2^(intento - 1L))
      Sys.sleep(demora)
      return(obtener(actual, accept, intento + 1L))
    }
    if (!codigo %in% c(301L, 302L, 303L, 307L, 308L)) return(respuesta)

    destino <- httr::headers(respuesta)[["location"]] %||% ""
    siguiente <- url_absoluta(destino, actual)
    if (!nzchar(siguiente) || !mismo_dominio(inicial, siguiente)) {
      registrar("Redirección descartada en ", actual, " -> ", if (nzchar(siguiente)) siguiente else destino, " (otro dominio o no HTTPS).", nivel = "ERROR")
      return(NULL)
    }
    actual <- siguiente
  }
  NULL
}

texto_respuesta <- function(respuesta) {
  if (is.null(respuesta)) return("")
  tryCatch(httr::content(respuesta, as = "text", encoding = "UTF-8"), error = function(e) "")
}

codigo_respuesta <- function(respuesta) {
  if (is.null(respuesta)) return(0L)
  tryCatch(as.integer(httr::status_code(respuesta)), error = function(e) 0L)
}

parsear_robots <- function(texto) {
  # RFC 9309: combina user-agent contiguos del mismo grupo y aplica
  # las reglas del grupo si incluye "*" o el agente LaPalaBot.
  bloques <- strsplit(gsub("\r", "", texto, fixed = TRUE), "\n[[:space:]]*\n", perl = TRUE)[[1L]]
  resultado <- list(bloqueadas = character(), permitidas = character(), espera = 0, mapas = character())
  for (bloque in bloques) {
    lineas <- trimws(sub("#.*$", "", strsplit(bloque, "\n", fixed = TRUE)[[1L]]))
    lineas <- lineas[nzchar(lineas)]
    if (!length(lineas)) next
    pares <- lapply(lineas, function(linea) {
      partes <- strsplit(linea, ":", fixed = TRUE)[[1L]]
      if (length(partes) < 2L) return(NULL)
      list(clave = tolower(trimws(partes[1L])), valor = trimws(paste(partes[-1L], collapse = ":")))
    })
    pares <- Filter(Negate(is.null), pares)
    agentes <- vapply(Filter(function(x) identical(x$clave, "user-agent"), pares),
                      function(x) tolower(x$valor), character(1))
    aplica <- length(agentes) > 0L &&
      any(agentes == "*" | grepl("lapalabot", agentes, fixed = TRUE))
    for (par in pares) {
      if (par$clave == "sitemap" && url_https_valida(par$valor)) {
        resultado$mapas <- c(resultado$mapas, par$valor)
        next
      }
      if (!aplica) next
      if (par$clave == "disallow" && nzchar(par$valor)) {
        resultado$bloqueadas <- c(resultado$bloqueadas, par$valor)
      } else if (par$clave == "allow" && nzchar(par$valor)) {
        resultado$permitidas <- c(resultado$permitidas, par$valor)
      } else if (par$clave == "crawl-delay") {
        demora <- suppressWarnings(as.numeric(par$valor))
        if (length(demora) == 1L && is.finite(demora) && demora >= 0) {
          resultado$espera <- max(resultado$espera, demora)
        }
      }
    }
  }
  resultado$mapas <- unique(resultado$mapas)
  resultado
}

coincide_regla_robots <- function(ruta, patron) {
  if (!nzchar(patron)) return(FALSE)
  final_exacto <- endsWith(patron, "$")
  patron <- sub("\\$$", "", patron)
  patron <- gsub("*", ".*", patron, fixed = TRUE)
  expresion <- paste0("^", patron, if (final_exacto) "$" else "")
  isTRUE(tryCatch(grepl(expresion, ruta, perl = TRUE), error = function(e) FALSE))
}

regla_aplica <- function(ruta, bloqueadas, permitidas) {
  coincide_bloqueo <- bloqueadas[vapply(bloqueadas, function(patron) coincide_regla_robots(ruta, patron), logical(1))]
  coincide_permiso <- permitidas[vapply(permitidas, function(patron) coincide_regla_robots(ruta, patron), logical(1))]
  if (!length(coincide_bloqueo)) return(TRUE)
  if (!length(coincide_permiso)) return(FALSE)
  max(nchar(gsub("[*$]", "", coincide_permiso))) >= max(nchar(gsub("[*$]", "", coincide_bloqueo)))
}

revisar_robots <- function(url) {
  parsed <- partes_url(url)
  if (is.null(parsed) || !nzchar(parsed$host)) return(list(permitido = FALSE, bloqueadas = character(), permitidas = character(), demora = ESPERA_MINIMA))
  origen <- paste0(parsed$esquema, "://", parsed$host)
  if (exists(origen, envir = REGLAS_ROBOTS, inherits = FALSE)) return(get(origen, envir = REGLAS_ROBOTS))
  respuesta <- obtener(paste0(origen, "/robots.txt"), "text/plain,*/*")
  codigo <- codigo_respuesta(respuesta)
  if (!is.null(respuesta) && codigo >= 400L && codigo < 500L && !codigo %in% c(401L, 403L, 408L, 425L, 429L, 451L)) {
    # RFC 9309 trata los 4xx ordinarios como robots.txt no disponible; se puede
    # consultar la página pública. Auth, denegación explícita y rate limit siguen cerrados.
    reglas <- list(permitido = TRUE, estado = if (codigo == 404L) "sin_archivo_robots" else "robots_no_disponible_4xx",
                   demora = ESPERA_MINIMA, mapas = character(), bloqueadas = character(), permitidas = character())
  } else if (is.null(respuesta) || codigo == 0L || codigo >= 500L || codigo %in% c(401L, 403L, 429L) || codigo < 200L || codigo >= 300L) {
    estado <- if (codigo %in% c(401L, 403L)) "bloqueado" else "no_verificable"
    registrar("No se pudo verificar robots.txt en ", origen, " (HTTP ", codigo, if (is.null(respuesta)) "; sin respuesta: red, certificado TLS o redirección" else "", ").", nivel = "ERROR")
    reglas <- list(permitido = FALSE, estado = estado, demora = ESPERA_MINIMA, mapas = character(), bloqueadas = character(), permitidas = character())
  } else {
    parseadas <- parsear_robots(texto_respuesta(respuesta))
    ruta <- partes_url(url)$ruta
    permitido <- regla_aplica(ruta, parseadas$bloqueadas, parseadas$permitidas)
    reglas <- list(
      permitido = permitido,
      estado = if (permitido) "permitido" else "bloqueado",
      demora = max(ESPERA_MINIMA, parseadas$espera, na.rm = TRUE),
      mapas = parseadas$mapas,
      bloqueadas = parseadas$bloqueadas,
      permitidas = parseadas$permitidas
    )
  }
  assign(origen, reglas, envir = REGLAS_ROBOTS)
  assign(parsed$host, reglas$demora, envir = ESPERAS_ROBOTS)
  reglas
}

permiso_url <- function(url, reglas) {
  if (!isTRUE(reglas$permitido)) return(FALSE)
  ruta <- partes_url(url)$ruta
  regla_aplica(ruta, reglas$bloqueadas, reglas$permitidas)
}

patrones_categoria <- list(
  convocatorias = "convocatoria|llamado|programa|concurso|postulacion|oportunidad",
  empleo_publico = "empleo|trabajo|personal|servidor|cargo|vacante|seleccion|recursos humanos|convocatoria",
  contrataciones = "sicoes|licitacion|contratacion|contrato|compra|adquisicion|convocatoria|proveedor",
  merito = "merito|seleccion|evaluacion|convocatoria|admisiones|concurso|docente",
  becas = "beca|pasantia|movilidad|estudio|scholarship|convocatoria|programa|posgrado|maestria|doctorado|intercambio",
  fondos = "fondo|financiamiento|financiar|apoyo|grant|convocatoria|proyecto",
  precios = "precio|producto|farmacia|alquiler|repuesto|mercado|cotizacion",
  normativa = "norma|gaceta|ley|decreto|resolucion|reglamento|edicto"
)

es_enlace_relevante <- function(url, texto, categoria) {
  patron <- patrones_categoria[[categoria]] %||% "convocatoria|oportunidad"
  texto_busqueda <- normalizar_texto(paste(url, texto))
  grepl(patron, texto_busqueda, perl = TRUE) && !grepl("\\.(jpg|jpeg|png|gif|css|js|zip|rar)(\\?|$)", tolower(url))
}

enlaces_html <- function(documento, pagina, categoria) {
  nodos <- xml2::xml_find_all(documento, "//a[@href]")
  if (!length(nodos)) return(character())
  hrefs <- xml2::xml_attr(nodos, "href")
  textos <- xml2::xml_text(nodos)
  enlaces <- vapply(seq_along(hrefs), function(i) url_absoluta(hrefs[i], pagina), character(1))
  contextos <- vapply(seq_along(nodos), function(i) {
    nodo <- nodos[[i]]
    contenedor <- tryCatch(xml2::xml_find_first(
      nodo, "ancestor::tr[1] | ancestor::article[1] | ancestor::li[1] | ancestor::*[contains(@class,'card') or contains(@class,'item')][1]"
    ), error = function(e) NULL)
    if (is.null(contenedor) || inherits(contenedor, "xml_missing")) {
      contenedor <- tryCatch(xml2::xml_parent(nodo), error = function(e) NULL)
    }
    if (is.null(contenedor) || inherits(contenedor, "xml_missing")) "" else limpiar_texto(xml2::xml_text(contenedor), 1600L)
  }, character(1))
  keep <- vapply(seq_along(enlaces), function(i) {
    if (!nzchar(enlaces[i]) || !mismo_dominio(enlaces[i], pagina)) return(FALSE)
    # Excluye recursos que no son publicaciones, pero acepta enlaces genéricos
    # cuando el título/fecha/contexto del registro identifica una oportunidad.
    if (grepl("\\.(jpg|jpeg|png|gif|svg|css|js|zip|rar|7z)(\\?|$)", tolower(enlaces[i]))) return(FALSE)
    es_enlace_relevante(enlaces[i], paste(textos[i], contextos[i]), categoria)
  }, logical(1))
  unique(enlaces[keep])
}

enlaces_mapa <- function(url, reglas, categoria) {
  mapas <- unique(c(reglas$mapas, url_absoluta("/sitemap.xml", url)))
  salida <- character()
  for (mapa in head(mapas, 3L)) {
    if (!mismo_dominio(mapa, url) || !permiso_url(mapa, reglas)) next
    respuesta <- obtener(mapa, "application/xml,text/xml,*/*")
    codigo <- codigo_respuesta(respuesta)
    if (is.null(respuesta) || codigo < 200L || codigo >= 300L) next
    documento <- tryCatch(xml2::read_xml(texto_respuesta(respuesta)), error = function(e) NULL)
    if (is.null(documento)) next
    ubicaciones <- xml2::xml_text(xml2::xml_find_all(documento, "//*[local-name()='loc']"))
    ubicaciones <- vapply(ubicaciones, url_absoluta, character(1), base = mapa)
    salida <- c(salida, ubicaciones[vapply(ubicaciones, function(u) {
      mismo_dominio(u, url) && es_enlace_relevante(u, u, categoria)
    }, logical(1))])
  }
  unique(salida)
}

extraer_fecha_limite <- function(texto) {
  normalizado <- normalizar_texto(texto)
  contexto <- regexpr("(fecha limite|plazo de postulacion|hasta el|cierre de postulaciones|fecha de cierre)[^0-9]{0,55}", normalizado, perl = TRUE)
  if (contexto[1L] < 0L) return("")
  desde <- contexto[1L] + attr(contexto, "match.length")
  resto <- substr(normalizado, desde, nchar(normalizado))

  formato_iso <- regexpr("[0-9]{4}-(0?[1-9]|1[0-2])-(0?[1-9]|[12][0-9]|3[01])", resto, perl = TRUE)
  if (formato_iso[1L] > 0L) {
    valor <- regmatches(resto, formato_iso)
    fecha <- suppressWarnings(as.Date(valor, format = "%Y-%m-%d"))
    if (!is.na(fecha)) return(as.character(fecha))
  }

  fecha_numerica <- regexpr("(0?[1-9]|[12][0-9]|3[01])[/.-](0?[1-9]|1[0-2])[/.-][0-9]{4}", resto, perl = TRUE)
  if (fecha_numerica[1L] > 0L) {
    valor <- regmatches(resto, fecha_numerica)
    separador <- if (grepl("/", valor, fixed = TRUE)) "/" else if (grepl(".", valor, fixed = TRUE)) "." else "-"
     formato <- paste0("%d", separador, "%m", separador, "%Y")
    fecha <- suppressWarnings(as.Date(valor, format = formato))
    if (!is.na(fecha)) return(as.character(fecha))
  }

  meses <- c(enero = "01", febrero = "02", marzo = "03", abril = "04", mayo = "05", junio = "06", julio = "07", agosto = "08", septiembre = "09", setiembre = "09", octubre = "10", noviembre = "11", diciembre = "12")
  fecha_textual <- regexpr("[0-3]?[0-9] de (enero|febrero|marzo|abril|mayo|junio|julio|agosto|septiembre|setiembre|octubre|noviembre|diciembre) de? [0-9]{4}", resto, perl = TRUE)
  if (fecha_textual[1L] > 0L) {
    valor <- regmatches(resto, fecha_textual)
    partes <- strsplit(valor, " +")[[1L]]
    mes <- meses[[partes[3L]]]
    fecha <- suppressWarnings(as.Date(sprintf("%04d-%s-%02d", as.integer(partes[5L]), mes, as.integer(partes[1L]))))
    if (!is.na(fecha)) return(as.character(fecha))
  }
  ""
}

titulo_pagina <- function(documento) {
  titulo <- limpiar_texto(xml2::xml_text(xml2::xml_find_first(documento, "//main//h1 | //article//h1 | //h1")), 240L)
  if (!nzchar(titulo)) titulo <- limpiar_texto(xml2::xml_text(xml2::xml_find_first(documento, "//title")), 240L)
  titulo
}

texto_pagina <- function(documento) {
  serializado <- xml2::xml_serialize(documento, NULL)
  copia <- xml2::xml_unserialize(serializado)
  ruido <- xml2::xml_find_all(copia, "//script | //style | //noscript | //nav | //footer | //header")
  if (length(ruido)) xml2::xml_remove(ruido)
  limpiar_texto(xml2::xml_text(xml2::xml_find_first(copia, "//body")), 12000L)
}

detectar_barrera <- function(texto, documento) {
  plano <- normalizar_texto(texto)
  if (grepl("captcha|verify you are human|prove you are human|cloudflare challenge|access denied|acceso denegado|solicitud bloqueada", plano)) return("bloqueada_por_el_portal")
  if (grepl("login required|sign in to view|log in to continue|inicia sesion para ver|inicia sesion para continuar", plano)) return("requiere_autenticacion")
  if (grepl("javascript is required|enable javascript|please enable javascript|activa javascript|habilita javascript", plano)) return("requiere_javascript")
  if (!is.null(documento)) {
    contrasena <- tryCatch(xml2::xml_find_first(documento, "//input[translate(@type,'PASSWORD','password')='password']"), error = function(e) NULL)
    if (!is.null(contrasena) && !inherits(contrasena, "xml_missing")) return("requiere_autenticacion")
    nodos_app <- tryCatch(xml2::xml_find_all(documento, "//*[@id='root' or @id='app' or @id='__next' or @id='__nuxt']"), error = function(e) list())
    scripts <- tryCatch(xml2::xml_find_all(documento, "//script[@src]"), error = function(e) list())
    if (nchar(plano) < 60L && length(nodos_app) && length(scripts)) return("requiere_javascript")
  }
  ""
}

extraer_productos <- function(documento, fuente, pagina) {
  nodos <- xml2::xml_find_all(documento, "//*[@itemtype and contains(@itemtype,'Product')] | //script[contains(translate(@type,'JSONLD','jsonld'),'ld+json')]")
  salida <- list()
  for (nodo in nodos) {
    contenido <- if (identical(xml2::xml_name(nodo), "script")) {
      tryCatch(jsonlite::fromJSON(xml2::xml_text(nodo), simplifyVector = FALSE), error = function(e) NULL)
    } else {
      list(
        "@type" = "Product",
        name = xml2::xml_text(xml2::xml_find_first(nodo, ".//*[@itemprop='name']")),
        offers = list(
          price = xml2::xml_attr(xml2::xml_find_first(nodo, ".//*[@itemprop='price']"), "content"),
          priceCurrency = xml2::xml_attr(xml2::xml_find_first(nodo, ".//*[@itemprop='priceCurrency']"), "content")
        )
      )
    }
    visitar <- function(valor) {
      if (!is.list(valor)) return(invisible(NULL))
      tipos <- tolower(as.character(unlist(valor[["@type"]] %||% "", use.names = FALSE)))
      if ("product" %in% tipos) {
        oferta <- valor$offers %||% list()
        if (is.list(oferta) && length(oferta) && is.list(oferta[[1L]])) oferta <- oferta[[1L]]
        precio <- suppressWarnings(as.numeric(as.character(oferta$price %||% valor$price %||% NA)))
        producto <- limpiar_texto(valor$name %||% "", 180L)
        if (nzchar(producto) && length(precio) == 1L && is.finite(precio) && precio > 0) {
          salida[[length(salida) + 1L]] <<- list(
            producto = producto,
            precio = precio,
            moneda = as.character(oferta$priceCurrency %||% valor$priceCurrency %||% ""),
            unidad = limpiar_texto(valor$unitText %||% "", 80L),
            ciudad = as.character(fuente$ciudad %||% ""),
            departamento = as.character(fuente$departamento %||% ""),
            observado_en = as.character(Sys.Date()),
            fuente_id = as.character(fuente$id),
            fuente_nombre = as.character(fuente$nombre %||% ""),
            fuente_url = pagina,
            estado = "observado"
          )
        }
      }
      for (hijo in valor) if (is.list(hijo)) visitar(hijo)
      invisible(NULL)
    }
    visitar(contenido)
  }
  salida
}

extraer_imagen_publicacion <- function(documento, pagina) {
  metadatos <- xml2::xml_find_all(documento, "//meta[@property='og:image' or @name='twitter:image' or @name='twitter:image:src']")
  candidatas_meta <- vapply(metadatos, function(nodo) texto_seguro(xml2::xml_attr(nodo, "content")), character(1))
  imagenes <- xml2::xml_find_all(documento, "//main//img | //article//img")
  candidatas_html <- vapply(imagenes, function(nodo) {
    alternativo <- normalizar_texto(xml2::xml_attr(nodo, "alt") %||% "")
    ancho <- suppressWarnings(as.numeric(xml2::xml_attr(nodo, "width")))
    imagen_pequena <- length(ancho) == 1L && !is.na(ancho) && is.finite(ancho) && ancho < 140
    if (grepl("logo|escudo|avatar|icono", alternativo) || imagen_pequena) return("")
    origen <- texto_seguro(xml2::xml_attr(nodo, "src"))
    if (!nzchar(origen)) origen <- texto_seguro(xml2::xml_attr(nodo, "data-src"))
    origen
  }, character(1))
  for (candidata in c(candidatas_meta, candidatas_html)) {
    imagen <- url_absoluta(candidata, pagina)
    if (nzchar(imagen) && url_https_valida(imagen)) return(imagen)
  }
  ""
}

extraer_oportunidad <- function(documento, fuente, pagina, texto_enlace = "") {
  titulo <- titulo_pagina(documento)
  cuerpo <- texto_pagina(documento)
  if (nchar(cuerpo) < 100L || !nzchar(titulo)) return(NULL)
  categoria <- as.character(fuente$categoria %||% "convocatorias")
  plano <- normalizar_texto(paste(titulo, texto_enlace, cuerpo))
  if (grepl("captcha|access denied|acceso denegado|inicia sesion", plano)) return(NULL)
  titulo_plano <- normalizar_texto(titulo)
  titulos_de_seccion <- paste0(
    "^(inicio|home|buscar|busqueda|resultados|convocatorias?|noticias?|becas?|",
    "contrataciones( menores| directas| en el extranjero)?|",
    "licitaciones?( publicas?)?|programa anual de contrataciones|",
    "publicacion( omas)?|requisitos|marco normativo|compras publicas|",
    "adquisiciones?|empleos?|ofertas laborales?)$"
  )
  # No convertir páginas índice/sección en oportunidades individuales.
  if (grepl(titulos_de_seccion, titulo_plano)) return(NULL)
  fecha_limite <- extraer_fecha_limite(cuerpo)
  url_canonica <- sub("#.*$", "", pagina)
  hash <- digest::digest(paste(normalizar_texto(titulo), substr(cuerpo, 1L, 1800L)), algo = "sha256")
  list(
    id = digest::digest(url_canonica, algo = "sha256"),
    categoria = categoria,
    titulo = titulo,
    imagen_url = extraer_imagen_publicacion(documento, pagina),
    entidad = as.character(fuente$nombre %||% "Fuente institucional"),
    descripcion = substr(cuerpo, 1L, 1100L),
    requisitos = "",
    departamento = as.character(fuente$departamento %||% ""),
    fecha_publicacion = "",
    fecha_limite = fecha_limite,
    extraido_en = as.character(Sys.time()),
    verificado_en = as.character(Sys.time()),
    estado = if (nzchar(fecha_limite) && as.Date(fecha_limite) < Sys.Date()) "vencida" else "abierta",
    url = url_canonica,
    fuente_id = as.character(fuente$id),
    fuente_nombre = as.character(fuente$nombre %||% "Fuente institucional"),
    fuente_url = as.character(fuente$url),
    fuente_oficial = isTRUE(fuente$oficial %||% FALSE),
    confianza_extraccion = "requiere_revision",
    hash_contenido = hash
  )
}

parsear_fecha_publicada <- function(texto) {
  valor <- limpiar_texto(texto, 120L)
  if (!nzchar(valor)) return("")
  fecha_iso <- suppressWarnings(as.Date(substr(valor, 1L, 10L)))
  if (length(fecha_iso) == 1L && !is.na(fecha_iso)) return(as.character(fecha_iso))
  hora <- suppressWarnings(as.POSIXct(
    valor,
    tz = "UTC",
    tryFormats = c(
      "%a, %d %b %Y %H:%M:%S %z",
      "%a, %d %b %Y %H:%M:%S GMT",
      "%Y-%m-%dT%H:%M:%OS%z",
      "%Y-%m-%d %H:%M:%S"
    )
  ))
  if (length(hora) != 1L || is.na(hora)) return("")
  as.character(as.Date(hora, tz = "UTC"))
}

crear_oportunidad_fuente <- function(titulo, descripcion, url, fuente, publicada = "", limite = "", entidad = "", imagen_url = "") {
  titulo <- limpiar_texto(titulo, 240L)
  descripcion <- limpiar_texto(descripcion, 1100L)
  if (!nzchar(titulo) || !url_https_valida(url) || !mismo_dominio(url, fuente$url)) return(NULL)
  if (!nzchar(limite)) limite <- extraer_fecha_limite(descripcion)
  fecha_publicacion <- parsear_fecha_publicada(publicada)
  cuerpo_hash <- digest::digest(paste(normalizar_texto(titulo), normalizar_texto(descripcion)), algo = "sha256")
  list(
    id = digest::digest(sub("#.*$", "", url), algo = "sha256"),
    categoria = as.character(fuente$categoria %||% "convocatorias"),
    titulo = titulo,
    imagen_url = url_absoluta(imagen_url, url),
    entidad = limpiar_texto(entidad, 180L),
    descripcion = descripcion,
    requisitos = "",
    departamento = as.character(fuente$departamento %||% ""),
    fecha_publicacion = fecha_publicacion,
    fecha_limite = limite,
    extraido_en = as.character(Sys.time()),
    verificado_en = as.character(Sys.time()),
    estado = if (nzchar(limite) && as.Date(limite) < Sys.Date()) "vencida" else "abierta",
    url = sub("#.*$", "", url),
    fuente_id = as.character(fuente$id),
    fuente_nombre = as.character(fuente$nombre %||% "Fuente institucional"),
    fuente_url = as.character(fuente$url),
    fuente_oficial = isTRUE(fuente$oficial %||% FALSE),
    confianza_extraccion = "requiere_revision",
    hash_contenido = cuerpo_hash
  )
}

valor_de <- function(registro, nombres) {
  for (nombre in nombres) {
    valor <- registro[[nombre]]
    if (!is.null(valor) && length(valor) && !is.list(valor) && !all(is.na(valor))) {
      texto <- limpiar_texto(valor, 12000L)
      if (nzchar(texto)) return(texto)
    }
  }
  ""
}

oportunidades_de_feed <- function(documento, base, fuente) {
  entradas <- xml2::xml_find_all(documento, "//*[local-name()='item' or local-name()='entry']")
  resultado <- list()
  for (entrada in entradas) {
    titulo <- limpiar_texto(xml2::xml_text(xml2::xml_find_first(entrada, "./*[local-name()='title']")), 240L)
    nodo_enlace <- xml2::xml_find_first(entrada, "./*[local-name()='link']")
    enlace <- xml2::xml_attr(nodo_enlace, "href")
    if (is.na(enlace) || !nzchar(enlace)) enlace <- xml2::xml_text(nodo_enlace)
    enlace <- url_absoluta(enlace, base)
    descripcion <- limpiar_texto(xml2::xml_text(xml2::xml_find_first(
      entrada,
      "./*[local-name()='description' or local-name()='summary' or local-name()='content' or local-name()='encoded']"
    )), 1100L)
    publicada <- limpiar_texto(xml2::xml_text(xml2::xml_find_first(
      entrada,
      "./*[local-name()='pubDate' or local-name()='published' or local-name()='updated' or local-name()='date']"
    )), 120L)
    nodo_imagen <- xml2::xml_find_first(entrada, ".//*[local-name()='thumbnail' or local-name()='content' or local-name()='enclosure']")
    imagen_feed <- texto_seguro(xml2::xml_attr(nodo_imagen, "url"))
    oportunidad <- crear_oportunidad_fuente(titulo, descripcion, enlace, fuente, publicada, imagen_url = imagen_feed)
    if (!is.null(oportunidad)) resultado[[length(resultado) + 1L]] <- oportunidad
  }
  resultado
}

oportunidades_de_json <- function(texto, fuente) {
  datos <- tryCatch(jsonlite::fromJSON(texto, simplifyVector = FALSE), error = function(e) NULL)
  if (!is.list(datos)) return(list())
  registros <- datos$opportunities %||% datos$oportunidades %||% datos$items %||% datos$results %||% datos$data %||% list()
  if (is.list(registros) && !is.null(registros$title %||% registros$titulo)) registros <- list(registros)
  resultado <- list()
  for (registro in registros) {
    if (!is.list(registro)) next
    enlace <- url_absoluta(valor_de(registro, c("url", "link", "enlace")), fuente$url)
    fecha_limite <- valor_de(registro, c("fecha_limite", "deadline", "validThrough", "cierre"))
    fecha_limite <- parsear_fecha_publicada(fecha_limite) %||% ""
    if (!nzchar(fecha_limite)) fecha_limite <- extraer_fecha_limite(paste("fecha limite", valor_de(registro, c("fecha_limite", "deadline", "validThrough", "cierre"))))
    oportunidad <- crear_oportunidad_fuente(
      valor_de(registro, c("titulo", "título", "title", "name", "nombre")),
      valor_de(registro, c("descripcion", "description", "summary", "resumen")),
      enlace,
      fuente,
      publicada = valor_de(registro, c("fecha_publicacion", "published_at", "datePublished", "fecha")),
      limite = fecha_limite,
      entidad = valor_de(registro, c("entidad", "organization", "organizacion", "company")),
      imagen_url = valor_de(registro, c("imagen_url", "imagen", "image_url", "image", "thumbnail"))
    )
    if (!is.null(oportunidad)) resultado[[length(resultado) + 1L]] <- oportunidad
  }
  resultado
}

oportunidades_de_csv <- function(texto, fuente) {
  tabla <- tryCatch(utils::read.csv(text = texto, stringsAsFactors = FALSE, check.names = FALSE), error = function(e) NULL)
  if (is.null(tabla) || !nrow(tabla)) return(list())
  resultado <- list()
  for (indice in seq_len(nrow(tabla))) {
    registro <- as.list(tabla[indice, , drop = FALSE])
    nombres <- vapply(names(registro), normalizar_texto, character(1))
    names(registro) <- nombres
    enlace <- url_absoluta(valor_de(registro, c("url", "link", "enlace")), fuente$url)
    fecha_limite <- valor_de(registro, c("fecha limite", "deadline", "validthrough", "cierre"))
    fecha_limite <- parsear_fecha_publicada(fecha_limite) %||% ""
    if (!nzchar(fecha_limite)) fecha_limite <- extraer_fecha_limite(paste("fecha limite", valor_de(registro, c("fecha limite", "deadline", "validthrough", "cierre"))))
    oportunidad <- crear_oportunidad_fuente(
      valor_de(registro, c("titulo", "title", "name", "nombre")),
      valor_de(registro, c("descripcion", "description", "summary", "resumen")),
      enlace,
      fuente,
      publicada = valor_de(registro, c("fecha publicacion", "published at", "datepublished", "fecha")),
      limite = fecha_limite,
      entidad = valor_de(registro, c("entidad", "organization", "organizacion", "company")),
      imagen_url = valor_de(registro, c("imagen url", "imagen", "image url", "image", "thumbnail"))
    )
    if (!is.null(oportunidad)) resultado[[length(resultado) + 1L]] <- oportunidad
  }
  resultado
}

precios_de_registros <- function(registros, fuente) {
  salida <- list()
  for (registro in registros) {
    if (!is.list(registro)) next
    precio_texto <- valor_de(registro, c("precio", "price", "valor"))
    precio <- suppressWarnings(as.numeric(precio_texto))
    producto <- valor_de(registro, c("producto", "product", "articulo", "item", "nombre"))
    if (!nzchar(producto) || length(precio) != 1L || !is.finite(precio) || precio <= 0) next
    salida[[length(salida) + 1L]] <- list(
      producto = producto,
      precio = precio,
      moneda = valor_de(registro, c("moneda", "currency")),
      unidad = valor_de(registro, c("unidad", "unit")),
      ciudad = valor_de(registro, c("ciudad", "city")) %||% as.character(fuente$ciudad %||% ""),
      departamento = valor_de(registro, c("departamento", "department")) %||% as.character(fuente$departamento %||% ""),
      observado_en = parsear_fecha_publicada(valor_de(registro, c("observado_en", "observed_at", "fecha"))) %||% as.character(Sys.Date()),
      fuente_id = as.character(fuente$id),
      fuente_nombre = as.character(fuente$nombre %||% ""),
      fuente_url = url_absoluta(valor_de(registro, c("url", "link")), fuente$url) %||% as.character(fuente$url),
      estado = "observado"
    )
    ultimo <- salida[[length(salida)]]
    if (!nzchar(ultimo$ciudad)) ultimo$ciudad <- as.character(fuente$ciudad %||% "")
    if (!nzchar(ultimo$departamento)) ultimo$departamento <- as.character(fuente$departamento %||% "")
    if (!nzchar(ultimo$observado_en)) ultimo$observado_en <- as.character(Sys.Date())
    if (!nzchar(ultimo$fuente_url)) ultimo$fuente_url <- as.character(fuente$url)
    salida[[length(salida)]] <- ultimo
  }
  salida
}

precios_de_json <- function(texto, fuente) {
  datos <- tryCatch(jsonlite::fromJSON(texto, simplifyVector = FALSE), error = function(e) NULL)
  if (!is.list(datos)) return(list())
  registros <- datos$precios %||% datos$prices %||% datos$items %||% datos$data %||% list()
  if (is.list(registros) && !is.null(registros$precio %||% registros$price)) registros <- list(registros)
  precios_de_registros(registros, fuente)
}

precios_de_csv <- function(texto, fuente) {
  tabla <- tryCatch(utils::read.csv(text = texto, stringsAsFactors = FALSE, check.names = FALSE), error = function(e) NULL)
  if (is.null(tabla) || !nrow(tabla)) return(list())
  registros <- lapply(seq_len(nrow(tabla)), function(indice) {
    fila <- as.list(tabla[indice, , drop = FALSE])
    names(fila) <- vapply(names(fila), normalizar_texto, character(1))
    fila
  })
  precios_de_registros(registros, fuente)
}

descubrir_feeds <- function(documento, pagina) {
  nodos <- xml2::xml_find_all(documento, "//link[@href and (contains(translate(@type,'RSSATOM','rssatom'),'rss') or contains(translate(@type,'RSSATOM','rssatom'),'atom'))]")
  hrefs <- xml2::xml_attr(nodos, "href")
  feeds <- vapply(hrefs, url_absoluta, character(1), base = pagina)
  unique(feeds[vapply(feeds, function(feed) nzchar(feed) && mismo_dominio(feed, pagina), logical(1))])
}

oportunidades_de_tabla <- function(documento, pagina, fuente) {
  filas <- xml2::xml_find_all(documento, "//table//tr[td]")
  if (!length(filas)) return(list())
  patron <- patrones_categoria[[as.character(fuente$categoria)]] %||% "convocatoria|oportunidad"
  resultado <- list()

  for (fila in filas) {
    celdas <- xml2::xml_find_all(fila, "./th|./td")
    textos <- vapply(celdas, function(celda) limpiar_texto(xml2::xml_text(celda), 1800L), character(1))
    texto_fila <- limpiar_texto(textos, 5000L)
    plano <- normalizar_texto(texto_fila)
    if (length(celdas) < 2L || nchar(texto_fila) < 25L || !grepl(patron, plano, perl = TRUE)) next

    nodos_enlace <- xml2::xml_find_all(fila, ".//a[@href]")
    hrefs <- xml2::xml_attr(nodos_enlace, "href")
    enlaces <- vapply(hrefs, url_absoluta, character(1), base = pagina)
    enlaces <- enlaces[vapply(enlaces, function(enlace) nzchar(enlace) && mismo_dominio(enlace, pagina), logical(1))]
    if (!length(enlaces)) next

    textos_normalizados <- vapply(textos, normalizar_texto, character(1))
    genericas <- grepl("^(n[roº°.]?|estado|descargar|habilitados?|convocatoria|ver|detalle)$", textos_normalizados)
    candidatos_titulo <- textos[!genericas & nchar(textos) > 8L & nchar(textos) <= 280L]
    indices <- which(nzchar(textos) & grepl(patron, textos_normalizados, perl = TRUE))
    titulo <- if (length(indices)) textos[indices[1L]] else if (length(candidatos_titulo)) candidatos_titulo[which.max(nchar(candidatos_titulo))] else ""
    if (!nzchar(titulo)) next

    oportunidad <- crear_oportunidad_fuente(titulo, texto_fila, enlaces[1L], fuente, limite = extraer_fecha_limite(texto_fila))
    if (is.null(oportunidad)) next
    if (grepl("proceso concluido|convocatoria cerrada|plazo vencido|no vigente", plano)) {
      oportunidad$estado <- "cerrada"
    }
    resultado[[length(resultado) + 1L]] <- oportunidad
  }
  resultado
}

leer_detalle <- function(url, reglas, fuente, texto_enlace = "") {
  categoria <- as.character(fuente$categoria %||% fuente$category %||% "convocatorias")
  if (!mismo_dominio(url, fuente$url) || !permiso_url(url, reglas)) return(list(omitida = TRUE, motivo = "fuera_de_alcance_o_bloqueada"))
  respuesta <- obtener(url)
  codigo <- codigo_respuesta(respuesta)
  if (is.null(respuesta) || codigo < 200L || codigo >= 400L) return(list(omitida = TRUE, motivo = paste0("http_", codigo)))
  final <- respuesta$url %||% url
  if (!mismo_dominio(url, final)) return(list(omitida = TRUE, motivo = "redireccion_a_otro_dominio"))
  documento <- tryCatch(xml2::read_html(texto_respuesta(respuesta)), error = function(e) NULL)
  if (is.null(documento)) return(list(omitida = TRUE, motivo = "html_no_interpretable"))
  cuerpo <- texto_pagina(documento)
  barrera <- detectar_barrera(cuerpo, documento)
  if (nzchar(barrera)) return(list(omitida = TRUE, motivo = barrera))
  lista_precios <- if (identical(categoria, "precios")) extraer_productos(documento, fuente, url) else list()
  list(
    omitida = FALSE,
    oportunidad = extraer_oportunidad(documento, fuente, url, texto_enlace),
    oportunidades_tabla = oportunidades_de_tabla(documento, url, fuente),
    precios = lista_precios,
    cuerpo = cuerpo
  )
}

recopilar_fuente <- function(fuente, publicar = function(oportunidades = list(), precios = list()) invisible(NULL)) {
  url <- as.character(fuente$url)
  categoria <- as.character(fuente$categoria)
  metodo <- tolower(as.character(fuente$metodo %||% "automatico"))
  registrar("Fuente: ", fuente$nombre %||% url)
  reglas <- revisar_robots(url)
  if (!isTRUE(reglas$permitido)) {
    return(list(id = fuente$id, url = url, estado = "omitida", motivo = paste("robots.txt", reglas$estado), oportunidades = list(), precios = list(), revisadas = 0L))
  }

  respuesta <- obtener(url)
  codigo <- codigo_respuesta(respuesta)
  if (is.null(respuesta) || codigo < 200L || codigo >= 400L) {
    return(list(id = fuente$id, url = url, estado = "no_disponible", motivo = paste("HTTP", codigo), oportunidades = list(), precios = list(), revisadas = 0L))
  }
  if (!mismo_dominio(url, respuesta$url %||% url)) {
    return(list(id = fuente$id, url = url, estado = "redireccion_externa", motivo = "La fuente redirige a otro dominio.", oportunidades = list(), precios = list(), revisadas = 0L))
  }

  cabeceras <- httr::headers(respuesta)
  tipo_contenido <- tolower(cabeceras[["content-type"]] %||% "")
  ruta <- tolower(partes_url(url)$ruta)
  cuerpo_original <- texto_respuesta(respuesta)
  es_json <- metodo %in% c("json", "api") ||
    (metodo %in% c("automatico", "auto") && (grepl("json", tipo_contenido) || grepl("\\.json$", ruta)))
  es_csv <- metodo == "csv" ||
    (metodo %in% c("automatico", "auto") && (grepl("csv", tipo_contenido) || grepl("\\.csv$", ruta)))
  es_feed <- metodo %in% c("rss", "atom", "feed") ||
    (metodo %in% c("automatico", "auto") && (
      grepl("(rss|atom)\\+xml", tipo_contenido) ||
        grepl("^\\s*<(rss|feed)([[:space:]>])", cuerpo_original, ignore.case = TRUE)
    ))

  if (es_json || es_csv) {
    if (identical(categoria, "precios")) {
      precios <- if (es_json) precios_de_json(cuerpo_original, fuente) else precios_de_csv(cuerpo_original, fuente)
      publicar(precios = precios)
      return(list(id = fuente$id, url = url, estado = if (length(precios)) "exportacion_revisada" else "exportacion_sin_datos", motivo = "", oportunidades = list(), precios = precios, revisadas = 1L))
    }
    oportunidades <- if (es_json) oportunidades_de_json(cuerpo_original, fuente) else oportunidades_de_csv(cuerpo_original, fuente)
    publicar(oportunidades = oportunidades)
    return(list(id = fuente$id, url = url, estado = if (length(oportunidades)) "exportacion_revisada" else "exportacion_sin_datos", motivo = "", oportunidades = oportunidades, precios = list(), revisadas = 1L))
  }

  if (es_feed || metodo %in% c("rss", "atom", "feed")) {
    documento_feed <- tryCatch(xml2::read_xml(cuerpo_original), error = function(e) NULL)
    if (is.null(documento_feed)) {
      return(list(id = fuente$id, url = url, estado = "feed_no_interpretable", motivo = "No se pudo interpretar la lista pública de publicaciones.", oportunidades = list(), precios = list(), revisadas = 1L))
    }
    oportunidades <- oportunidades_de_feed(documento_feed, url, fuente)
    publicar(oportunidades = oportunidades)
    return(list(id = fuente$id, url = url, estado = if (length(oportunidades)) "feed_revisado" else "feed_sin_publicaciones", motivo = "", oportunidades = oportunidades, precios = list(), revisadas = 1L))
  }

  if (metodo %in% c("mapa", "mapa_sitio", "sitemap")) {
    mapa <- tryCatch(xml2::read_xml(cuerpo_original), error = function(e) NULL)
    if (is.null(mapa)) {
      return(list(id = fuente$id, url = url, estado = "mapa_no_interpretable", motivo = "No se pudo interpretar el mapa del sitio.", oportunidades = list(), precios = list(), revisadas = 1L))
    }
    ubicaciones <- xml2::xml_text(xml2::xml_find_all(mapa, "//*[local-name()='loc']"))
    enlaces <- vapply(ubicaciones, url_absoluta, character(1), base = url)
    enlaces <- unique(enlaces[vapply(enlaces, function(enlace) {
      nzchar(enlace) && mismo_dominio(enlace, url) && es_enlace_relevante(enlace, enlace, categoria) && permiso_url(enlace, reglas)
    }, logical(1))])
    oportunidades <- list()
    for (enlace in head(enlaces, PAGINAS_POR_FUENTE)) {
      detalle <- tryCatch(leer_detalle(enlace, reglas, fuente), error = function(e) list(omitida = TRUE))
      if (isTRUE(detalle$omitida)) next
      hallazgos <- c(if (!is.null(detalle$oportunidad)) list(detalle$oportunidad), detalle$oportunidades_tabla)
      if (length(hallazgos)) {
        oportunidades <- c(oportunidades, hallazgos)
        publicar(oportunidades = hallazgos)
      }
    }
    return(list(id = fuente$id, url = url, estado = if (length(oportunidades)) "mapa_revisado" else "mapa_sin_coincidencias", motivo = "", oportunidades = oportunidades, precios = list(), revisadas = min(length(enlaces), PAGINAS_POR_FUENTE)))
  }

  documento <- tryCatch(xml2::read_html(cuerpo_original), error = function(e) NULL)
  if (is.null(documento)) {
    return(list(id = fuente$id, url = url, estado = "html_no_interpretable", motivo = "No se pudo interpretar la pagina como HTML.", oportunidades = list(), precios = list(), revisadas = 0L))
  }
  cuerpo <- texto_pagina(documento)
  barrera <- detectar_barrera(cuerpo, documento)
  if (nzchar(barrera)) {
    return(list(id = fuente$id, url = url, estado = "omitida", motivo = barrera, oportunidades = list(), precios = list(), revisadas = 0L))
  }

  oportunidades <- oportunidades_de_tabla(documento, url, fuente)
  precios <- if (identical(categoria, "precios")) extraer_productos(documento, fuente, url) else list()
  publicar(oportunidades, precios)
  if (identical(categoria, "precios")) {
    return(list(id = fuente$id, url = url, estado = if (length(precios)) "revisada" else "sin_datos_estructurados", motivo = "", oportunidades = list(), precios = precios, revisadas = 1L))
  }

  enlaces <- head(enlaces_html(documento, url, categoria), PAGINAS_POR_FUENTE)
  revisadas <- 1L

  if (metodo %in% c("automatico", "auto")) {
    for (feed_url in head(descubrir_feeds(documento, url), 2L)) {
      if (!permiso_url(feed_url, reglas)) next
      respuesta_feed <- obtener(feed_url, "application/rss+xml,application/atom+xml,application/xml,text/xml,*/*")
      if (is.null(respuesta_feed) || codigo_respuesta(respuesta_feed) < 200L || codigo_respuesta(respuesta_feed) >= 300L) next
      documento_feed <- tryCatch(xml2::read_xml(texto_respuesta(respuesta_feed)), error = function(e) NULL)
      if (!is.null(documento_feed)) {
        hallazgos <- oportunidades_de_feed(documento_feed, feed_url, fuente)
        oportunidades <- c(oportunidades, hallazgos)
        publicar(oportunidades = hallazgos)
      }
      revisadas <- revisadas + 1L
    }
  }

  # El mapa del sitio es un complemento cuando el listado ofrece pocos enlaces;
  # evita repetir solicitudes costosas en portales con tablas ya completas.
  if (length(enlaces) < min(3L, PAGINAS_POR_FUENTE)) {
    enlaces <- unique(c(enlaces, enlaces_mapa(url, reglas, categoria)))
  }
  enlaces <- head(enlaces, PAGINAS_POR_FUENTE)

  for (enlace in enlaces) {
    if (!permiso_url(enlace, reglas)) next
    detalle <- tryCatch(leer_detalle(enlace, reglas, fuente), error = function(e) {
      list(omitida = TRUE, motivo = conditionMessage(e))
    })
    revisadas <- revisadas + 1L
    if (isTRUE(detalle$omitida)) next
    hallazgos <- c(if (!is.null(detalle$oportunidad)) list(detalle$oportunidad), detalle$oportunidades_tabla)
    oportunidades <- c(oportunidades, hallazgos)
    precios <- c(precios, detalle$precios)
    if (length(hallazgos) || length(detalle$precios)) publicar(hallazgos, detalle$precios)
  }

  list(
    id = fuente$id,
    url = url,
    estado = if (length(oportunidades) || length(precios)) "revisada_con_datos" else "revisada_sin_datos",
    motivo = if (!length(enlaces)) "No se detectaron enlaces compatibles; no se generaron resultados." else "",
    oportunidades = oportunidades,
    precios = precios,
    revisadas = revisadas
  )
}

mezclar_oportunidades <- function(anteriores, nuevas) {
  por_url <- list()
  for (item in c(anteriores, nuevas)) {
    if (!is.list(item)) next
    url <- sub("#.*$", "", as.character(item$url %||% ""))
    if (!nzchar(url)) next
    if (!is.null(por_url[[url]])) {
      previo <- por_url[[url]]
      for (campo in names(item)) {
        valor <- item[[campo]]
        if (!is.null(valor) && length(valor) && !(is.character(valor) && !nzchar(valor[[1L]]))) previo[[campo]] <- valor
      }
      por_url[[url]] <- previo
    } else {
      por_url[[url]] <- item
    }
  }
  unname(por_url)
}

mezclar_precios <- function(anteriores, nuevos) {
  observaciones <- c(anteriores, nuevos)
  if (!length(observaciones)) return(list())
  claves <- vapply(observaciones, function(item) paste(item$fuente_id, item$fuente_url, item$producto, item$observado_en, item$precio, sep = "|"), character(1))
  unname(observaciones[!duplicated(claves)])
}

verificar_publicacion <- function(item, fuentes) {
  fuente <- Filter(function(x) identical(as.character(x$id), as.character(item$fuente_id)), fuentes)
  if (!length(fuente) || !url_https_valida(item$url %||% "")) return(list(estado = "por_verificar", confirmado = FALSE))
  fuente <- fuente[[1L]]
  reglas <- revisar_robots(item$url)
  if (!isTRUE(reglas$permitido) || !permiso_url(item$url, reglas)) return(list(estado = "por_verificar", confirmado = FALSE))
  respuesta <- obtener(item$url)
  codigo <- codigo_respuesta(respuesta)
  if (codigo %in% c(404L, 410L)) return(list(estado = "cerrada", confirmado = TRUE, motivo = paste("HTTP", codigo)))
  if (is.null(respuesta) || codigo < 200L || codigo >= 400L || !mismo_dominio(item$url, respuesta$url %||% item$url)) {
    return(list(estado = "por_verificar", confirmado = FALSE))
  }
  documento <- tryCatch(xml2::read_html(texto_respuesta(respuesta)), error = function(e) NULL)
  if (is.null(documento)) return(list(estado = "por_verificar", confirmado = FALSE))
  cuerpo <- texto_pagina(documento)
  if (nzchar(detectar_barrera(cuerpo, documento))) return(list(estado = "por_verificar", confirmado = FALSE))
  titulo <- normalizar_texto(titulo_pagina(documento))
  esperado <- normalizar_texto(item$titulo)
  coincide <- nzchar(titulo) && nzchar(esperado) && (grepl(esperado, titulo, fixed = TRUE) || grepl(titulo, esperado, fixed = TRUE))
  cerrado <- grepl("convocatoria cerrada|plazo vencido|ya no se aceptan postulaciones|proceso concluido|oferta concluida|proceso finalizado", normalizar_texto(cuerpo))
  if (coincide && cerrado) return(list(estado = "cerrada", confirmado = TRUE, motivo = "El portal informa el cierre de la publicacion."))
  if (coincide) return(list(estado = "abierta", confirmado = TRUE))
  list(estado = "por_verificar", confirmado = FALSE)
}

conservar_catalogo <- function(catalogo) {
  hoy <- Sys.Date()
  oportunidades <- lapply(catalogo$oportunidades %||% list(), function(item) {
    if (identical(item$categoria, "normativa")) return(item)
    verificado <- suppressWarnings(as.POSIXct(item$verificado_en %||% item$extraido_en %||% "", tz = "UTC"))
    antiguedad_dias <- if (length(verificado) == 1L && !is.na(verificado)) {
      as.numeric(difftime(Sys.time(), verificado, units = "days"))
    } else Inf
    if (antiguedad_dias >= 7 && texto_seguro(item$estado) %in% c("abierta", "por_verificar")) {
      item$descripcion <- ""
      item$requisitos <- ""
      item$estado <- "por_verificar"
    }
    if (identical(texto_seguro(item$estado), "cerrada")) {
      marca <- suppressWarnings(as.Date(substr(item$verificado_en %||% "", 1L, 10L)))
      if (!is.na(marca) && marca < hoy - 7) return(NULL)
    }
    item
  })
  oportunidades <- Filter(Negate(is.null), oportunidades)
  precios <- Filter(function(item) {
    fecha <- suppressWarnings(as.Date(item$observado_en %||% ""))
    !is.na(fecha) && fecha >= hoy - 365
  }, catalogo$observaciones_precios %||% list())
  catalogo$oportunidades <- oportunidades
  catalogo$observaciones_precios <- precios
  catalogo
}

escanear_noticias_bloqueos <- function(archivo_fuentes = ARCHIVO_FUENTES, max_por_medio = 40L) {
  registro <- leer_json(archivo_fuentes, list())
  fuentes <- Filter(function(x) is.list(x) && isTRUE(x$habilitada), registro$fuentes_noticias %||% list())
  palabras <- "bloque|bloquo|paro(s)?|movilizad|movilizacion(es)?"
  hallazgos <- list(); informes <- list()
  for (fuente in fuentes) {
    url <- texto_seguro(fuente$feed_url, fuente$url)
    reglas <- tryCatch(revisar_robots(url), error = function(e) list(permitido = FALSE, estado = "no_verificable"))
    if (!isTRUE(reglas$permitido)) {
      informes[[length(informes) + 1L]] <- list(medio = fuente$nombre, url = url, estado = "no_verificado", detalle = "No se pudieron verificar las reglas públicas del feed.")
      next
    }
    respuesta <- tryCatch(obtener(url, "application/rss+xml,application/atom+xml,application/xml,text/xml,*/*"), error = function(e) NULL)
    if (is.null(respuesta) || codigo_respuesta(respuesta) < 200L || codigo_respuesta(respuesta) >= 300L) {
      informes[[length(informes) + 1L]] <- list(medio = fuente$nombre, url = url, estado = "sin_respuesta")
      next
    }
    documento <- tryCatch(xml2::read_xml(texto_respuesta(respuesta)), error = function(e) NULL)
    if (is.null(documento)) {
      informes[[length(informes) + 1L]] <- list(medio = fuente$nombre, url = url, estado = "feed_no_interpretable")
      next
    }
    entradas <- head(xml2::xml_find_all(documento, "//*[local-name()='item' or local-name()='entry']"), max_por_medio)
    leidas <- 0L
    for (entrada in entradas) {
      titulo <- texto_seguro(xml2::xml_text(xml2::xml_find_first(entrada, "./*[local-name()='title']")))
      descripcion <- texto_seguro(xml2::xml_text(xml2::xml_find_first(entrada, "./*[local-name()='description' or local-name()='summary' or local-name()='content']")))
      plano <- normalizar_texto(paste(titulo, descripcion))
      if (!grepl(palabras, plano, perl = TRUE)) next
      nodo_link <- xml2::xml_find_first(entrada, "./*[local-name()='link']")
      href <- texto_seguro(xml2::xml_attr(nodo_link, "href"))
      if (!nzchar(href)) href <- xml2::xml_text(nodo_link)
      enlace <- url_absoluta(href, url)
      fecha_txt <- texto_seguro(xml2::xml_text(xml2::xml_find_first(entrada, "./*[local-name()='pubDate' or local-name()='published' or local-name()='updated' or local-name()='date']")))
      fecha <- suppressWarnings(as.POSIXct(fecha_txt, tz = "UTC"))
      if (length(fecha) == 1L && !is.na(fecha) && as.numeric(difftime(Sys.time(), fecha, units = "hours")) > 72) next
      hallazgos[[length(hallazgos) + 1L]] <- list(medio = texto_seguro(fuente$nombre), titulo = limpiar_texto(titulo, 240L), resumen = limpiar_texto(descripcion, 700L), url = enlace, publicado_en = fecha_txt, palabras = regmatches(plano, gregexpr(palabras, plano, perl = TRUE))[[1L]])
      leidas <- leidas + 1L
    }
    informes[[length(informes) + 1L]] <- list(medio = fuente$nombre, url = url, estado = "consultado", coincidencias = leidas)
  }
  if (length(hallazgos)) {
    urls <- vapply(hallazgos, function(x) x$url, character(1))
    hallazgos <- hallazgos[!duplicated(urls)]
  }
  list(consultado_en = as.character(Sys.time()), ventana_horas = 72L, hallazgos = hallazgos, fuentes = informes)
}

ejecutar_recoleccion_diaria <- function(fuentes_archivo = ARCHIVO_FUENTES, catalogo_archivo = ARCHIVO_CATALOGO) {
  ARCHIVO_FUENTES <<- fuentes_archivo
  ARCHIVO_CATALOGO <<- catalogo_archivo
  rm(list = ls(envir = ESPERAS_DOMINIO, all.names = TRUE), envir = ESPERAS_DOMINIO)
  rm(list = ls(envir = REGLAS_ROBOTS, all.names = TRUE), envir = REGLAS_ROBOTS)
  rm(list = ls(envir = ESPERAS_ROBOTS, all.names = TRUE), envir = ESPERAS_ROBOTS)
  inicio <- Sys.time()
  anterior <- leer_json(ARCHIVO_CATALOGO, list(oportunidades = list(), observaciones_precios = list(), fuentes = list()))
  fuentes <- leer_fuentes()
  if (length(fuentes)) {
    historial <- anterior$fuentes %||% list()
    registro_anterior <- lapply(fuentes, function(fuente) {
      coincidencia <- Filter(function(item) identical(as.character(item$id %||% ""), as.character(fuente$id %||% "")), historial)
      if (length(coincidencia)) coincidencia[[length(coincidencia)]] else list()
    })
    categoria_coincide <- vapply(fuentes, function(fuente) {
      nzchar(CATEGORIA_PRIORITARIA) && identical(as.character(fuente$categoria), CATEGORIA_PRIORITARIA)
    }, logical(1))
    fuente_no_oficial <- !vapply(fuentes, function(fuente) isTRUE(fuente$oficial), logical(1))
    fuente_con_fallo <- vapply(registro_anterior, function(item) {
      texto_seguro(item$estado) %in% c("error", "no_disponible", "omitida", "redireccion_externa", "html_no_interpretable", "feed_no_interpretable")
    }, logical(1))
    rendimiento_previo <- vapply(registro_anterior, function(item) {
      as.numeric(item$oportunidades_encontradas %||% 0) + 0.5 * as.numeric(item$precios_observados %||% 0)
    }, numeric(1))
    fuentes <- fuentes[order(!categoria_coincide, fuente_no_oficial, fuente_con_fallo, -rendimiento_previo, seq_along(fuentes))]

    # Dentro de cada nivel de prioridad, rota por host para que un dominio con
    # varias rutas no monopolice el comienzo del ciclo.
    alternar_por_host <- function(indices) {
      pendientes <- indices
      salida <- integer()
      hosts <- vapply(fuentes[indices], function(fuente) partes_url(as.character(fuente$url))$host, character(1))
      names(hosts) <- indices
      while (length(pendientes)) {
        hosts_pendientes <- unique(hosts[as.character(pendientes)])
        siguientes <- vapply(hosts_pendientes, function(host) {
          pendientes[which(hosts[as.character(pendientes)] == host)[1L]]
        }, integer(1))
        salida <- c(salida, siguientes)
        pendientes <- pendientes[!pendientes %in% siguientes]
      }
      salida
    }
    nivel <- paste(categoria_coincide, fuente_no_oficial, sep = "|")
    niveles <- c("TRUE|FALSE", "TRUE|TRUE", "FALSE|FALSE", "FALSE|TRUE")
    orden_rotado <- unlist(lapply(niveles, function(valor) alternar_por_host(which(nivel == valor))), use.names = FALSE)
    if (length(orden_rotado) == length(fuentes)) fuentes <- fuentes[orden_rotado]
  }
  fuentes_fuera_de_consulta <- list()
  if (MODO_CONSULTA && length(fuentes)) {
    palabras_vacias <- c("de", "del", "la", "las", "el", "los", "en", "y", "o", "para", "por", "con", "un", "una", "que", "hay", "mas", "me", "mi", "quiero", "busco", "necesito", "como", "cuales", "cual", "antes", "fin", "mes")
    terminos <- unique(strsplit(gsub("[^a-z0-9 ]", " ", normalizar_texto(CONSULTA_ENLINEA)), " +")[[1L]])
    terminos <- terminos[nchar(terminos) >= 3L & !terminos %in% palabras_vacias]
    puntaje_fuente <- vapply(fuentes, function(fuente) {
      texto <- normalizar_texto(paste(fuente$nombre, fuente$categoria, fuente$url, fuente$nota))
      coincidencias <- sum(vapply(terminos, function(t) grepl(t, texto, fixed = TRUE), logical(1)))
      coincidencias * 3 +
        (if (nzchar(CATEGORIA_PRIORITARIA) && identical(as.character(fuente$categoria), CATEGORIA_PRIORITARIA)) 4 else 0) +
        (if (isTRUE(fuente$oficial)) 1 else 0)
    }, numeric(1))
    orden <- order(-puntaje_fuente, seq_along(fuentes))
    elegidas <- head(orden[puntaje_fuente[orden] > 0], MAX_FUENTES_CONSULTA)
    if (!length(elegidas)) elegidas <- head(orden, min(3L, length(orden)))
    fuentes_fuera_de_consulta <- fuentes[setdiff(seq_along(fuentes), elegidas)]
    fuentes <- fuentes[elegidas]
    registrar("Consulta en línea: ", length(fuentes), " fuentes seleccionadas.")
  }
  anteriores <- anterior$oportunidades %||% list()
  nuevas <- list()
  nuevos_precios <- list()
  informes <- list()
  fuentes_completadas <- 0L
  fuente_actual <- "Preparando la revisión de fuentes"
  solicitudes_http <- 0L

  informar_progreso <- function(completadas, fuente = "") {
    if (exists("actualizar_progreso_recoleccion", mode = "function", inherits = TRUE)) {
      try(actualizar_progreso_recoleccion(total_fuentes_ciclo, completadas, fuente, solicitudes_http), silent = TRUE)
    }
  }
  informar_solicitud <- function(url) {
    solicitudes_http <<- solicitudes_http + 1L
    informar_progreso(fuentes_completadas, fuente_actual)
  }
  assign("informar_solicitud_recoleccion", informar_solicitud, envir = .GlobalEnv)
  resumir_fuente <- function(item) list(
    id = item$id,
    url = item$url,
    estado = item$estado,
    motivo = item$motivo %||% "",
    revisadas = as.integer(item$revisadas %||% 0L),
    oportunidades_encontradas = as.integer(item$oportunidades_encontradas %||% length(item$oportunidades %||% list())),
    precios_observados = as.integer(item$precios_observados %||% length(item$precios %||% list())),
    solicitudes_http = as.integer(item$solicitudes_http %||% 0L),
    duracion_segundos = round(as.numeric(item$duracion_segundos %||% 0), 2),
    revisado_en = as.character(item$revisado_en %||% Sys.time())
  )
  guardar_avance <- function(fuente, resultado = NULL) {
    fuentes_parciales <- lapply(informes, resumir_fuente)
    if (!is.null(resultado)) fuentes_parciales <- c(fuentes_parciales, list(resumir_fuente(resultado)))
    parcial <- conservar_catalogo(list(
      oportunidades = mezclar_oportunidades(anteriores, nuevas),
      observaciones_precios = mezclar_precios(anterior$observaciones_precios %||% list(), nuevos_precios),
      fuentes = fuentes_parciales,
      actualizado_en = as.character(Sys.time()),
      ejecucion_iniciada = as.character(inicio)
    ))
    guardar_json(parcial, ARCHIVO_CATALOGO)
    invisible(parcial)
  }
  # Evita insistir con barreras confirmadas; un robots.txt no verificable
  # vuelve a probarse antes porque puede tratarse de una caída transitoria.
  barreras_persistentes <- c("requiere_javascript", "requiere_autenticacion", "bloqueada_por_el_portal", "robots.txt bloqueado")
  total_fuentes_ciclo <- length(fuentes)
  id_barrera_reciente <- vapply(fuentes, function(fuente) {
    previos <- Filter(function(item) identical(as.character(item$id %||% ""), as.character(fuente$id %||% "")), anterior$fuentes %||% list())
    if (!length(previos)) return(FALSE)
    previo <- previos[[length(previos)]]
    fecha <- suppressWarnings(as.POSIXct(previo$revisado_en %||% "", tz = "UTC"))
    texto <- paste(texto_seguro(previo$estado), texto_seguro(previo$motivo))
    barrera_confirmada <- any(vapply(barreras_persistentes, grepl, logical(1), x = texto, fixed = TRUE))
    robots_no_verificable <- grepl("robots.txt no_verificable", texto, fixed = TRUE)
    espera <- if (barrera_confirmada) 7 * 24 * 60 * 60 else if (robots_no_verificable) 6 * 60 * 60 else 0
    texto_seguro(previo$estado) %in% c("omitida", "pospuesta_por_barrera") && espera > 0 &&
      length(fecha) == 1L && !is.na(fecha) && as.numeric(difftime(Sys.time(), fecha, units = "secs")) < espera
  }, logical(1))
  pospuestas <- fuentes[id_barrera_reciente]
  fuentes <- fuentes[!id_barrera_reciente]
  fuentes_completadas <- length(pospuestas)
  informar_progreso(length(pospuestas))

  for (indice_fuente in seq_along(fuentes)) {
    fuente <- fuentes[[indice_fuente]]
    fuentes_completadas <- length(pospuestas) + indice_fuente - 1L
    fuente_actual <- texto_seguro(fuente$nombre, fuente$url)
    informar_progreso(fuentes_completadas, fuente_actual)
    solicitudes_inicio <- solicitudes_http
    inicio_fuente <- Sys.time()
    publicar <- function(oportunidades = list(), precios = list()) {
      nuevas <<- c(nuevas, oportunidades)
      nuevos_precios <<- c(nuevos_precios, precios)
      if (length(oportunidades) || length(precios)) guardar_avance(fuente)
      invisible(NULL)
    }
    informe <- tryCatch(recopilar_fuente(fuente, publicar), error = function(e) {
      list(id = fuente$id, url = fuente$url, estado = "error", motivo = conditionMessage(e), oportunidades = list(), precios = list(), revisadas = 0L)
    })
    informe$revisado_en <- as.character(Sys.time())
    informe$oportunidades_encontradas <- as.integer(length(informe$oportunidades %||% list()))
    informe$precios_observados <- as.integer(length(informe$precios %||% list()))
    informe$solicitudes_http <- as.integer(solicitudes_http - solicitudes_inicio)
    informe$duracion_segundos <- as.numeric(difftime(Sys.time(), inicio_fuente, units = "secs"))
    informes[[length(informes) + 1L]] <- informe
    nuevas <- c(nuevas, informe$oportunidades %||% list())
    nuevos_precios <- c(nuevos_precios, informe$precios %||% list())
    if (informe$estado %in% c("error", "no_disponible", "html_no_interpretable", "feed_no_interpretable", "mapa_no_interpretable")) {
      registrar("Fallo en ", fuente$nombre %||% fuente$url, ": ", informe$motivo %||% informe$estado, nivel = "ERROR")
    }
    registrar("Resultado ", fuente$id, ": ", informe$estado, "; oportunidades: ", length(informe$oportunidades %||% list()))
    guardar_avance(fuente, informe)
    fuentes_completadas <- length(pospuestas) + indice_fuente
    fuente_actual <- if (indice_fuente < length(fuentes)) texto_seguro(fuentes[[indice_fuente + 1L]]$nombre, fuentes[[indice_fuente + 1L]]$url) else if (length(pospuestas)) paste("Omitiendo", length(pospuestas), "fuentes con barreras recientes; guardando el catálogo") else "Guardando el catálogo"
    informar_progreso(fuentes_completadas, fuente_actual)
  }

  oportunidades <- mezclar_oportunidades(anteriores, nuevas)
  claves_nuevas <- unique(vapply(nuevas, function(item) as.character(item$url %||% ""), character(1)))
  pendientes <- which(!vapply(oportunidades, function(item) {
    as.character(item$url %||% "") %in% claves_nuevas || identical(item$categoria, "normativa") ||
      identical(texto_seguro(item$estado), "cerrada")
  }, logical(1)))
  if (length(pendientes) && !MODO_CONSULTA) {
    pendientes <- head(pendientes, 40L)
    for (indice in pendientes) {
      item <- oportunidades[[indice]]
      limite <- suppressWarnings(as.Date(item$fecha_limite %||% ""))
      verificacion <- tryCatch(verificar_publicacion(item, fuentes), error = function(e) list(estado = "por_verificar", confirmado = FALSE))
      item$estado <- verificacion$estado
      if (identical(item$estado, "abierta") && !is.na(limite) && limite < Sys.Date()) item$estado <- "vencida"
      if (isTRUE(verificacion$confirmado)) item$verificado_en <- as.character(Sys.time())
      if (identical(item$estado, "cerrada")) item$motivo_cierre <- verificacion$motivo %||% "Cierre confirmado en la fuente."
      oportunidades[[indice]] <- item
    }
  }

  observaciones <- mezclar_precios(anterior$observaciones_precios %||% list(), nuevos_precios)
  noticias_bloqueos <- tryCatch(escanear_noticias_bloqueos(fuentes_archivo), error = function(e) {
    registrar("No se pudo consultar los feeds de noticias: ", conditionMessage(e), nivel = "ERROR")
    anterior$alertas_bloqueos %||% list(consultado_en = "", ventana_horas = 72L, hallazgos = list(), fuentes = list())
  })

  resultado <- conservar_catalogo(list(
    oportunidades = oportunidades,
    observaciones_precios = observaciones,
    alertas_bloqueos = noticias_bloqueos,
    fuentes = lapply(c(informes, lapply(fuentes_fuera_de_consulta, function(fuente) {
      previos <- Filter(function(item) identical(as.character(item$id %||% ""), as.character(fuente$id %||% "")), anterior$fuentes %||% list())
      if (length(previos)) previos[[length(previos)]] else list(id = fuente$id, url = fuente$url, estado = "sin_revisar")
    }), lapply(pospuestas, function(fuente) {
      previos <- Filter(function(item) identical(as.character(item$id %||% ""), as.character(fuente$id %||% "")), anterior$fuentes %||% list())
      previo <- if (length(previos)) previos[[length(previos)]] else list()
      c(previo, list(id = fuente$id, url = fuente$url, estado = "pospuesta_por_barrera", motivo = texto_seguro(previo$motivo), revisado_en = texto_seguro(previo$revisado_en)))
    })), resumir_fuente),
    actualizado_en = as.character(Sys.time()),
    ejecucion_iniciada = as.character(inicio),
    ejecucion_finalizada = as.character(Sys.time())
  ))
  guardar_json(resultado, ARCHIVO_CATALOGO)
  registrar("Ciclo completo. Oportunidades vigentes: ", sum(vapply(resultado$oportunidades, function(item) identical(item$estado, "abierta"), logical(1))), "; observaciones de precios: ", length(resultado$observaciones_precios))
  invisible(resultado)
}

run_daily_collection <- ejecutar_recoleccion_diaria

if (sys.nframe() == 0L) ejecutar_recoleccion_diaria()
