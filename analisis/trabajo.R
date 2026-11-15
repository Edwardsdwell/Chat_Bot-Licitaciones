# trabajo.R — worker robusto para análisis SEO/rastreo de La Pala
# Uso: Rscript trabajo.R <tipo> <id> <url(s)> [max_paginas]
# Tipos admitidos: sitio, rastreo
#
# Contrato de estado:
#   logs/trabajos/<id>.json
#   estado = "en_curso" | "completado" | "fallido"
#   resultado = resultado del análisis
#   error = mensaje seguro si falla

if (.Platform$OS.type == "windows" && identical(Sys.getlocale("LC_CTYPE"), "C")) {
  suppressWarnings(Sys.setlocale("LC_CTYPE", "English_United States.utf8"))
}

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0L || all(is.na(x))) y else x
}

argumentos <- commandArgs(trailingOnly = TRUE)
estado <- "fallido"
mensaje_error <- NULL
resultado <- NULL
tipo <- "trabajo"
id <- if (length(argumentos) >= 2L) as.character(argumentos[[2L]]) else ""

es_id_valido <- function(x) {
  length(x) == 1L && !is.na(x) && nzchar(x) &&
    grepl("^[A-Za-z0-9_-]{1,100}$", x)
}

escribir_estado <- function(ruta, objeto) {
  temporal <- paste0(ruta, ".tmp")
  jsonlite::write_json(
    objeto, temporal, auto_unbox = TRUE, null = "null", na = "null",
    pretty = FALSE, dataframe = "rows", digits = NA
  )
  if (!file.exists(temporal)) stop("No se pudo crear el archivo temporal de estado.")
  ok <- file.rename(temporal, ruta)
  if (!isTRUE(ok)) {
    # En Windows rename puede fallar si el destino ya existe.
    if (file.exists(ruta)) unlink(ruta)
    ok <- file.rename(temporal, ruta)
  }
  if (!isTRUE(ok)) stop("No se pudo publicar el estado del trabajo.")
  invisible(TRUE)
}

raiz <- normalizePath(
  Sys.getenv("LA_PALA_ROOT", unset = getwd()),
  winslash = "/", mustWork = TRUE
)
setwd(raiz)

if (!es_id_valido(id)) {
  stop("Identificador de trabajo ausente o inválido. Use solo letras, números, guion y guion bajo.")
}

directorio_logs <- Sys.getenv("LA_PALA_LOG_DIR", file.path(raiz, "logs"))
dir_trabajos <- file.path(directorio_logs, "trabajos")
dir.create(dir_trabajos, recursive = TRUE, showWarnings = FALSE)
ruta_estado <- file.path(dir_trabajos, paste0(id, ".json"))
ruta_lock <- file.path(dir_trabajos, paste0(id, ".lock"))

lock_adquirido <- FALSE
tryCatch({
  if (length(argumentos) < 3L) stop("Faltan argumentos: tipo, id y URL.")
  tipo <- tolower(trimws(argumentos[[1L]]))
  urls <- trimws(unlist(strsplit(argumentos[[3L]], ",", fixed = TRUE)))
  urls <- urls[nzchar(urls)]
  if (!length(urls)) stop("No se proporcionó ninguna URL.")
  if (tipo == "sitio" && length(urls) < 1L) stop("El análisis de sitios requiere al menos una URL.")
  if (!tipo %in% c("sitio", "rastreo")) stop("Tipo de trabajo no admitido.")
  if (any(!grepl("^https://", urls, ignore.case = TRUE))) {
    stop("Solo se admiten URL HTTPS.")
  }

  # Crear el lock con exclusión simple. La API debe impedir duplicados del mismo ID.
  # dir.create() es atómico: solo un worker puede adquirir este ID.
  if (file.exists(ruta_lock) || !isTRUE(dir.create(ruta_lock, showWarnings = FALSE))) {
    stop("El trabajo ya está bloqueado o en ejecución.")
  }
  lock_adquirido <<- TRUE

  escribir_estado(ruta_estado, list(
    estado = "en_curso", id = id, tipo = tipo,
    iniciado = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
    resultado = NULL, error = NULL
  ))

  # Cargar módulos después de validar argumentos.
  archivo_motor <- file.path(raiz, "analisis", "motor.R")
  if (!file.exists(archivo_motor)) stop("No se encuentra analisis/motor.R.")
  source(archivo_motor, local = .GlobalEnv, encoding = "UTF-8")

  if (tipo == "sitio") {
    resultado <- comparar_sitios(urls)
  } else {
    max_paginas <- if (length(argumentos) >= 4L) {
      suppressWarnings(as.integer(argumentos[[4L]]))
    } else 8L
    if (is.na(max_paginas)) max_paginas <- 8L
    max_paginas <- max(2L, min(12L, max_paginas))
    resultado <- rastrear(urls[[1L]], max_paginas)
  }

  if (is.null(resultado)) stop("El análisis terminó sin devolver resultados.")
  estado <- "completado"
}, error = function(e) {
  mensaje_error <<- conditionMessage(e)
  estado <<- "fallido"
  ruta_fallas <- file.path(directorio_logs, "fallas.txt")
  dir.create(dirname(ruta_fallas), recursive = TRUE, showWarnings = FALSE)
  linea <- sprintf("%s [ERROR] analisis/%s (%s): %s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"), tipo %||% "trabajo", id, gsub("[\\r\\n\\t]+", " ", mensaje_error))
  try(cat(linea, file = ruta_fallas, append = TRUE), silent = TRUE)
})

objeto_final <- list(
  estado = estado,
  id = id,
  finalizado = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
  resultado = if (identical(estado, "completado")) resultado else NULL,
  error = if (identical(estado, "fallido")) mensaje_error else NULL
)

tryCatch(
  escribir_estado(ruta_estado, objeto_final),
  error = function(e) {
    message("ERROR CRÍTICO: no se pudo escribir el estado: ", conditionMessage(e))
    quit(save = "no", status = 2L)
  }
)

if (lock_adquirido && file.exists(ruta_lock)) unlink(ruta_lock, recursive = TRUE)

if (identical(estado, "fallido")) {
  message("Trabajo fallido: ", mensaje_error)
  quit(save = "no", status = 1L)
}
message("Trabajo completado: ", id)
