# Ejecuta una recolección manual
if (.Platform$OS.type == "windows" && identical(Sys.getlocale("LC_CTYPE"), "C")) {
  suppressWarnings(Sys.setlocale("LC_CTYPE", "English_United States.utf8"))
}

argumentos <- commandArgs(trailingOnly = FALSE)
archivo_ejecutado <- grep("^--file=", argumentos, value = TRUE)
rutas_iniciales <- getwd()
if (length(archivo_ejecutado)) {rutas_iniciales <- c(rutas_iniciales, dirname(sub("^--file=", "", archivo_ejecutado[[1L]])))}
archivos_fuente <- vapply(sys.frames(), function(marco) {
  ruta <- marco$ofile
  if (is.null(ruta) || !length(ruta)) return("")
  as.character(ruta[[1L]])
}, character(1))
if (length(archivos_fuente)) {rutas_iniciales <- c(rutas_iniciales, dirname(archivos_fuente[nzchar(archivos_fuente)]))}
if (requireNamespace("rstudioapi", quietly = TRUE) &&
    isTRUE(tryCatch(rstudioapi::isAvailable(), error = function(error) FALSE))) {
  archivo_activo <- tryCatch(rstudioapi::getActiveDocumentContext()$path, error = function(error) "")
  if (length(archivo_activo) && !is.na(archivo_activo) && nzchar(archivo_activo)) {
    rutas_iniciales <- c(dirname(archivo_activo), rutas_iniciales)}}
raices_candidatas <- character()
for (ruta in unique(rutas_iniciales[nzchar(rutas_iniciales)])) {
  actual <- normalizePath(ruta, winslash = "/", mustWork = FALSE)
  repeat {
    raices_candidatas <- c(raices_candidatas, actual)
    padre <- dirname(actual)
    if (identical(padre, actual)) break
    actual <- padre}}

raices_candidatas <- unique(raices_candidatas)
es_raiz <- vapply(raices_candidatas, function(ruta) {
  file.exists(file.path(ruta, "scraping", "collect.R")) &&
    file.exists(file.path(ruta, "scraping", "sources.json"))
}, logical(1))

if (!any(es_raiz)) {
  stop("No encuentro La Pala. Abre el proyecto en RStudio o ejecuta la recolección desde su carpeta.", call. = FALSE)
}

raiz <- normalizePath(raices_candidatas[which(es_raiz)[1L]], winslash = "/", mustWork = TRUE)
setwd(raiz)

directorio_logs <- Sys.getenv("LA_PALA_LOG_DIR", file.path(raiz, "logs"))
dir.create(directorio_logs, recursive = TRUE, showWarnings = FALSE)
archivo_estado <- file.path(directorio_logs, "estado_recoleccion.json")
archivo_fallas <- file.path(directorio_logs, "fallas.txt")
archivo_bloqueo <- file.path(directorio_logs, "recoleccion.lock")
archivo_encolado <- file.path(directorio_logs, "recoleccion.encolada")
solicitud_id <- Sys.getenv("LA_PALA_SOLICITUD_ID", unset = "")
if (!nzchar(solicitud_id)) {
  solicitud_id <- paste0("manual-", format(Sys.time(), "%Y%m%d%H%M%S"), "-", Sys.getpid())
}

guardar_estado_recoleccion <- function(estado, mensaje, resumen = list()) {
  temporal <- tempfile(pattern = ".estado-recoleccion-", tmpdir = dirname(archivo_estado))
  on.exit(if (file.exists(temporal)) unlink(temporal), add = TRUE)
  jsonlite::write_json(list(
    estado = estado,
    solicitud_id = solicitud_id,
    modo = if (identical(Sys.getenv("LA_PALA_MODO", unset = ""), "consulta")) "consulta" else "diaria",
    mensaje = mensaje,
    actualizado_en = as.character(Sys.time()),
    resumen = resumen
  ), temporal, auto_unbox = TRUE, pretty = TRUE, na = "null", null = "null")
  if (!file.rename(temporal, archivo_estado)) {
    respaldo <- paste0(archivo_estado, ".bak")
    if (file.exists(respaldo)) unlink(respaldo)
    habia <- file.exists(archivo_estado)
    if (habia && !file.rename(archivo_estado, respaldo)) stop("No se pudo respaldar el estado de recolección.")
    if (!file.rename(temporal, archivo_estado)) {
      if (habia && file.exists(respaldo)) file.rename(respaldo, archivo_estado)
      stop("No se pudo publicar el estado de recolección.")
    }
    if (file.exists(respaldo)) unlink(respaldo)
  }
  invisible(TRUE)
}

actualizar_progreso_recoleccion <- function(total, completadas, fuente = "", solicitudes = 0L) {
  porcentaje <- if (total > 0L) floor(completadas * 100 / total) else 0L
  guardar_estado_recoleccion("en_curso", if (nzchar(fuente)) paste0("Consultando: ", fuente) else "Preparando la revisión de fuentes.", list(
    total_fuentes = as.integer(total),
    fuentes_completadas = as.integer(completadas),
    porcentaje = as.integer(porcentaje),
    fuente_actual = fuente,
    solicitudes_http = as.integer(solicitudes)
  ))
}

ejecutar_con_estado <- function() {
  if (file.exists(archivo_bloqueo)) {
    antiguedad <- as.numeric(difftime(Sys.time(), file.info(archivo_bloqueo)$mtime, units = "hours"))
    if (is.finite(antiguedad) && antiguedad < 8) {
      stop("Ya hay una recolección en curso.", call. = FALSE)
    }
    unlink(archivo_bloqueo)
  }
  if (!isTRUE(dir.create(archivo_bloqueo, showWarnings = FALSE))) {
    stop("No se pudo reservar la recolección; puede existir otra instancia activa.", call. = FALSE)
  }
  if (file.exists(archivo_encolado)) unlink(archivo_encolado, recursive = TRUE)
  on.exit(unlink(archivo_bloqueo, recursive = TRUE), add = TRUE)

  guardar_estado_recoleccion("en_curso", "Preparando la revisión de fuentes.", list(total_fuentes = 0L, fuentes_completadas = 0L, porcentaje = 0L, fuente_actual = ""))
  fase <- "cargando el recolector"
  tryCatch({
    assign("actualizar_progreso_recoleccion", actualizar_progreso_recoleccion, envir = .GlobalEnv)
    source(file.path(raiz, "scraping", "collect.R"), local = .GlobalEnv)
    fase <- "consultando las fuentes"
    resultado <- ejecutar_recoleccion_diaria()
    fase <- "guardando el resumen"
    fuentes_con_hallazgos <- sum(vapply(resultado$fuentes, function(fuente) {
      as.integer(fuente$oportunidades_encontradas %||% 0L) > 0L || as.integer(fuente$precios_observados %||% 0L) > 0L
    }, logical(1)))
    resumen <- list(
      oportunidades_activas = sum(vapply(resultado$oportunidades, function(item) identical(item$estado, "abierta"), logical(1))),
      observaciones_precios = length(resultado$observaciones_precios),
      fuentes_revisadas = length(resultado$fuentes),
      fuentes_con_hallazgos = as.integer(fuentes_con_hallazgos),
      solicitudes_http = sum(vapply(resultado$fuentes, function(fuente) as.integer(fuente$solicitudes_http %||% 0L), integer(1))),
      duracion_segundos = round(sum(vapply(resultado$fuentes, function(fuente) as.numeric(fuente$duracion_segundos %||% 0), numeric(1))), 1),
      total_fuentes = length(resultado$fuentes),
      fuentes_completadas = length(resultado$fuentes),
      porcentaje = 100L,
      fuente_actual = "Catálogo actualizado"
    )
    guardar_estado_recoleccion("completada", "Terminó la recolección de las fuentes.", resumen)
    resultado
  }, error = function(error) {
    detalle <- gsub("[\\r\\n\\t]+", " ", conditionMessage(error))
    try(cat(sprintf("%s [ERROR] worker/%s: %s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"), fase, detalle),
            file = archivo_fallas, append = TRUE), silent = TRUE)
    guardar_estado_recoleccion("fallida", "No pude completar la actualización. Inténtalo de nuevo más tarde.", list(fase = fase))
    stop("No se completó la actualización.", call. = FALSE)})}
ejecutar_con_estado()
