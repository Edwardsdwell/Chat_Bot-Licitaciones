if (.Platform$OS.type == "windows" && identical(Sys.getlocale("LC_CTYPE"), "C")) {
  suppressWarnings(Sys.setlocale("LC_CTYPE", "English_United States.utf8"))
}

argumentos <- commandArgs(trailingOnly = FALSE)
archivo_ejecutado <- grep("^--file=", argumentos, value = TRUE)
rutas_iniciales <- getwd()

if (length(archivo_ejecutado)) {
  rutas_iniciales <- c(rutas_iniciales, dirname(sub("^--file=", "", archivo_ejecutado[[1L]])))
}

archivos_fuente <- vapply(sys.frames(), function(marco) {
  ruta <- marco$ofile
  if (is.null(ruta) || !length(ruta)) return("")
  as.character(ruta[[1L]])
}, character(1))

if (length(archivos_fuente)) {
  rutas_iniciales <- c(rutas_iniciales, dirname(archivos_fuente[nzchar(archivos_fuente)]))
}

if (requireNamespace("rstudioapi", quietly = TRUE) &&
    isTRUE(tryCatch(rstudioapi::isAvailable(), error = function(e) FALSE))) {
  archivo_activo <- tryCatch(rstudioapi::getActiveDocumentContext()$path, error = function(e) "")
  if (length(archivo_activo) && !is.na(archivo_activo) && nzchar(archivo_activo)) {
    rutas_iniciales <- c(dirname(archivo_activo), rutas_iniciales)
  }
}

raices_candidatas <- character()
for (ruta in unique(rutas_iniciales[nzchar(rutas_iniciales)])) {
  actual <- normalizePath(ruta, winslash = "/", mustWork = FALSE)
  repeat {
    raices_candidatas <- c(raices_candidatas, actual)
    padre <- dirname(actual)
    if (identical(padre, actual)) break
    actual <- padre
  }
}

es_raiz <- vapply(unique(raices_candidatas), function(ruta) {
  file.exists(file.path(ruta, "backend", "servidor.R")) &&
    file.exists(file.path(ruta, "scraping", "sources.json"))
}, logical(1))

if (!any(es_raiz)) {
  stop("No encuentro La Pala. Abre la carpeta del proyecto o ejecuta Rscript start.R desde ella.", call. = FALSE)
}

raiz <- unique(raices_candidatas)[which(es_raiz)[1L]]
raiz <- normalizePath(raiz, winslash = "/", mustWork = TRUE)
setwd(raiz)
directorio_datos <- Sys.getenv("LA_PALA_DATA_DIR", file.path(raiz, "data"))
directorio_logs <- Sys.getenv("LA_PALA_LOG_DIR", file.path(raiz, "logs"))
dir.create(directorio_datos, recursive = TRUE, showWarnings = FALSE)
dir.create(directorio_logs, recursive = TRUE, showWarnings = FALSE)
archivo_catalogo <- Sys.getenv("LA_PALA_CATALOG_FILE", file.path(directorio_datos, "catalogo.json"))
catalogo_inicial <- file.path(raiz, "data", "catalogo.json")
if (!file.exists(archivo_catalogo) && file.exists(catalogo_inicial)) {
  dir.create(dirname(archivo_catalogo), recursive = TRUE, showWarnings = FALSE)
  file.copy(catalogo_inicial, archivo_catalogo, overwrite = FALSE)
}

archivo_variables_privadas <- file.path(raiz, ".Renviron")
if (file.exists(archivo_variables_privadas)) readRenviron(archivo_variables_privadas)

# Algunas sesiones administradas definen R_LIBS_USER a una carpeta temporal.
# Recupera una biblioteca de usuario persistente para que la API y sus workers
# compartan los paquetes instalados entre procesos.
if (.Platform$OS.type == "windows") {
  version_biblioteca <- paste0(R.Version()$major, ".", strsplit(R.Version()$minor, "\\.")[[1L]][1L])
  candidatos_biblioteca <- c(
    Sys.getenv("R_LIBS_USER", unset = ""),
    file.path(Sys.getenv("LOCALAPPDATA", unset = ""), "R", "win-library", version_biblioteca),
    file.path(Sys.getenv("USERPROFILE", unset = ""), "Documents", "R", "win-library", version_biblioteca)
  )
  candidatos_biblioteca <- unique(candidatos_biblioteca[nzchar(candidatos_biblioteca) & dir.exists(candidatos_biblioteca)])
  if (length(candidatos_biblioteca)) {
    Sys.setenv(R_LIBS_USER = candidatos_biblioteca[[1L]])
    .libPaths(unique(c(candidatos_biblioteca, .libPaths())))
  }
}

cat("\nLA PALA · INICIO\n")
cat("Proyecto:", raiz, "\n")

paquetes_requeridos <- c("httpuv", "jsonlite", "later", "httr", "xml2", "digest")
paquetes_faltantes <- paquetes_requeridos[
  !vapply(paquetes_requeridos, requireNamespace, logical(1), quietly = TRUE)
]

if (length(paquetes_faltantes)) {
  cat("\nFaltan paquetes de R:", paste(paquetes_faltantes, collapse = ", "), "\n")
  cat("Instálalos con:\n")
  cat("install.packages(c(", paste(sprintf("'%s'", paquetes_faltantes), collapse = ", "), "))\n")
  stop("Instala las dependencias indicadas y vuelve a iniciar La Pala.", call. = FALSE)
}

anfitrion <- Sys.getenv("LA_PALA_HOST", unset = Sys.getenv("PEGA_HOST", unset = "127.0.0.1"))
puerto <- suppressWarnings(as.integer(Sys.getenv("PORT", unset = "8000")))
if (is.na(puerto) || puerto < 1L || puerto > 65535L) puerto <- 8000L
direccion <- paste0("http://", anfitrion, ":", puerto, "/")
cat("Aplicación:", direccion, "\n")
cat("Fuentes:", file.path(raiz, "scraping", "sources.json"), "\n")

source(file.path(raiz, "backend", "servidor.R"), local = TRUE)

binario_rscript <- Sys.which("Rscript")
if (!nzchar(binario_rscript)) {
  candidato <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")
  if (file.exists(candidato)) binario_rscript <- candidato
}

archivo_recolector <- file.path(raiz, "scraping", "run_daily.R")
archivo_bloqueo_recoleccion <- file.path(directorio_logs, "recoleccion.lock")
archivo_recoleccion_encolada <- file.path(directorio_logs, "recoleccion.encolada")

marca_reciente <- function(ruta, limite, unidad = "mins") {
  if (!file.exists(ruta)) return(FALSE)
  marca <- tryCatch(file.info(ruta)$mtime, error = function(e) NA)
  if (length(marca) != 1L || is.na(marca)) return(FALSE)
  edad <- suppressWarnings(as.numeric(difftime(Sys.time(), marca, units = unidad)))
  is.finite(edad) && edad >= 0 && edad < limite
}

iniciar_recoleccion <- function() {
  bloqueo_activo <- marca_reciente(archivo_bloqueo_recoleccion, 8, "hours")
  cola_activa <- marca_reciente(archivo_recoleccion_encolada, 5, "mins")
  if (file.exists(archivo_bloqueo_recoleccion) && !bloqueo_activo) unlink(archivo_bloqueo_recoleccion)
  if (file.exists(archivo_recoleccion_encolada) && !cola_activa) unlink(archivo_recoleccion_encolada)
  if (bloqueo_activo || cola_activa) {
    message("El Chamebador ya tiene una exploración activa o en cola.")
    return(invisible(FALSE))
  }
  if (!nzchar(binario_rscript) || !file.exists(archivo_recolector)) {
    message("No se pudo iniciar la recolección automática: falta Rscript o scraping/run_daily.R.")
    return(invisible(FALSE))
  }
  if (!isTRUE(dir.create(archivo_recoleccion_encolada, showWarnings = FALSE))) {
    message("Otra exploración acaba de ponerse en cola.")
    return(invisible(FALSE))
  }
  tryCatch({
    system2(
      binario_rscript,
      args = shQuote(archivo_recolector),
      stdout = FALSE,
      stderr = FALSE,
      wait = FALSE
    )
    message("Chamebador inició la exploración en segundo plano.")
    TRUE
  }, error = function(error) {
    unlink(archivo_recoleccion_encolada, recursive = TRUE)
    message("No se pudo iniciar la recolección: ", conditionMessage(error))
    FALSE
  })
}

segundos_hasta_manana <- function() {
  zona <- "America/La_Paz"
  ahora <- as.POSIXct(format(Sys.time(), "%Y-%m-%d %H:%M:%S", tz = zona), tz = zona)
  dia_siguiente <- as.Date(ahora, tz = zona) + 1
  medianoche <- as.POSIXct(paste(dia_siguiente, "00:00:00"), tz = zona)
  max(60, as.numeric(difftime(medianoche, ahora, units = "secs")))
}

programar_siguiente_recoleccion <- function() {
  later::later(function() {
    iniciar_recoleccion()
    programar_siguiente_recoleccion()
  }, delay = segundos_hasta_manana())
}

later::later(iniciar_recoleccion, delay = 5)
programar_siguiente_recoleccion()

if (anfitrion %in% c("127.0.0.1", "localhost") &&
    Sys.getenv("LA_PALA_OPEN_BROWSER", unset = "1") != "0") {
  later::later(function() {
    tryCatch(utils::browseURL(direccion), error = function(error) {
      message("Abre manualmente: ", direccion)
    })
  }, delay = 2)
}

cat("Deja esta ventana abierta mientras uses la aplicación. Pulsa Ctrl+C para detenerla.\n\n")
servir(anfitrion, puerto)
