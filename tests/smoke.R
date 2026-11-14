# Pruebas locales de humo.
# Ejecutar desde la raíz: Rscript tests/smoke.R

args <- commandArgs(trailingOnly = FALSE)
script <- grep("^--file=", args, value = TRUE)
if (length(script)) {
  ruta <- normalizePath(sub("^--file=", "", script[[1L]]), winslash = "/", mustWork = TRUE)
  raiz <- dirname(dirname(ruta))
  setwd(raiz)}

paquetes <- c("jsonlite", "digest", "xml2", "httr", "curl", "httpuv", "later")
faltan <- paquetes[!vapply(paquetes, requireNamespace, logical(1), quietly = TRUE)]
if (length(faltan)) stop("Faltan paquetes: ", paste(faltan, collapse = ", "))
stopifnot(file.exists("backend/nucleo.R"), file.exists("scraping/collect.R"))
jsonlite::fromJSON("scraping/sources.json", simplifyVector = FALSE)
jsonlite::fromJSON("data/catalogo.json", simplifyVector = FALSE)
source("backend/nucleo.R", local = .GlobalEnv, encoding = "UTF-8")
# Normalización y ranking.
stopifnot(identical(normalizar_texto("Becas en La Paz"), "becas en la paz"))
o <- list(
  titulo = "Convocatoria de becas 2026",
  entidad = "Universidad Pública",
  descripcion = "Postulación para estudiantes.",
  categoria = "becas",
  departamento = "La Paz",
  requisitos = "Título de bachiller"
)
stopifnot(puntuar_oportunidad(o, c("becas", "estudiantes")) > 0)
stopifnot(puntuar_oportunidad(o, c("", NA_character_)) == 0)

# PageRank, componentes y análisis con grafos vacíos / datos incompletos.
stopifnot(length(pagerank(matrix(numeric(), 0, 0))) == 0L)
stopifnot(componentes(matrix(numeric(), 0, 0)) == 0L)
stopifnot(length(comunidades(matrix(numeric(), 0, 0))) == 0L)
analisis <- analizar_catalogo(list(list(
  titulo = "Convocatoria de prueba",
  descripcion = "Publicación de prueba para validar el análisis.",
  categoria = "becas",
  fuente_nombre = "Fuente de prueba",
  fuente_oficial = TRUE,
  fecha_limite = "",
  url = "https://example.org/convocatoria"
)))
stopifnot(!isTRUE(analisis$vacio), analisis$totales$oportunidades == 1L)
# El recolector se carga sin activar el ciclo de red al ser source()'ado.
source("scraping/collect.R", local = .GlobalEnv, encoding = "UTF-8")
stopifnot(identical(extraer_fecha_limite("Fecha límite: 2026-12-15"), "2026-12-15"))
stopifnot(identical(extraer_fecha_limite("Fecha límite: 31/02/2026"), ""))
cat("OK: pruebas locales de humo completadas. No se contactó ningún sitio web.\\n")