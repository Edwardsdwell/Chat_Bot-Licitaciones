# Diagnóstico de fuentes: dice qué responde cada sitio y cuántos ítems logra extraer el recolector.
# Uso (desde la raíz del proyecto):   Rscript scraping/diagnostico.R [id1 id2 ...]
# Salida: logs/diagnostico.csv y muestras HTML en logs/muestras/<id>.html
# Pega aquí el CSV (o las filas con problemas) para ajustar extractores con datos reales.
source(file.path("scraping", "collect.R"), local = FALSE)
ids <- commandArgs(trailingOnly = TRUE)
fuentes <- leer_fuentes()
if (length(ids)) fuentes <- Filter(function(f) f$id %in% ids, fuentes)
dir.create(file.path(DIR_LOGS, "muestras"), recursive = TRUE, showWarnings = FALSE)
diagnosticar <- function(f) {
  url <- as.character(f$url)
  fila <- list(id = f$id, url = url, robots = "", http = 0L, tipo = "", bytes = 0L, barrera = "",
               filas_tabla = 0L, items_tabla = 0L, items_tarjetas = 0L, enlaces_relevantes = 0L, titulo = "", nota = "")
  reglas <- revisar_robots(url)
  fila$robots <- reglas$estado %||% ""
  if (!isTRUE(reglas$permitido)) { fila$nota <- "bloqueada por robots"; return(fila) }
  resp <- obtener(url, intento = if (isTRUE(reglas$sin_respuesta)) 3L else 1L)
  fila$http <- codigo_respuesta(resp)
  if (is.null(resp)) { fila$nota <- "sin respuesta (DNS/timeout/TLS/redirección a otro dominio)"; return(fila) }
  cuerpo <- texto_respuesta(resp)
  fila$tipo <- substr(tolower(httr::headers(resp)[["content-type"]] %||% ""), 1L, 40L)
  fila$bytes <- nchar(cuerpo, type = "bytes")
  writeLines(substr(cuerpo, 1L, 300000L), file.path(DIR_LOGS, "muestras", paste0(f$id, ".html")), useBytes = TRUE)
  doc <- tryCatch(xml2::read_html(cuerpo), error = function(e) NULL)
  if (is.null(doc)) { fila$nota <- "html no interpretable"; return(fila) }
  fila$titulo <- titulo_pagina(doc)
  fila$barrera <- detectar_barrera(texto_pagina(doc), doc)
  fila$filas_tabla <- length(xml2::xml_find_all(doc, "//table//tr[td]"))
  fila$items_tabla <- length(oportunidades_de_tabla(doc, url, f))
  fila$items_tarjetas <- length(tryCatch(oportunidades_de_tarjetas(doc, url, f), error = function(e) list()))
  fila$enlaces_relevantes <- length(enlaces_html(doc, url, as.character(f$categoria)))
  fila
}
filas <- list()
for (f in fuentes) {
  r <- tryCatch(diagnosticar(f), error = function(e) list(id = f$id, url = as.character(f$url), robots = "", http = 0L, tipo = "", bytes = 0L, barrera = "",
    filas_tabla = 0L, items_tabla = 0L, items_tarjetas = 0L, enlaces_relevantes = 0L, titulo = "", nota = paste("error:", conditionMessage(e))))
  filas[[length(filas) + 1L]] <- r
  cat(sprintf("%-34s http=%-3s robots=%-26s tabla=%-3s tarjetas=%-3s barrera=%s %s\n", r$id, r$http, r$robots, r$items_tabla, r$items_tarjetas, r$barrera, r$nota))
}
tabla <- do.call(rbind, lapply(filas, function(x) as.data.frame(x, stringsAsFactors = FALSE)))
utils::write.csv(tabla, file.path(DIR_LOGS, "diagnostico.csv"), row.names = FALSE, fileEncoding = "UTF-8")
cat("\nGuardado en", file.path(DIR_LOGS, "diagnostico.csv"), "\n")
