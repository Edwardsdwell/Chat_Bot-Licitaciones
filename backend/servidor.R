# Servidor ligero sobre httpuv para la API y los archivos de la interfaz.
source(if (file.exists("backend/nucleo.R")) "backend/nucleo.R" else "nucleo.R", local = .GlobalEnv)
TIPOS <- c(html = "text/html; charset=utf-8", css = "text/css; charset=utf-8", js = "text/javascript; charset=utf-8", json = "application/json; charset=utf-8", jpeg = "image/jpeg", jpg = "image/jpeg", png = "image/png", svg = "image/svg+xml", ico = "image/x-icon", webp = "image/webp")
DIR_WEB <- normalizePath(file.path(RAIZ, "frontend"), winslash = "/")
DIR_MAPACHE <- normalizePath(file.path(RAIZ, "Chat_Bot-Public-Services", "mapache"), winslash = "/")
respuesta <- function(estado, cuerpo, tipo, extra = list()) list(status = estado, headers = c(list("Content-Type" = tipo), as.list(CABECERAS), extra), body = cuerpo)
json <- function(estado, x) respuesta(estado, as.character(jsonlite::toJSON(x, auto_unbox = TRUE, null = "null", na = "null")), TIPOS[["json"]], list("Cache-Control" = "no-store"))
consulta <- function(qs) {
  p <- strsplit(sub("^\\?", "", qs %||% ""), "&", fixed = TRUE)[[1L]]; p <- p[nzchar(p)]; k <- sub("=.*$", "", p); v <- ifelse(grepl("=", p, fixed = TRUE), sub("^[^=]*=", "", p), "")
  setNames(lapply(gsub("+", " ", v, fixed = TRUE), httpuv::decodeURIComponent), vapply(k, httpuv::decodeURIComponent, ""))
}
estatico <- function(ruta, req) {
  base <- if (startsWith(ruta, "/mapache/")) DIR_MAPACHE else DIR_WEB
  relativa <- if (identical(base, DIR_MAPACHE)) sub("^/mapache/", "", ruta) else if (ruta == "/") "index.html" else sub("^/", "", ruta)
  f <- normalizePath(file.path(base, relativa), winslash = "/", mustWork = FALSE)
  if (!startsWith(f, paste0(base, "/")) || !file.exists(f) || dir.exists(f)) return(json(404L, list(mensaje = "No encontrado.")))
  i <- file.info(f); etag <- sprintf('"%x-%x"', as.integer(i$mtime), i$size)
  if (identical(req$HTTP_IF_NONE_MATCH, etag)) return(list(status = 304L, headers = list(ETag = etag), body = ""))
  respuesta(200L, readBin(f, "raw", i$size), TIPOS[[tolower(tools::file_ext(f))]] %||% "application/octet-stream", list(ETag = etag, "Cache-Control" = "no-cache"))
}
aplicacion <- list(call = function(req) {
  ruta <- req$PATH_INFO; post <- identical(req$REQUEST_METHOD, "POST")
  if (!startsWith(ruta, "/api/")) return(if (req$REQUEST_METHOD %in% c("GET", "HEAD")) estatico(ruta, req) else json(405L, list(mensaje = "Método no permitido.")))
  h <- RUTAS[[ruta]]; if (is.null(h)) return(json(404L, list(mensaje = "Ruta inexistente.")))
  if (post != (ruta %in% RUTAS_POST)) return(json(405L, list(mensaje = "Método no permitido.")))
  res <- new.env(); res$status <- 200L; q <- consulta(req$QUERY_STRING); f <- names(formals(h)); a <- q[names(q) %in% f]
  if ("req" %in% f) a$req <- req; if ("res" %in% f) a$res <- res
  cuerpo <- tryCatch(do.call(h, a), error = function(e) {
    if (exists("registrar_falla", mode = "function", inherits = TRUE)) registrar_falla(ruta, e)
    res$status <- 500L
    list(mensaje = "No pude completar esta operación ahora. Inténtalo de nuevo más tarde.")
  })
  json(res$status, cuerpo)
})
servir <- function(host, puerto) httpuv::runServer(host, puerto, aplicacion)
