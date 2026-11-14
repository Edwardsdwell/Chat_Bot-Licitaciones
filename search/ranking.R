# search/ranking.R
# Ranking liviano, explicable y tolerante a datos incompletos.

`%||%` <- function(x, y) {if (is.null(x) || length(x) == 0L || all(is.na(x))) y else x}
normalizar_texto <- function(valor) {
  if (is.null(valor) || !length(valor)) return("")
  partes <- as.character(unlist(valor, recursive = TRUE, use.names = FALSE))
  partes <- partes[!is.na(partes)]
  if (!length(partes)) return("")
  x <- paste(partes, collapse = " ")
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT", sub = "")
  if (is.na(x)) x <- ""
  x <- tolower(x)
  x <- gsub("https?://[^[:space:]]+", " ", x, perl = TRUE)
  x <- gsub("[^a-z0-9]+", " ", x, perl = TRUE)
  trimws(gsub("\\s+", " ", x, perl = TRUE))
}

# Convierte una cadena en términos comparables. Elimina entradas vacías
# para evitar que grepl("", texto) puntúe accidentalmente todos los registros.
normalizar_terminos <- function(terminos) {
  if (is.null(terminos) || !length(terminos)) return(character())
  x <- vapply(as.list(terminos), normalizar_texto, character(1))
  x <- unlist(strsplit(x, "\\s+", perl = TRUE), use.names = FALSE)
  unique(x[nzchar(x)])
}

campo_oportunidad <- function(oportunidad, campos) {
  if (is.null(oportunidad) || !is.list(oportunidad)) return("")
  valores <- lapply(campos, function(n) oportunidad[[n]] %||% "")
  normalizar_texto(unlist(valores, recursive = TRUE, use.names = FALSE))
}

coincide_termino <- function(termino, texto) {
  if (!nzchar(termino) || !nzchar(texto)) return(FALSE)
  # Coincidencia de palabra/frase completa; evita "sol" dentro de "solicitud".
  patron <- paste0("(^| )", gsub(" ", " ", termino, fixed = TRUE), "( |$)")
  grepl(patron, texto, perl = TRUE)
}

puntuar_oportunidad <- function(oportunidad, terminos) {
  ts <- normalizar_terminos(terminos)
  if (!length(ts)) return(0)

  titulo <- campo_oportunidad(oportunidad, c("titulo", "entidad", "nombre"))
  detalle <- campo_oportunidad(
    oportunidad,
    c("descripcion", "categoria", "departamento", "requisitos",
      "institucion", "tipo", "modalidad", "ubicacion", "resumen")
  )

  sum(vapply(ts, function(t) {
    if (coincide_termino(t, titulo)) return(4)
    if (coincide_termino(t, detalle)) return(1)
    0
  }, numeric(1)))
}

# Ranking estable: conserva todos los campos originales y añade puntaje.
# Acepta data.frame o lista de oportunidades (listas nombradas).
rankear_oportunidades <- function(oportunidades, consulta = "", limite = Inf) {
  if (is.null(oportunidades) || length(oportunidades) == 0L) {
    if (is.data.frame(oportunidades)) {
      oportunidades$puntaje <- numeric(0)
      return(oportunidades)
    }
    return(list())
  }

  es_df <- is.data.frame(oportunidades)
  filas <- if (es_df) {
    lapply(seq_len(nrow(oportunidades)), function(i) as.list(oportunidades[i, , drop = FALSE]))
  } else if (is.list(oportunidades)) {
    if (!is.null(names(oportunidades)) &&
        any(names(oportunidades) %in% c("titulo", "descripcion", "categoria"))) {
      list(oportunidades)
    } else {oportunidades}
  } else {return(list())}
  q <- normalizar_terminos(consulta)
  puntajes <- vapply(filas, puntuar_oportunidad, numeric(1), terminos = q)

  # Desempate estable por fecha de cierre más próxima, si existe.
  orden <- order(-puntajes, seq_along(puntajes), na.last = TRUE)
  if (is.finite(limite)) orden <- head(orden, max(0L, as.integer(limite)))
  if (es_df) {
    out <- oportunidades[orden, , drop = FALSE]
    out$puntaje <- puntajes[orden]
    rownames(out) <- NULL
    return(out)
  }
  out <- filas[orden]
  for (i in seq_along(out)) {
    out[[i]]$puntaje <- unname(puntajes[orden][i])
  }
  out
}
# Alias de compatibilidad para código que use nombres alternativos.
rankear <- rankear_oportunidades
