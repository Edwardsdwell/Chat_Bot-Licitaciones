`%||%` <- get0("%||%", ifnotfound = function(x, y) if (is.null(x) || !length(x) || all(is.na(x))) y else x)
texto_seguro <- get0("texto_seguro", ifnotfound = function(x, d = "") { if (is.null(x) || !length(x) || is.list(x) || is.na(x[[1]])) return(d); v <- trimws(as.character(x[[1]])); if (nzchar(v)) v else d })
fecha_segura <- get0("fecha_segura", ifnotfound = function(v) { t <- texto_seguro(v); if (!nzchar(t)) as.Date(NA) else suppressWarnings(as.Date(substr(t, 1, 10))) })
# Motor analítico ligero: preprocesamiento, BM25, estadísticas, grafos, PageRank, SEO y fraude.
PARADAS <- unique(strsplit(normalizar_texto("de la el en y a los las del se por un una con para al es su que o lo que cual como mas pero sus le ya este si porque esta entre cuando muy sin sobre tambien me hasta hay donde quien desde todo nos durante todos uno les ni contra otros ese eso ante ellos e esto antes algunos unos yo otro otra el ella estar estas algo puedo pueden quiero busco fin mes cierren cierre abiertas abierta"), " ")[[1]])
tokenizar <- function(x) { t <- strsplit(normalizar_texto(x), " ", fixed = TRUE)[[1]]; t <- t[nchar(t) > 2 & !t %in% PARADAS]; sub("(?<=.{3})(es|s)$", "", t, perl = TRUE) }
campo <- function(ops, n, d = "") vapply(ops, function(o) texto_seguro(o[[n]], d), "")
host <- function(u) tolower(sub("^https?://([^/:?#]+).*$", "\\1", u))

# ---- Búsqueda BM25 con campos ponderados, corrección ortográfica y bono por frase ----
CACHE <- new.env()
tokens_doc <- function(o) {
  contenido <- c(rep(o$titulo, 3), rep(o$entidad, 2), o$descripcion, o$categoria, o$departamento, o$requisitos)
  identidad <- texto_seguro(o$id, texto_seguro(o$url))
  k <- paste(identidad, digest::digest(paste(contenido, collapse = " "), algo = "xxhash64"), sep = "|")
  if (is.null(CACHE[[k]])) CACHE[[k]] <- tokenizar(contenido)
  CACHE[[k]]
}
terminos_consulta <- function(texto) unique(tokenizar(texto))
puntuar_bm25 <- function(ops, q, k1 = 1.4, b = .75) {
  d <- lapply(ops, tokens_doc); n <- length(d); if (!n || !length(q)) return(numeric(n))
  voc <- unique(unlist(d)); if (!length(voc)) return(numeric(n))
  q <- unique(vapply(q, function(w) if (w %in% voc) w else { a <- drop(utils::adist(w, voc)); if (min(a) <= 1 + (nchar(w) > 5)) voc[which.min(a)] else w }, ""))
  l <- lengths(d); tf <- matrix(vapply(q, function(w) vapply(d, function(t) sum(t == w), 0), numeric(n)), n); df <- colSums(tf > 0)
  p <- rowSums(sweep(tf * (k1 + 1) / (tf + k1 * (1 - b + b * l / max(mean(l), 1))), 2, log(1 + (n - df + .5) / (df + .5)), `*`))
  fr <- paste(q, collapse = " "); if (length(q) > 1) p <- p + 2 * grepl(fr, vapply(ops, function(o) paste(tokenizar(o$titulo), collapse = " "), ""), fixed = TRUE)
  p
}

# ---- PageRank ponderado (iteración de potencia, maneja nodos sin salida) ----
pagerank <- function(A, d = .85, it = 100L, tol = 1e-9) {
  A <- as.matrix(A)
  if (length(dim(A)) != 2L || nrow(A) != ncol(A)) stop("La matriz de PageRank debe ser cuadrada.")
  n <- nrow(A)
  if (!n) return(numeric())
  storage.mode(A) <- "numeric"
  A[!is.finite(A) | A < 0] <- 0
  d <- max(0, min(.99, as.numeric(d)[1L]))
  it <- max(1L, as.integer(it)[1L])
  s <- rowSums(A)
  P <- A / ifelse(s > 0, s, 1)
  r <- rep(1 / n, n)
  for (i in seq_len(it)) {
    r2 <- (1 - d) / n + d * (drop(crossprod(P, r)) + sum(r[s == 0]) / n)
    if (!all(is.finite(r2)) || sum(r2) <= 0) r2 <- rep(1 / n, n)
    fin <- sum(abs(r2 - r)) < tol
    r <- r2
    if (fin) break
  }
  r / sum(r)
}

# ---- HITS: autoridad (enlaces entrantes) y hub (enlaces salientes) ----
hits <- function(A, it = 100L, tol = 1e-9) {
  A <- as.matrix(A); n <- nrow(A)
  if (n != ncol(A)) stop("La matriz HITS debe ser cuadrada.")
  if (!n) return(list(authority = numeric(), hub = numeric()))
  storage.mode(A) <- "numeric"; A[!is.finite(A) | A < 0] <- 0
  autoridad <- centro <- rep(1 / sqrt(n), n)
  for (i in seq_len(max(1L, as.integer(it)))) {
    a <- drop(crossprod(A, centro)); a <- a / max(sqrt(sum(a^2)), 1e-12)
    h <- drop(A %*% a); h <- h / max(sqrt(sum(h^2)), 1e-12)
    convergio <- max(abs(a - autoridad), abs(h - centro)) < tol
    autoridad <- a; centro <- h
    if (convergio) break
  }
  list(authority = autoridad, hub = centro)
}

comunidades <- function(A) {
  A <- as.matrix(A)
  if (nrow(A) != ncol(A)) stop("La matriz de comunidades debe ser cuadrada.")
  n <- nrow(A)
  if (!n) return(integer())
  S <- A + t(A)
  l <- seq_len(n)
  for (k in seq_len(15L)) {
    ant <- l
    for (i in seq_len(n)) {
      vecinos <- setdiff(seq_len(n), i)
      if (length(vecinos) && sum(S[i, vecinos]) > 0) {
        v <- tapply(S[i, vecinos], l[vecinos], sum)
        l[i] <- as.integer(names(v)[which.max(v)])
      }
    }
    if (all(ant == l)) break
  }
  match(l, unique(l))
}

componentes <- function(A) {
  A <- as.matrix(A)
  if (nrow(A) != ncol(A)) stop("La matriz de componentes debe ser cuadrada.")
  n <- nrow(A)
  if (!n) return(0L)
  S <- (A + t(A)) > 0
  grupos <- seq_len(n)
  repeat {
    nuevos <- vapply(seq_len(n), function(i) {
      vecinos <- which(S[i, ])
      min(c(grupos[i], grupos[vecinos]))
    }, integer(1))
    if (all(nuevos == grupos)) break
    grupos <- match(nuevos, unique(nuevos))
  }
  length(unique(grupos))
}

# ---- Fraude: señales textuales, de dominio y de duplicación ----
SENALES <- c(pago = "pag(ue|a|o)|deposit|transferenc|cuota de inscripcion|costo de (inscripcion|tramite)|adelant", urgencia = "urgente|ultima oportunidad|solo hoy|cupos limitados|apresur", promesa = "garantizad|sin experiencia|ganancias? (seguras|aseguradas)|ingresos? extra|dinero facil|gana (dolares|miles)", mensajeria = "whatsapp|telegram|wa me|t me", datos = "contrasena|clave|cuenta bancaria|tarjeta|codigo qr")
PESOS <- c(pago = 25, urgencia = 15, promesa = 25, mensajeria = 15, datos = 20)
riesgo_fraude <- function(o, duplicados = 0) {
  t <- normalizar_texto(c(o$titulo, o$descripcion)); u <- texto_seguro(o$url); h <- host(u); of <- isTRUE(o$fuente_oficial)
  s <- vapply(names(SENALES), function(n) grepl(SENALES[[n]], t), TRUE); p <- sum(PESOS[s])
  extra <- c("enlace sin HTTPS" = !grepl("^https://", u) * 20, "acortador de enlaces" = grepl("(^|\\.)(bit\\.ly|tinyurl\\.com|cutt\\.ly|t\\.co|goo\\.gl|is\\.gd)$", h) * 25,
    "dominio atípico" = (!of && (grepl("\\.(xyz|top|click|work|loan|icu|tk|ml|cf|ga|gq)$", h) || lengths(regmatches(h, gregexpr("[0-9-]", h))) > 4)) * 15,
    "texto en mayúsculas" = (nchar(gsub("[^A-ZÁÉÍÓÚ]", "", texto_seguro(o$titulo))) > .6 * max(nchar(texto_seguro(o$titulo)), 1) && nchar(texto_seguro(o$titulo)) > 12) * 10,
    "contenido duplicado" = (duplicados > 1) * 15, "sin entidad ni fecha" = (!nzchar(texto_seguro(o$entidad)) && !nzchar(texto_seguro(o$fecha_limite))) * 5)
  p <- min(100, p + sum(extra)); list(puntaje = p, nivel = if (p < 25) "bajo" else if (p < 50) "medio" else "alto", motivos = as.list(c(if (any(s)) paste("señal de", names(s)[s]), names(extra)[extra > 0])))
}
riesgos <- function(ops) { h <- campo(ops, "hash_contenido"); dup <- ifelse(nzchar(h), as.integer(table(h)[h]), 1L); lapply(seq_along(ops), function(i) riesgo_fraude(ops[[i]], dup[i])) }

# ---- Estadísticas, comparativas, relaciones y grafo del catálogo ----
conteo <- function(x, top = 12L) {
  x <- as.character(x)
  x <- x[!is.na(x) & nzchar(trimws(x))]
  if (!length(x)) return(list())
  t <- head(sort(table(x), decreasing = TRUE), top)
  lapply(seq_along(t), function(i) list(nombre = names(t)[i], n = as.integer(t[i])))
}

entropia <- function(x) {
  x <- as.character(x)
  x <- x[!is.na(x) & nzchar(trimws(x))]
  if (!length(x)) return(0)
  p <- as.numeric(table(x)) / length(x)
  if (length(p) < 2L) 0 else -sum(p * log(p)) / log(length(p))
}

similitud_sitios <- function(ops, fu) {
  u <- unique(fu)
  if (length(u) < 2L) return(list(matriz = NULL, pares = list()))
  tk <- lapply(u, function(s) unlist(lapply(ops[fu == s], tokens_doc), use.names = FALSE))
  vocabulario <- unlist(tk, use.names = FALSE)
  if (!length(vocabulario)) {
    S <- diag(length(u)); dimnames(S) <- list(u, u)
    return(list(matriz = S, pares = list()))
  }
  voc <- names(head(sort(table(vocabulario), decreasing = TRUE), 400L))
  tf <- t(vapply(tk, function(t) as.numeric(table(factor(t, levels = voc))), numeric(length(voc))))
  if (is.null(dim(tf))) tf <- matrix(tf, nrow = length(u))
  idf <- log(1 + length(u) / pmax(colSums(tf > 0), 1))
  W <- tf * rep(idf, each = length(u))
  W <- W / pmax(sqrt(rowSums(W^2)), 1e-9)
  S <- tcrossprod(W)
  dimnames(S) <- list(u, u)
  ij <- which(upper.tri(S), arr.ind = TRUE)
  if (!nrow(ij)) return(list(matriz = S, pares = list()))
  ij <- head(ij[order(-S[ij]), , drop = FALSE], 8L)
  list(matriz = S, pares = lapply(seq_len(nrow(ij)), function(k) {
    list(a = u[ij[k, 1L]], b = u[ij[k, 2L]], similitud = round(S[ij[k, 1L], ij[k, 2L]], 3))
  }))
}

dias_cierre <- function(ops) {
  vapply(ops, function(o) {
    f <- fecha_segura(o$fecha_limite)
    if (is.na(f)) NA_real_ else as.numeric(f - Sys.Date())
  }, numeric(1))
}

analizar_catalogo <- function(ops) {
  if (!length(ops)) return(list(vacio = TRUE))
  # Normaliza las estructuras sin descartar registros incompletos.
  ops <- lapply(ops, function(o) {
    if (!is.list(o)) o <- as.list(o)
    o
  })
  fu <- campo(ops, "fuente_nombre", "Sin fuente")
  fu[!nzchar(fu)] <- "Sin fuente"
  ca <- campo(ops, "categoria", "Sin categoría")
  de <- campo(ops, "departamento", "Sin especificar")
  en <- campo(ops, "entidad")
  ri <- riesgos(ops)
  pr <- vapply(ri, function(x) as.numeric(x$puntaje %||% 0), numeric(1))
  dc <- dias_cierre(ops)
  ld <- nchar(campo(ops, "descripcion"))
  fechas_pub <- campo(ops, "fecha_publicacion")
  fechas_pub <- as.Date(substr(fechas_pub, 1L, 10L))
  meses <- format(fechas_pub[!is.na(fechas_pub)], "%Y-%m")
  serie <- if (length(meses)) { z <- sort(table(meses)); lapply(seq_along(z), function(i) list(fecha = names(z)[i], n = as.integer(z[i]))) } else list()

  cub <- cut(dc, c(-Inf, 7, 14, 30, 60, Inf),
             c("≤7 días", "8-14", "15-30", "31-60", ">60"))
  dist_c <- as.data.frame(table(cub, useNA = "no"))

  sim <- similitud_sitios(ops, fu)
  sitios <- unique(fu)
  top_en <- names(head(sort(table(en[nzchar(en)]), decreasing = TRUE), 100L))

  aristas_base <- list()
  for (i in seq_along(ops)) {
    aristas_base[[length(aristas_base) + 1L]] <- data.frame(de = paste0("s:", fu[i]), a = paste0("c:", ca[i]), w = 1)
    if (nzchar(de[i])) aristas_base[[length(aristas_base) + 1L]] <- data.frame(de = paste0("s:", fu[i]), a = paste0("d:", de[i]), w = 1)
    if (nzchar(en[i]) && en[i] %in% top_en) aristas_base[[length(aristas_base) + 1L]] <- data.frame(de = paste0("s:", fu[i]), a = paste0("e:", en[i]), w = 1)
  }
  e <- if (length(aristas_base)) do.call(rbind, aristas_base) else data.frame(de = character(), a = character(), w = numeric())
  if (nrow(e)) {
    e <- stats::aggregate(w ~ de + a, e, sum)
    e <- e[!grepl(":$", e$a), , drop = FALSE]
  }
  if (!is.null(sim$matriz)) {
    ij <- which(sim$matriz > .15 & upper.tri(sim$matriz), arr.ind = TRUE)
    if (nrow(ij)) {
      w <- as.numeric(sim$matriz[ij]) * 5
      s1 <- paste0("s:", sitios[ij[, 1L]])
      s2 <- paste0("s:", sitios[ij[, 2L]])
      e <- rbind(e, data.frame(de = c(s1, s2), a = c(s2, s1), w = c(w, w)))
    }
  }

  # Mantiene nodos de fuentes aun cuando faltan categoría/entidad.
  nodos_fuente <- paste0("s:", sitios)
  nd <- unique(c(nodos_fuente, e$de, e$a))
  nd <- nd[!is.na(nd) & nzchar(nd)]
  A <- matrix(0, length(nd), length(nd))
  if (nrow(e) && length(nd)) {
    ii <- match(e$de, nd); jj <- match(e$a, nd)
    validos <- !is.na(ii) & !is.na(jj)
    if (any(validos)) A[cbind(ii[validos], jj[validos])] <- e$w[validos]
  }
  r <- pagerank(A)
  h <- hits(A)
  cm <- comunidades(A)
  S2 <- A + t(A)
  gr <- if (length(nd)) rowSums(S2 > 0) else integer()
  top <- head(order(-r), 60L)
  nodos <- lapply(top, function(i) list(
    id = nd[i], nombre = substring(nd[i], 3L), tipo = substr(nd[i], 1L, 1L),
    pagerank = round(r[i], 5), authority = round(h$authority[i], 5),
    hub = round(h$hub[i], 5), grado = gr[i], comunidad = cm[i]
  ))
  Am <- if (length(top)) A[top, top, drop = FALSE] else matrix(0, 0, 0)
  ar <- which(Am > 0, arr.ind = TRUE)

  media_segura <- function(x) {
    z <- mean(x, na.rm = TRUE)
    if (is.nan(z) || is.infinite(z)) NA_real_ else z
  }
  comp <- do.call(rbind, lapply(sitios, function(sitio) {
    idx <- which(fu == sitio)
    enlaces_fuente <- campo(ops[idx], "fuente_url", "")
    if (!any(nzchar(enlaces_fuente))) enlaces_fuente <- campo(ops[idx], "url", "")
    enlaces_fuente <- enlaces_fuente[nzchar(enlaces_fuente) & grepl("^https://", enlaces_fuente)]
    data.frame(
      sitio = sitio, url = if (length(enlaces_fuente)) enlaces_fuente[[1L]] else "", n = length(idx),
      con_fecha = round(100 * mean(!is.na(dc[idx]))),
      descripcion_media = round(mean(ld[idx])),
      riesgo_medio = round(mean(pr[idx]), 1),
      dias_cierre_medio = round(media_segura(dc[idx])),
      oficial = any(vapply(ops[idx], function(o) isTRUE(o$fuente_oficial), logical(1))),
      pagerank = if (paste0("s:", sitio) %in% nd) r[match(paste0("s:", sitio), nd)] else 0,
      authority = if (paste0("s:", sitio) %in% nd) h$authority[match(paste0("s:", sitio), nd)] else 0,
      hub = if (paste0("s:", sitio) %in% nd) h$hub[match(paste0("s:", sitio), nd)] else 0,
      stringsAsFactors = FALSE
    )
  }))
  comp <- comp[order(-comp$pagerank), , drop = FALSE]
  num <- comp[c("n", "con_fecha", "descripcion_media", "riesgo_medio", "pagerank", "authority", "hub")]
  cr <- if (nrow(num) > 1L) suppressWarnings(stats::cor(num, use = "pairwise.complete.obs")) else NULL
  fr <- head(order(-pr), min(8L, sum(pr >= 25)))

  list(
    totales = list(
      oportunidades = length(ops), sitios = length(sitios), nodos = length(nd),
      aristas = sum(A > 0), densidad = round(sum(A > 0) / max(length(nd) * (length(nd) - 1L), 1), 4),
      componentes = componentes(A), comunidades = if (length(cm)) max(cm) else 0L,
      riesgo_medio = round(mean(pr), 1), riesgo_alto = sum(pr >= 50)
    ),
    distribuciones = list(
      categoria = conteo(ca), departamento = conteo(de), fuente = conteo(fu),
      cierre = lapply(seq_len(nrow(dist_c)), function(i) list(nombre = as.character(dist_c$cub[i]), n = dist_c$Freq[i])),
      publicaciones_por_mes = serie,
      sin_fecha = sum(is.na(dc)),
      descripcion = list(
        media = round(mean(ld)), mediana = stats::median(ld), desviacion = round(stats::sd(ld), 1),
        q1 = unname(stats::quantile(ld, .25)), q3 = unname(stats::quantile(ld, .75))
      ),
      entropia_categorias = round(entropia(ca), 3)
    ),
    anomalias = local({
      n_por_sitio <- as.numeric(table(factor(fu, levels = unique(fu))))
      if (length(n_por_sitio) < 4L) list(metodo = "IQR", sitios_requeridos = 4L, sitios_actuales = length(n_por_sitio), hallazgos = list()) else {
        q <- stats::quantile(n_por_sitio, c(.25, .75), na.rm = TRUE); iqr <- q[[2L]] - q[[1L]]
        atipicos <- which(n_por_sitio < q[[1L]] - 1.5 * iqr | n_por_sitio > q[[2L]] + 1.5 * iqr)
        list(metodo = "IQR", sitios_requeridos = 4L, sitios_actuales = length(n_por_sitio),
             hallazgos = lapply(atipicos, function(i) list(sitio = sitios[i], publicaciones = n_por_sitio[i])))
      }
    }),
    comparativa = unname(lapply(seq_len(nrow(comp)), function(i) as.list(comp[i, ]))),
    relaciones = list(
      similares = sim$pares,
      correlaciones = if (!is.null(cr)) lapply(rownames(cr), function(a) {
        setNames(as.list(round(cr[a, ], 2)), colnames(cr))
      }) |> setNames(rownames(cr)) else NULL
    ),
    grafo = list(
      nodos = nodos,
      aristas = lapply(seq_len(nrow(ar)), function(k) list(de = ar[k, 1L], a = ar[k, 2L]))
    ),
    fraude = lapply(fr, function(i) c(
      list(titulo = texto_seguro(ops[[i]]$titulo), url = texto_seguro(ops[[i]]$url), fuente = fu[i]),
      ri[[i]]
    ))
  )
}

# ---- SEO / SERP de una página pública y rastreo ligero con PageRank interno ----
ip_v4_publica <- function(ip) {
  partes <- suppressWarnings(as.integer(strsplit(ip, ".", fixed = TRUE)[[1L]]))
  if (length(partes) != 4L || anyNA(partes) || any(partes < 0L | partes > 255L)) return(FALSE)
  a <- partes[1L]; b <- partes[2L]; c <- partes[3L]
  if (a == 0L || a == 10L || a == 127L || a >= 224L) return(FALSE)
  if (a == 100L && b >= 64L && b <= 127L) return(FALSE)
  if (a == 169L && b == 254L) return(FALSE)
  if (a == 172L && b >= 16L && b <= 31L) return(FALSE)
  if (a == 192L && b == 168L) return(FALSE)
  if (a == 192L && b == 0L && c %in% c(0L, 2L)) return(FALSE)
  if (a == 198L && b %in% c(18L, 19L, 51L)) return(FALSE)
  if (a == 203L && b == 0L && c == 113L) return(FALSE)
  TRUE
}

url_publica <- function(u) {
  if (length(u) != 1L || is.na(u) || !nzchar(u) || nchar(u) > 2048L) return(FALSE)
  p <- tryCatch(xml2::url_parse(u), error = function(e) NULL)
  if (is.null(p)) return(FALSE)
  esquema <- tolower(p$scheme %||% "")
  h <- tolower(p$hostname %||% p$server %||% "")
  if (!identical(esquema, "https") || !nzchar(h)) return(FALSE)
  if (!grepl("^[a-z0-9-]+(\\.[a-z0-9-]+)*\\.[a-z]{2,}$", h)) return(FALSE)
  if (grepl("(^localhost$|\\.(local|internal|lan|localdomain|home|corp|test|invalid)$)", h)) return(FALSE)
  # Rechaza credenciales embebidas y puertos distintos del HTTPS estándar.
  if (nzchar(p$username %||% "") || nzchar(p$password %||% "")) return(FALSE)
  puerto <- p$port %||% ""
  if (nzchar(as.character(puerto)) && !identical(as.character(puerto), "443")) return(FALSE)
  # Rechaza IPs literales; además consulta DNS y descarta destinos IPv4 no públicos.
  if (grepl("^[0-9.]+$", h)) return(FALSE)
  ips <- tryCatch(curl::nslookup(h), error = function(e) character())
  if (!length(ips)) return(FALSE)
  ips <- unique(as.character(ips))
  if (any(grepl(":", ips))) return(FALSE) # IPv6 no se habilita hasta validar rangos globales.
  all(vapply(ips, ip_v4_publica, logical(1)))
}
descargar <- function(u) {
  t0 <- proc.time()[["elapsed"]]
  visitadas <- character()
  r <- NULL
  for (i in seq_len(4L)) {
    if (u %in% visitadas || !url_publica(u)) return(NULL)
    visitadas <- c(visitadas, u)
    r <- tryCatch(httr::GET(
      u,
      httr::user_agent(Sys.getenv("LA_PALA_USER_AGENT", "LaPalaBot/2.0")),
      httr::timeout(10),
      httr::config(followlocation = FALSE, maxfilesize = 2e6)
    ), error = function(e) NULL)
    if (is.null(r) || !r$status_code %in% c(301L, 302L, 303L, 307L, 308L)) break
    destino <- httr::headers(r)$location %||% ""
    if (!nzchar(destino)) return(NULL)
    destino <- tryCatch(xml2::url_absolute(destino, u), error = function(e) "")
    if (!nzchar(destino) || !url_publica(destino) || host(destino) != host(u)) return(NULL)
    u <- destino
  }
  if (!is.null(r)) list(r = r, url = u, ms = round(1000 * (proc.time()[["elapsed"]] - t0)))
}
robots_ok <- function(u) {
  r <- tryCatch(httr::GET(paste0("https://", host(u), "/robots.txt"), httr::timeout(6), httr::config(followlocation = FALSE)), error = function(e) NULL)
  if (is.null(r)) return(FALSE)
  if (r$status_code >= 500L) return(FALSE)
  if (r$status_code >= 400L) return(TRUE)
  l <- trimws(sub("#.*", "", strsplit(httr::content(r, "text", encoding = "UTF-8"), "\r?\n")[[1]])); ag <- FALSE; dis <- character()
  for (x in l) { k <- tolower(x); if (startsWith(k, "user-agent:")) ag <- trimws(substring(x, 12)) == "*" else if (ag && startsWith(k, "disallow:") && nzchar(p <- trimws(substring(x, 10)))) dis <- c(dis, p) }
  ruta <- sub("^https://[^/]+", "", u); !any(vapply(dis, function(p) grepl(paste0("^", gsub("\\*", ".*", gsub("([.+?^(){}|\\[\\]\\\\])", "\\\\\\1", sub("\\$$", "", p), perl = TRUE))), ruta), TRUE))
}
meta_de <- function(d, n) { v <- xml2::xml_attr(xml2::xml_find_all(d, sprintf("//meta[@name='%s' or @property='%s']", n, n)), "content"); if (length(v)) trimws(v[1]) else "" }
analizar_pagina <- function(html, u, ms = NA) {
  d <- xml2::read_html(html); ld <- length(xml2::xml_find_all(d, "//script[@type='application/ld+json']")); xml2::xml_remove(xml2::xml_find_all(d, "//script|//style|//noscript"))
  tit <- trimws(xml2::xml_text(xml2::xml_find_first(d, "//title"))); tit <- if (is.na(tit)) "" else tit; cuerpo <- trimws(gsub("\\s+", " ", xml2::xml_text(xml2::xml_find_first(d, "//body")))); cuerpo <- if (is.na(cuerpo)) "" else cuerpo
  hr <- xml2::xml_attr(xml2::xml_find_all(d, "//a[@href]"), "href"); hr <- unique(xml2::url_absolute(hr[!grepl("^(#|mailto:|tel:|javascript:)", hr)], u)); hr <- hr[grepl("^https?://", hr)]; ext <- host(hr) != host(u)
  im <- xml2::xml_attr(xml2::xml_find_all(d, "//img"), "alt"); h1 <- length(xml2::xml_find_all(d, "//h1")); pal <- tokenizar(cuerpo); tt <- head(sort(table(pal), decreasing = TRUE), 8)
  desc <- meta_de(d, "description"); can <- xml2::xml_attr(xml2::xml_find_first(d, "//link[@rel='canonical']"), "href"); sin_alt <- sum(is.na(im) | !nzchar(im))
  ch <- c("Título entre 30 y 60 caracteres" = nchar(tit) >= 30 && nchar(tit) <= 60, "Meta descripción entre 70 y 160 caracteres" = nchar(desc) >= 70 && nchar(desc) <= 160, "Un único H1" = h1 == 1, "URL canónica declarada" = !is.na(can) && nzchar(can),
    "Idioma declarado (lang)" = !is.na(xml2::xml_attr(xml2::xml_find_first(d, "/html"), "lang")), "Viewport móvil" = nzchar(meta_de(d, "viewport")), "Imágenes con texto alternativo (≥90 %)" = length(im) == 0 || sin_alt / length(im) <= .1, "Contenido de al menos 300 palabras" = length(pal) >= 300,
    "Al menos 3 enlaces internos" = sum(!ext) >= 3, "Etiquetas Open Graph" = nzchar(meta_de(d, "og:title")), "Datos estructurados JSON-LD" = ld > 0, "Respuesta en menos de 1,5 s" = !is.na(ms) && ms < 1500, "Proporción texto/HTML ≥ 10 %" = nchar(cuerpo) / max(nchar(html), 1) >= .1)
  pt <- c(10, 10, 10, 5, 5, 5, 10, 10, 5, 5, 5, 10, 10)
  list(url = u, titulo = tit, descripcion = desc, puntaje = sum(pt[ch]), tiempo_ms = ms, palabras = length(pal), h1 = h1, imagenes = length(im), imagenes_sin_alt = sin_alt, enlaces_internos = sum(!ext), enlaces_externos = sum(ext), dominios_externos = length(unique(host(hr[ext]))),
    terminos = lapply(seq_along(tt), function(i) list(termino = names(tt)[i], densidad = round(100 * as.integer(tt[i]) / max(length(pal), 1), 2))), cumple = as.list(names(ch)[ch]), mejorar = as.list(names(ch)[!ch]), enlaces = hr[!ext])
}
seo_url <- function(u) {
  if (!url_publica(u)) return(list(url = u, error = "Solo se analizan direcciones HTTPS públicas."))
  if (!robots_ok(u)) return(list(url = u, error = "El portal no autoriza la consulta automática según su robots.txt."))
  x <- descargar(u); if (is.null(x) || x$r$status_code != 200) return(list(url = u, error = "La página no respondió con contenido disponible."))
  tryCatch(analizar_pagina(httr::content(x$r, "text", encoding = "UTF-8"), x$url, x$ms), error = function(e) list(url = u, error = "El contenido no se pudo interpretar."))
}
comparar_sitios <- function(urls) {
  sitios <- head(unique(trimws(urls[nzchar(trimws(urls))])), 7L)
  salida <- vector("list", length(sitios))
  for (i in seq_along(sitios)) {
    salida[[i]] <- seo_url(sitios[[i]])
    salida[[i]]$enlaces <- NULL
    if (i < length(sitios)) Sys.sleep(1)
  }
  salida
}
rastrear <- function(u, max = 8L, pausa = 1) {
  max <- min(max(2L, as.integer(max)), 12L); if (!url_publica(u) || !robots_ok(u)) return(list(error = "Dirección no permitida o bloqueada por robots.txt."))
  cola <- sub("#.*$", "", u); vistas <- character(); pag <- list(); h <- host(u)
  while (length(cola) && length(vistas) < max) { a <- cola[1]; cola <- cola[-1]; if (a %in% vistas || !robots_ok(a)) next; vistas <- c(vistas, a); x <- descargar(a); Sys.sleep(pausa)
    if (is.null(x) || x$r$status_code != 200 || !grepl("html", httr::headers(x$r)[["content-type"]] %||% "")) next
    p <- tryCatch(analizar_pagina(httr::content(x$r, "text", encoding = "UTF-8"), x$url, x$ms), error = function(e) NULL); if (is.null(p)) next
    p$enlaces <- unique(sub("#.*$", "", p$enlaces[host(p$enlaces) == h])); pag[[a]] <- p; cola <- c(cola, setdiff(p$enlaces, c(vistas, cola))) }
  if (!length(pag)) return(list(error = "No se pudo leer ninguna página."))
  nd <- unique(c(names(pag), unlist(lapply(pag, `[[`, "enlaces")))); A <- matrix(0, length(nd), length(nd)); for (a in names(pag)) A[match(a, nd), match(pag[[a]]$enlaces, nd)] <- 1
  r <- pagerank(A); puntajes_hits <- hits(A); o <- order(-r)[seq_len(min(15, length(nd)))]
  list(sitio = h, paginas_leidas = length(pag), nodos = length(nd), aristas = sum(A), puntaje_medio = round(mean(vapply(pag, `[[`, 0, "puntaje"))), paginas = lapply(o, function(i) { p <- pag[[nd[i]]]; list(url = nd[i], pagerank = round(r[i], 4), authority = round(puntajes_hits$authority[i], 4), hub = round(puntajes_hits$hub[i], 4), entrantes = sum(A[, i]), salientes = sum(A[i, ]), titulo = p$titulo %||% "", puntaje = p$puntaje %||% NA) }))
}
