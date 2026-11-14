# analisis/texto.R
# Preprocesamiento y extracción de texto de La Pala (solo R base).
# Limpia el texto publicado, extrae plazos, importes, contactos, requisitos y resúmenes,
# y redacta respuestas elaboradas del chat. nucleo.R lo carga junto a motor.R.

TX_PARADA <- c(
  "de", "la", "el", "los", "las", "un", "una", "unos", "unas", "y", "o", "u", "e", "a", "en", "del", "al",
  "por", "para", "con", "sin", "sobre", "entre", "hasta", "desde", "como", "que", "se", "su", "sus", "es",
  "son", "ser", "fue", "han", "has", "hay", "este", "esta", "estos", "estas", "ese", "esa", "esos", "esas",
  "lo", "le", "les", "mas", "muy", "ya", "no", "si", "ni", "pero", "tambien", "segun", "cada", "todo",
  "todos", "toda", "todas", "otro", "otra", "otros", "otras", "cual", "cuales", "donde", "cuando", "quien",
  "quienes", "tiene", "tienen", "puede", "pueden", "deben", "debe", "favor", "parte", "partir", "durante",
  "mediante", "contra", "tras", "ante", "bajo", "hacia", "asi", "solo", "mismo", "misma"
)

TX_MESES <- c(enero = 1L, febrero = 2L, marzo = 3L, abril = 4L, mayo = 5L, junio = 6L, julio = 7L,
              agosto = 8L, septiembre = 9L, setiembre = 9L, octubre = 10L, noviembre = 11L, diciembre = 12L)

TX_DEPARTAMENTOS <- c(
  "La Paz" = "la paz|el alto|lapaz",
  "Santa Cruz" = "santa cruz|scz",
  "Cochabamba" = "cochabamba",
  "Oruro" = "oruro",
  "Potosí" = "potosi",
  "Chuquisaca" = "chuquisaca|sucre",
  "Tarija" = "tarija",
  "Beni" = "beni|trinidad",
  "Pando" = "pando|cobija"
)

TX_SECCIONES <- list(
  requisitos = "requisit|experiencia|formacion|titulo|licenciatura|perfil|conocimiento|habilidad|indispensable|deseable|egresad|profesional",
  postulacion = "postul|presentar|enviar|remitir|correo|@|inscrip|documentacion|documentos",
  ubicacion = "ubicacion|lugar de trabajo|lugar de prestacion|sede|oficinas?|ciudad de|departamento de|direccion|avenida|calle ",
  remuneracion = "sueldo|salario|haber|remuneracion|honorario|presupuesto|precio referencial|monto|bs\\.?\\s?[0-9]"
)

TX_CONSULTA_CATEGORIA <- c(
  becas = "becas", empleo_publico = "empleo público", contrataciones = "licitaciones",
  merito = "concursos de mérito", fondos = "fondos", normativa = "normativa", convocatorias = "convocatorias"
)

TX_SINONIMOS <- list(
  beca = "becas pasantia", becas = "beca pasantia", pasantia = "pasantias beca", pasantias = "pasantia beca",
  licitacion = "licitaciones", licitaciones = "licitacion", convocatoria = "convocatorias",
  convocatorias = "convocatoria", empleo = "empleos vacante cargo", trabajo = "empleo vacante",
  chamba = "empleo vacante", vacante = "vacantes cargo", fondo = "fondos financiamiento",
  ley = "decreto norma", decreto = "supremo norma", sicoes = "contratacion licitacion",
  medico = "salud", abogado = "juridico derecho", ingeniero = "ingenieria",
  sistemas = "informatica software", contador = "contabilidad finanzas"
)

TX_RUIDO_CONSULTA <- paste(
  "hola dame damelo muestrame mostrar muestra quiero quisiera busco buscar necesito puedes podrias favor",
  "por para de del la el los las un una unos unas en con que cual cuales cuantas cuantos hay haya existen existe",
  "abiertas abiertos abierta abierto vigentes vigente disponibles disponible ofertas oferta publicaciones",
  "publicacion oportunidades oportunidad cierran cierra cierren cierre vencen vence antes hasta esta este",
  "semana mes hoy manana proximos proximas proximo proxima dias dia urgente urgentes fin final todas todos",
  "sobre acerca informacion me mi mis puedo pedir aplicar postular postulo"
)

# ---- Normalización ---------------------------------------------------------------

tx_sin_acentos <- function(x) chartr("áéíóúüñÁÉÍÓÚÜÑ", "aeiouunAEIOUUN", enc2utf8(as.character(x)))
tx_norm <- function(x) tolower(tx_sin_acentos(x))

tx_tokens <- function(x) {
  p <- unlist(strsplit(tx_norm(x), "[^a-z0-9]+"), use.names = FALSE)
  p[nchar(p) >= 4L & !(p %in% TX_PARADA) & !grepl("^[0-9]+$", p)]
}

# ---- Limpieza y segmentación -----------------------------------------------------

tx_limpiar <- function(x) {
  x <- enc2utf8(texto_seguro(x))
  if (!nzchar(x)) return("")
  x <- gsub("<(script|style)[^>]*>.*?</\\1>", " ", x, perl = TRUE, ignore.case = TRUE)
  x <- gsub("<[^>]+>", " ", x)
  entidades <- c("&nbsp;" = " ", "&#160;" = " ", "&amp;" = "&", "&lt;" = "<", "&gt;" = ">", "&quot;" = "\"", "&#39;" = "'", "&ordm;" = "º")
  for (e in names(entidades)) x <- gsub(e, entidades[[e]], x, fixed = TRUE)
  x <- gsub("[[:cntrl:]]+", " ", x)
  x <- gsub("(?i)(leer m[aá]s|ver m[aá]s|saber m[aá]s|compartir en [a-z]+|compartir|imprimir|haz clic aqu[ií]|clic aqu[ií]|descargar pdf|volver arriba|ir al contenido|men[uú] principal)", " ", x, perl = TRUE)
  trimws(gsub("[[:space:]]+", " ", x))
}

tx_oraciones <- function(x) {
  x <- tx_limpiar(x)
  if (!nzchar(x)) return(character())
  o <- unlist(strsplit(x, "(?<=[.!?;:])\\s+(?=[A-ZÁÉÍÓÚÑ0-9«\"(¿¡])", perl = TRUE), use.names = FALSE)
  o <- trimws(o)
  o <- o[nchar(o) >= 20L]
  o <- ifelse(nchar(o) > 400L, paste0(substr(o, 1L, 397L), "…"), o)
  unique(o)
}

# ---- Fechas ----------------------------------------------------------------------

tx_fechas <- function(texto, anio_ref = as.integer(format(Sys.Date(), "%Y"))) {
  t <- tx_norm(texto)
  vacio <- as.Date(character())
  if (!nzchar(t)) return(vacio)
  salida <- character()
  patron <- "([0-3]?[0-9]) de (enero|febrero|marzo|abril|mayo|junio|julio|agosto|septiembre|setiembre|octubre|noviembre|diciembre)( (de|del) ([0-9]{4}))?"
  for (s in regmatches(t, gregexpr(patron, t, perl = TRUE))[[1L]]) {
    p <- regmatches(s, regexec(patron, s, perl = TRUE))[[1L]]
    if (length(p) < 6L) next
    mes <- TX_MESES[[p[[3L]]]]
    dia <- as.integer(p[[2L]])
    if (nzchar(p[[6L]])) {
      salida <- c(salida, sprintf("%04d-%02d-%02d", as.integer(p[[6L]]), mes, dia))
    } else {
      f <- as.Date(sprintf("%04d-%02d-%02d", anio_ref, mes, dia), format = "%Y-%m-%d")
      if (!is.na(f) && f < Sys.Date() - 31L) f <- as.Date(sprintf("%04d-%02d-%02d", anio_ref + 1L, mes, dia), format = "%Y-%m-%d")
      salida <- c(salida, if (is.na(f)) NA_character_ else format(f))
    }
  }
  for (s in regmatches(t, gregexpr("[0-3]?[0-9][/.-][01]?[0-9][/.-]20[0-9]{2}", t))[[1L]]) {
    p <- as.integer(strsplit(s, "[/.-]")[[1L]])
    if (length(p) == 3L) salida <- c(salida, sprintf("%04d-%02d-%02d", p[[3L]], p[[2L]], p[[1L]]))
  }
  salida <- c(salida, regmatches(t, gregexpr("20[0-9]{2}-[01][0-9]-[0-3][0-9]", t))[[1L]])
  f <- as.Date(salida, format = "%Y-%m-%d")
  sort(unique(f[!is.na(f)]))
}

# Fecha límite: la más tardía entre las fechas de oraciones que hablan de plazos o cierre.
tx_fecha_limite <- function(texto, oraciones = tx_oraciones(texto)) {
  contexto <- "plazo|fecha limite|fecha de cierre|cierre|vence|vencimiento|hasta el|recepcion de|presentacion de|presentar|postulacion|postular|inscripcion"
  mejores <- as.Date(character())
  for (o in oraciones) {
    if (!grepl(contexto, tx_norm(o), perl = TRUE)) next
    mejores <- c(mejores, tx_fechas(o))
  }
  if (length(mejores)) max(mejores) else as.Date(NA)
}

tx_fecha_tope_consulta <- function(t) {
  hoy <- Sys.Date()
  if (grepl("(^| )(hoy|urgente|urgentes)( |$)", t)) return(hoy)
  if (grepl("(^| )manana( |$)", t)) return(hoy + 1L)
  if (grepl("esta semana", t)) return(hoy + ((7L - as.integer(format(hoy, "%u"))) %% 7L))
  if (grepl("proxima semana|proximos (7|siete) dias|proximos dias", t)) return(hoy + 7L)
  if (grepl("proximos (15|quince) dias|quincena", t)) return(hoy + 15L)
  if (grepl("proximos (30|treinta) dias|proximo mes", t)) return(hoy + 30L)
  f <- tx_fechas(t)
  if (length(f) && grepl("(antes|hasta|para|al|del) ", t)) return(f[[1L]])
  as.Date(NA)
}

# ---- Importes y contactos --------------------------------------------------------

tx_montos <- function(texto, max = 3L) {
  t <- tx_limpiar(texto)
  if (!nzchar(t)) return(character())
  a <- regmatches(t, gregexpr("(?i)(?<![A-Za-z])(bs|usd|us\\$|\\$us)\\.?\\s?[0-9][0-9.,]*", t, perl = TRUE))[[1L]]
  b <- regmatches(t, gregexpr("(?i)[0-9][0-9.,]*\\s?(bolivianos|dolares|dólares)", t, perl = TRUE))[[1L]]
  m <- unique(trimws(gsub("[.,]+$", "", c(a, b))))
  head(m[nzchar(m)], max)
}

tx_contactos <- function(texto) {
  t <- tx_limpiar(texto)
  correos <- unique(unlist(regmatches(t, gregexpr("[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}", t)), use.names = FALSE))
  tel <- unique(unlist(regmatches(t, gregexpr("(?i)(tel[eé]fonos?|tel\\.?|cel(ular)?\\.?|whatsapp)[: ]*\\+?[0-9][0-9 -]{6,14}[0-9]", t, perl = TRUE)), use.names = FALSE))
  list(correos = head(correos, 3L), telefonos = head(tel, 2L))
}

# ---- Secciones, resumen y palabras clave ----------------------------------------

tx_secciones <- function(texto, max_por = 2L, oraciones = tx_oraciones(texto)) {
  out <- list()
  if (!length(oraciones)) return(out)
  normal <- tx_norm(oraciones)
  for (n in names(TX_SECCIONES)) {
    sel <- oraciones[grepl(TX_SECCIONES[[n]], normal, perl = TRUE)]
    if (length(sel)) out[[n]] <- as.list(head(substr(sel, 1L, 260L), max_por))
  }
  out
}

tx_palabras_clave <- function(texto, k = 6L) {
  p <- tx_tokens(texto)
  if (!length(p)) return(character())
  names(head(sort(table(p), decreasing = TRUE), k))
}

tx_resumen <- function(texto, titulo = "", n = 2L, max_chars = 300L, oraciones = tx_oraciones(texto)) {
  tit <- tx_norm(titulo)
  oraciones <- oraciones[tx_norm(oraciones) != tit & nchar(oraciones) >= 40L]
  if (!length(oraciones)) return("")
  frec <- table(tx_tokens(paste(oraciones, collapse = " ")))
  tt <- tx_tokens(titulo)
  puntaje <- vapply(oraciones, function(o) {
    tk <- unique(tx_tokens(o))
    if (!length(tk)) return(0)
    base <- (sum(as.numeric(frec[tk]), na.rm = TRUE) + 3 * sum(tk %in% tt)) / sqrt(length(tk))
    if (grepl("cookies|derechos reservados|suscrib|newsletter|javascript", tx_norm(o))) base <- base * 0.1
    base
  }, numeric(1))
  elegidas <- sort(head(order(-puntaje), n))
  r <- paste(oraciones[elegidas], collapse = " ")
  if (nchar(r) > max_chars) r <- paste0(substr(r, 1L, max_chars - 1L), "…")
  r
}

tx_departamento <- function(texto) {
  t <- paste0(" ", tx_norm(texto), " ")
  mejor <- ""; pos <- Inf
  for (n in names(TX_DEPARTAMENTOS)) {
    m <- regexpr(paste0("(?<![a-z])(", TX_DEPARTAMENTOS[[n]], ")(?![a-z])"), t, perl = TRUE)
    if (m[[1L]] > 0L && m[[1L]] < pos) { mejor <- n; pos <- m[[1L]] }
  }
  mejor
}

tx_patron_departamento <- function(nombre) paste0("(?<![a-z])(", TX_DEPARTAMENTOS[[nombre]], ")(?![a-z])")

# ---- Enriquecimiento de una publicación -----------------------------------------

tx_enriquecer <- function(o) {
  if (!is.list(o)) return(o)
  titulo <- texto_seguro(o$titulo)
  base <- paste(texto_seguro(o$descripcion), texto_seguro(o$requisitos), sep = ". ")
  oraciones <- tx_oraciones(base)
  datos <- list()
  fl <- tx_fecha_limite(base, oraciones)
  if (!is.na(fl)) datos$fecha_limite_texto <- format(fl)
  m <- tx_montos(base)
  if (length(m)) datos$montos <- as.list(m)
  datos <- c(datos, tx_secciones(base, oraciones = oraciones))
  contacto <- tx_contactos(base)
  if (length(contacto$correos)) datos$correos <- as.list(contacto$correos)
  if (length(contacto$telefonos)) datos$telefonos <- as.list(contacto$telefonos)
  dep <- tx_departamento(paste(titulo, base))
  if (nzchar(dep)) datos$departamento_texto <- dep
  o$resumen <- if (identical(texto_seguro(o$confianza_extraccion), "requiere_revision")) "" else tx_resumen(base, titulo, oraciones = oraciones)
  o$palabras_clave <- as.list(tx_palabras_clave(paste(titulo, base), 6L))
  o$datos <- datos
  o
}

# ---- Consulta: depuración y expansión -------------------------------------------

tx_depurar_consulta <- function(t, departamento = "") {
  t <- gsub("[0-3]?[0-9] de (enero|febrero|marzo|abril|mayo|junio|julio|agosto|septiembre|setiembre|octubre|noviembre|diciembre)( (de|del) [0-9]{4})?", " ", t, perl = TRUE)
  t <- gsub("20[0-9]{2}", " ", t)
  if (nzchar(departamento)) t <- gsub(tx_patron_departamento(departamento), " ", paste0(" ", t, " "), perl = TRUE)
  ruido <- paste(strsplit(TX_RUIDO_CONSULTA, " ", fixed = TRUE)[[1L]], collapse = "|")
  t <- gsub(paste0("(?<![a-z0-9])(", ruido, ")(?![a-z0-9])"), " ", t, perl = TRUE)
  t <- gsub("[^a-z0-9 ]", " ", t)
  trimws(gsub("[[:space:]]+", " ", t))
}

tx_expandir <- function(t) {
  p <- unlist(strsplit(t, " ", fixed = TRUE), use.names = FALSE)
  extra <- unlist(lapply(p, function(w) if (!is.null(TX_SINONIMOS[[w]])) strsplit(TX_SINONIMOS[[w]], " ", fixed = TRUE)[[1L]]), use.names = FALSE)
  unique(extra)
}

# ---- Redacción de respuestas -----------------------------------------------------

tx_formato_fecha <- function(f) format(f, "%d/%m/%Y")

tx_plazo_texto <- function(f) {
  if (is.na(f)) return("sin fecha límite confirmada")
  d <- as.integer(f - Sys.Date())
  if (d < 0L) "plazo vencido"
  else if (d == 0L) "cierra hoy"
  else if (d == 1L) "cierra mañana"
  else if (d <= 14L) sprintf("cierra el %s (en %d días)", tx_formato_fecha(f), d)
  else sprintf("cierra el %s", tx_formato_fecha(f))
}

tx_top <- function(valores, n = 3L) {
  v <- valores[nzchar(valores)]
  if (!length(v)) return("")
  tb <- head(sort(table(v), decreasing = TRUE), n)
  paste0(names(tb), " (", as.integer(tb), ")", collapse = ", ")
}

tx_nombre_categoria <- function(id, categorias) {
  c <- Filter(function(x) identical(x$id, id), categorias)
  if (length(c)) c[[1L]]$nombre else ""
}

tx_disponibilidad <- function(activas, categorias) {
  if (!length(activas)) return("")
  ids <- vapply(activas, function(x) texto_seguro(x$categoria), character(1))
  tb <- sort(table(ids[nzchar(ids)]), decreasing = TRUE)
  if (!length(tb)) return("")
  nombres <- vapply(names(tb), function(i) { n <- tx_nombre_categoria(i, categorias); if (nzchar(n)) n else i }, character(1))
  paste0(nombres, " (", as.integer(tb), ")", collapse = ", ")
}

tx_redactar_busqueda <- function(coin, categorias) {
  todas <- if (length(coin$todas)) coin$todas else coin$oportunidades
  n <- coin$total
  cat_nombre <- tolower(tx_nombre_categoria(texto_seguro(coin$categoria), categorias))
  depto <- texto_seguro(coin$departamento)
  tope <- texto_seguro(coin$fecha_tope)
  intro <- paste0(
    "Encontré ", n, if (n == 1L) " publicación vigente" else " publicaciones vigentes",
    if (nzchar(cat_nombre)) paste0(" de ", cat_nombre) else "",
    if (nzchar(depto)) paste0(" para ", depto, " (incluye convocatorias de alcance nacional)") else "",
    if (nzchar(tope)) paste0(" con cierre hasta el ", tx_formato_fecha(fecha_segura(tope))) else "", "."
  )

  p <- coin$oportunidades[[1L]]
  fecha_p <- fecha_limite_efectiva(p)
  entidad <- texto_seguro(p$entidad, texto_seguro(p$fuente_nombre))
  lugar <- texto_seguro(departamento_efectivo(p))
  rasgos <- c(if (nzchar(entidad)) paste0("de ", entidad), if (nzchar(lugar)) paste0("en ", lugar))
  inferida <- is.na(fecha_segura(p$fecha_limite)) && !is.na(fecha_p)
  destacada <- paste0(
    "Coincidencia principal: «", texto_seguro(p$titulo, "publicación sin título"), "»",
    if (length(rasgos)) paste0(" ", paste(rasgos, collapse = ", ")) else "", "; ", tx_plazo_texto(fecha_p),
    if (inferida) " según el texto de la ficha" else "", "."
  )
  res <- texto_seguro(p$resumen)
  if (nzchar(res)) destacada <- paste0(destacada, " Resumen extraído: ", res)
  montos <- unlist(p$datos$montos, use.names = FALSE)
  if (length(montos)) destacada <- paste0(destacada, " Importes mencionados: ", paste(head(montos, 2L), collapse = "; "), ".")
  correos <- unlist(p$datos$correos, use.names = FALSE)
  if (length(correos)) destacada <- paste0(destacada, " Contacto publicado: ", correos[[1L]], ".")

  panorama <- character()
  if (n > 1L) {
    f <- vapply(todas, function(x) as.numeric(fecha_limite_efectiva(x)), numeric(1))
    hoy <- as.numeric(Sys.Date())
    con_fecha <- sum(!is.na(f))
    if (con_fecha) {
      prox7 <- sum(f >= hoy & f <= hoy + 7, na.rm = TRUE)
      panorama <- c(panorama, if (prox7 > 0L) sprintf("%d cierran en los próximos 7 días.", prox7) else "Ninguna cierra en los próximos 7 días.")
      futuras <- f[!is.na(f) & f >= hoy]
      if (length(futuras)) panorama <- c(panorama, paste0("La más próxima ", tx_plazo_texto(as.Date(min(futuras), origin = "1970-01-01")), "."))
    }
    sin_fecha <- sum(is.na(f))
    if (sin_fecha > 0L) panorama <- c(panorama, sprintf("%d sin fecha límite confirmada.", sin_fecha))
    ent <- vapply(todas, function(x) texto_seguro(x$entidad, texto_seguro(x$fuente_nombre)), character(1))
    if (length(unique(ent[nzchar(ent)])) > 1L) panorama <- c(panorama, paste0("Entidades con más publicaciones: ", tx_top(ent), "."))
    if (!nzchar(depto)) {
      dep <- vapply(todas, function(x) texto_seguro(departamento_efectivo(x)), character(1))
      dep <- dep[!grepl("bolivia|nacional", tx_norm(dep))]
      if (length(unique(dep[nzchar(dep)])) > 1L) panorama <- c(panorama, paste0("Por departamento: ", tx_top(dep), "."))
    }
  }
  partes <- c(intro, destacada)
  if (length(panorama)) partes <- c(partes, paste0("Panorama: ", paste(panorama, collapse = " ")))
  partes <- c(partes, "Pídeme «requisitos de la primera», «cómo postular a la segunda» o acota por ciudad, entidad o fecha. Verifica plazos y requisitos en el documento oficial antes de postular.")
  paste(partes, collapse = "\n\n")
}

tx_sugerencias <- function(coin, activas, categorias) {
  s <- character()
  cat_id <- texto_seguro(coin$categoria)
  palabra <- if (nzchar(cat_id) && !is.na(TX_CONSULTA_CATEGORIA[cat_id])) unname(TX_CONSULTA_CATEGORIA[cat_id]) else ""
  if (coin$total >= 1L) s <- c(s, "Dame los requisitos de la primera")
  todas <- if (length(coin$todas)) coin$todas else coin$oportunidades
  if (length(todas) && !nzchar(texto_seguro(coin$fecha_tope))) {
    f <- vapply(todas, function(x) as.numeric(fecha_limite_efectiva(x)), numeric(1))
    hoy <- as.numeric(Sys.Date())
    if (any(f >= hoy & f <= hoy + 7, na.rm = TRUE)) s <- c(s, paste0(if (nzchar(palabra)) paste0(toupper(substr(palabra, 1L, 1L)), substring(palabra, 2L)) else "Ofertas", " que cierren esta semana"))
  }
  if (!nzchar(texto_seguro(coin$departamento)) && length(todas)) {
    dep <- vapply(todas, function(x) texto_seguro(departamento_efectivo(x)), character(1))
    dep <- dep[nzchar(dep) & !grepl("bolivia|nacional", tx_norm(dep))]
    if (length(dep)) s <- c(s, paste0(if (nzchar(palabra)) paste0(toupper(substr(palabra, 1L, 1L)), substring(palabra, 2L)) else "Ofertas", " en ", names(sort(table(dep), decreasing = TRUE))[[1L]]))
  }
  ids <- unique(vapply(activas, function(x) texto_seguro(x$categoria), character(1)))
  otras <- setdiff(ids[ids %in% names(TX_CONSULTA_CATEGORIA)], cat_id)
  if (length(otras)) s <- c(s, paste0("¿Qué hay de ", unname(TX_CONSULTA_CATEGORIA[otras[[1L]]]), "?"))
  as.list(head(unique(s), 4L))
}
