# =============================================================================
# MAPACHE | Plataforma de minería web y análisis de riesgos para Bolivia
# Reemplaza Shiny por HTML interactivo estático (sin servidor Shiny).
#
# EJECUCIÓN:
#   install.packages(c("httr2","rvest","xml2","robotstxt","jsonlite","stringi",
#     "igraph","leaflet","DT","visNetwork","htmltools","htmlwidgets"))
#   Sys.setenv(FIRMS_MAP_KEY="...", BRAVE_SEARCH_KEY="...") # opcionales
#   Rscript mapache_bolivia.R
#
# SALIDA: mapache_site/index.html + CSV/JSON de auditoría y minería.
# Los HTML son interactivos en navegador. Para datos nuevos, vuelve a ejecutar
# el script; no se necesita ni se inicia una aplicación Shiny.
# =============================================================================

suppressPackageStartupMessages({
  library(httr2); library(rvest); library(xml2); library(robotstxt)
  library(jsonlite); library(stringi); library(igraph)
  library(leaflet); library(DT); library(visNetwork)
  library(htmltools); library(htmlwidgets)
})

`%||%` <- function(x, y) if (is.null(x) || !length(x) || all(is.na(x))) y else x
options(timeout = 35, stringsAsFactors = FALSE)

# ------------------------------- CONFIGURACIÓN -------------------------------
APP_NAME <- "MAPACHE"
OUT_DIR <- Sys.getenv("MAPACHE_OUT", file.path("frontend", "mapache"))
USER_AGENT <- Sys.getenv("MAPACHE_USER_AGENT",
                         "MapacheBolivia/2.0 (investigacion publica; contacto: contacto@ejemplo.org)")
DELAY_SEC <- as.numeric(Sys.getenv("MAPACHE_DELAY_SEC", "8"))
CACHE_MIN <- as.numeric(Sys.getenv("MAPACHE_CACHE_MIN", "30"))
MAX_SEEDS <- as.integer(Sys.getenv("MAPACHE_MAX_SEEDS", "20"))
MAX_PAGES <- as.integer(Sys.getenv("MAPACHE_MAX_PAGES", "60"))
MAX_DEPTH <- as.integer(Sys.getenv("MAPACHE_MAX_DEPTH", "1"))
MAX_LINKS_PAGE <- as.integer(Sys.getenv("MAPACHE_MAX_LINKS_PAGE", "80"))
REQUEST_TIMEOUT <- as.integer(Sys.getenv("MAPACHE_TIMEOUT", "25"))
FIRMS_KEY <- Sys.getenv("FIRMS_MAP_KEY")
BRAVE_KEY <- Sys.getenv("BRAVE_SEARCH_KEY")
BING_KEY <- Sys.getenv("BING_SEARCH_KEY")
GOOGLE_KEY <- Sys.getenv("GOOGLE_CSE_KEY")
GOOGLE_CX <- Sys.getenv("GOOGLE_CSE_CX")
BBOX <- c(west = -69.7, south = -23.0, east = -57.5, north = -9.5)
ABC_URL <- "https://transitabilidad.abc.gob.bo/"
USGS_URL <- "https://earthquake.usgs.gov/fdsnws/event/1/query"
FIRMS_AREA <- paste(BBOX[["west"]], BBOX[["south"]], BBOX[["east"]], BBOX[["north"]], sep=",")
dir.create(OUT_DIR, recursive=TRUE, showWarnings=FALSE)
dir.create(file.path(OUT_DIR, "data"), showWarnings=FALSE)

CIUDADES <- data.frame(
  nombre=c("La Paz","El Alto","Cochabamba","Santa Cruz","Sucre","Oruro","Potosí",
           "Tarija","Trinidad","Cobija","Riberalta","Rurrenabaque","Uyuni","Yacuiba",
           "Desaguadero","San Ignacio de Velasco"),
  lat=c(-16.50,-16.50,-17.39,-17.78,-19.03,-17.97,-19.58,-21.53,-14.83,-11.03,
        -11.00,-14.44,-20.46,-22.02,-16.57,-16.37),
  lon=c(-68.15,-68.19,-66.16,-63.18,-65.26,-67.11,-65.75,-64.73,-66.06,-68.77,
        -66.06,-67.53,-66.82,-63.68,-69.04,-60.96), stringsAsFactors=FALSE)
# Fuentes/semillas públicas iniciales: la búsqueda añade resultados a esta lista.
SEED_URLS <- c(ABC=ABC_URL, SICOES="https://sicoes.gob.bo/portal/index.php", USGS="https://earthquake.usgs.gov/",
               NASA_FIRMS="https://firms.modaps.eosdis.nasa.gov/",
               Bolivia_Gob="https://www.bolivia.gob.bo/",
               ATT="https://www.att.gob.bo/")

# ----------------------------- UTILIDADES SEGURAS -----------------------------
`%notin%` <- function(x, table) !(x %in% table)
sin_acentos <- function(x) stringi::stri_trans_general(tolower(as.character(x)), "Latin-ASCII")
safe_name <- function(x) gsub("[^A-Za-z0-9_-]+","_",x)
write_csv_safe <- function(x, name) {
  if (is.data.frame(x)) utils::write.csv(x, file.path(OUT_DIR,"data",name),
                                          row.names=FALSE, na="")
}
cache_env <- new.env(parent=emptyenv())
last_hit <- new.env(parent=emptyenv())
cache_get <- function(key) {
  z <- cache_env[[key]]
  if (!is.null(z) && as.numeric(difftime(Sys.time(),z$at,units="mins")) < CACHE_MIN) z$value else NULL
}
cache_put <- function(key,value) cache_env[[key]] <- list(at=Sys.time(),value=value)
polite_wait <- function(host) {
  old <- last_hit[[host]]
  if (!is.null(old)) {
    wait <- DELAY_SEC-as.numeric(difftime(Sys.time(),old,units="secs"))
    if (is.finite(wait) && wait>0) Sys.sleep(wait)
  }
  last_hit[[host]] <- Sys.time()
}
host_of <- function(url) {
  tryCatch(tolower(sub("/.*$","",sub("^https?://","",url))),error=function(e) "")
}
valid_http_url <- function(url) {
  length(url)==1 && !is.na(url) && grepl("^https?://",url,ignore.case=TRUE)
}
robots_allowed <- function(url) {
  if (!valid_http_url(url)) return(FALSE)
  tryCatch(isTRUE(robotstxt::paths_allowed(url, user_agent=USER_AGENT, warn=FALSE)),
           error=function(e) FALSE)
}
http_get <- function(url, query=list(), use_cache=TRUE, headers=list()) {
  if (!valid_http_url(url)) stop("URL HTTP/HTTPS no válida.")
  key <- paste0(url,"?",paste(names(query),query,collapse="&"))
  if (use_cache) {
    old <- cache_get(key); if (!is.null(old)) return(old)
  }
  polite_wait(host_of(url))
  req <- httr2::request(url) |>
    httr2::req_user_agent(USER_AGENT) |>
    httr2::req_timeout(REQUEST_TIMEOUT) |>
    httr2::req_options(followlocation=TRUE) |>
    httr2::req_retry(max_tries=2)
  if (length(headers)) req <- req |> httr2::req_headers(!!!headers)
  resp <- req |> httr2::req_url_query(!!!query) |> httr2::req_perform()
  out <- list(status=httr2::resp_status(resp), url=httr2::resp_url(resp),
              type=httr2::resp_header(resp,"content-type") %||% "",
              body=httr2::resp_body_string(resp))
  if (use_cache && out$status>=200 && out$status<400) cache_put(key,out)
  out
}
scrape_page <- function(url) {
  if (!robots_allowed(url)) return(list(ok=FALSE,reason="robots.txt no autoriza o no se pudo verificar"))
  z <- tryCatch(http_get(url),error=function(e)e)
  if (inherits(z,"error")) return(list(ok=FALSE,reason=conditionMessage(z)))
  if (z$status<200 || z$status>=400) return(list(ok=FALSE,reason=paste("HTTP",z$status)))
  if (!grepl("html|xhtml",z$type,ignore.case=TRUE))
    return(list(ok=FALSE,reason=paste("Contenido no HTML:",z$type)))
  doc <- tryCatch(read_html(z$body),error=function(e)NULL)
  if (is.null(doc)) return(list(ok=FALSE,reason="HTML ilegible"))
  list(ok=TRUE,doc=doc,url=z$url,status=z$status)
}
resolve_url <- function(base, href) {
  if (is.na(href) || !nzchar(trimws(href))) return(NA_character_)
  tryCatch(xml2::url_absolute(trimws(href),base),error=function(e)NA_character_)
}

# ----------------------------- MOTORES DE BÚSQUEDA -----------------------------
# Preferencia: Brave, Bing o Google CSE mediante claves oficiales.
# DuckDuckGo HTML se ofrece como fallback sujeto a robots.txt y a sus condiciones.
search_brave <- function(q,n=10) {
  if (!nzchar(BRAVE_KEY)) return(data.frame())
  z <- http_get("https://api.search.brave.com/res/v1/web/search",
    query=list(q=q,count=min(n,20)),use_cache=TRUE,
    headers=list(`X-Subscription-Token`=BRAVE_KEY,Accept="application/json"))
  if (z$status!=200) stop("Brave Search HTTP ",z$status)
  a <- fromJSON(z$body,simplifyDataFrame=TRUE)$web$results
  if (is.null(a) || !nrow(as.data.frame(a))) return(data.frame())
  data.frame(query=q,title=a$title,url=a$url,snippet=a$description,
             engine="Brave",stringsAsFactors=FALSE)
}
search_bing <- function(q,n=10) {
  if (!nzchar(BING_KEY)) return(data.frame())
  z <- http_get("https://api.bing.microsoft.com/v7.0/search",
                query=list(q=q,count=min(n,20)),use_cache=TRUE,
                headers=list(`Ocp-Apim-Subscription-Key`=BING_KEY))
  if (z$status!=200) stop("Bing Search HTTP ",z$status)
  a <- fromJSON(z$body,simplifyDataFrame=TRUE)$webPages$value
  if (is.null(a) || !length(a)) return(data.frame())
  data.frame(query=q,title=vapply(a,`[[`,"","name"),url=vapply(a,`[[`,"","url"),
    snippet=vapply(a,function(x)x$snippet %||% "", ""),engine="Bing",stringsAsFactors=FALSE)
}
search_google <- function(q,n=10) {
  if (!nzchar(GOOGLE_KEY) || !nzchar(GOOGLE_CX)) return(data.frame())
  z <- http_get("https://www.googleapis.com/customsearch/v1",
    query=list(key=GOOGLE_KEY,cx=GOOGLE_CX,q=q,num=min(n,10)),use_cache=TRUE)
  if (z$status!=200) stop("Google CSE HTTP ",z$status)
  a <- fromJSON(z$body,simplifyDataFrame=FALSE)$items
  if (!length(a)) return(data.frame())
  data.frame(query=q,title=vapply(a,`[[`,"","title"),url=vapply(a,`[[`,"","link"),
    snippet=vapply(a,function(x)x$snippet %||% "", ""),engine="Google CSE",stringsAsFactors=FALSE)
}
search_duckduckgo <- function(q,n=10) {
  u <- "https://html.duckduckgo.com/html/"
  if (!robots_allowed(u)) return(data.frame())
  z <- tryCatch(http_get(u,query=list(q=q),use_cache=TRUE),error=function(e)NULL)
  if (is.null(z) || z$status!=200) return(data.frame())
  d <- tryCatch(read_html(z$body),error=function(e)NULL)
  if (is.null(d)) return(data.frame())
  nodes <- html_elements(d,".result")
  if (!length(nodes)) return(data.frame())
  rows <- lapply(head(nodes,n),function(node) {
    a <- html_element(node,".result__a")
    s <- html_element(node,".result__snippet")
    data.frame(query=q,title=html_text2(a),url=html_attr(a,"href") %||% "",
      snippet=html_text2(s),engine="DuckDuckGo HTML",stringsAsFactors=FALSE)
  })
  do.call(rbind,rows)
}
search_web <- function(q,n=10) {
  engines <- list(search_brave,search_bing,search_google,search_duckduckgo)
  for (engine in engines) {
    x <- tryCatch(engine(q,n),error=function(e) {
      message("Motor ",deparse(substitute(engine)),": ",conditionMessage(e)); data.frame()
    })
    if (is.data.frame(x) && nrow(x)) return(x)
  }
  data.frame(query=character(),title=character(),url=character(),
             snippet=character(),engine=character())
}

# ----------------------------- MINERÍA Y CRAWLING ------------------------------
extract_page <- function(url,depth=0,source="seed") {
  p <- scrape_page(url)
  if (!p$ok) return(data.frame(url=url,final_url=url,status=NA,title="",description="",
    text="",word_count=0,links="",depth=depth,source=source,error=p$reason,
    stringsAsFactors=FALSE))
  d <- p$doc
  title <- html_text2(html_element(d,"title")) %||% ""
  desc <- html_attr(html_element(d,"meta[name='description']"),"content") %||% ""
  canonical <- html_attr(html_element(d,"link[rel='canonical']"),"href")
  final <- if (!is.na(canonical) && nzchar(canonical)) resolve_url(p$url,canonical) else p$url
  html_elements(d,"script,style,noscript,svg,nav,footer,header,form") |> xml_remove()
  txt <- html_text2(html_element(d,"body"))
  txt <- stringi::stri_replace_all_regex(txt,"\\s+"," ")
  anchors <- html_elements(d,"a[href]")
  links <- unique(vapply(head(anchors,MAX_LINKS_PAGE),function(a)
    resolve_url(p$url,html_attr(a,"href")),""))
  links <- links[!is.na(links) & grepl("^https?://",links)]
  data.frame(url=url,final_url=final,status=p$status,title=as.character(title),
    description=as.character(desc),text=as.character(txt),
    word_count=length(strsplit(trimws(txt),"\\s+")[[1]]),
    links=paste(links,collapse=" "),depth=depth,source=source,error="",
    stringsAsFactors=FALSE)
}
crawl_web <- function(seeds,max_pages=MAX_PAGES,max_depth=MAX_DEPTH) {
  queue <- data.frame(url=unique(seeds),depth=0L,source="seed",stringsAsFactors=FALSE)
  seen <- character(); pages <- list()
  while (nrow(queue)>0 && length(pages)<max_pages) {
    item <- queue[1,]; queue <- queue[-1,,drop=FALSE]
    u <- item$url[1]
    if (u %in% seen) next
    seen <- c(seen,u)
    message(sprintf("[%d/%d] Extrayendo %s",length(pages)+1,max_pages,u))
    page <- extract_page(u,item$depth,item$source)
    pages[[length(pages)+1]] <- page
    if (item$depth<max_depth && nzchar(page$links[1])) {
      nxt <- strsplit(page$links[1]," ",fixed=TRUE)[[1]]
      nxt <- nxt[!nxt %in% c(seen,queue$url)]
      # Limitar expansión al mismo dominio para evitar crawling indiscriminado.
      nxt <- nxt[host_of(nxt)==host_of(u)]
      if (length(nxt)) queue <- rbind(queue,data.frame(
        url=head(nxt,MAX_LINKS_PAGE),depth=item$depth+1L,source="crawl",
        stringsAsFactors=FALSE))
    }
  }
  if (!length(pages)) return(data.frame())
  do.call(rbind,pages)
}
evaluate_links <- function(pages) {
  if (!nrow(pages)) return(data.frame())
  out <- list()
  for (i in seq_len(nrow(pages))) {
    ls <- strsplit(pages$links[i] %||% "", " ",fixed=TRUE)[[1]]
    ls <- ls[nzchar(ls)]
    if (!length(ls)) next
    out[[length(out)+1]] <- data.frame(from=pages$final_url[i],to=ls,
      anchor_domain=host_of(ls),https=grepl("^https://",ls),
      stringsAsFactors=FALSE)
  }
  if (!length(out)) return(data.frame(from=character(),to=character(),
    anchor_domain=character(),https=logical()))
  unique(do.call(rbind,out))
}
link_quality <- function(pages,edges) {
  if (!nrow(pages)) return(data.frame())
  incoming <- if (nrow(edges)) table(edges$to) else integer()
  outgoing <- if (nrow(edges)) table(edges$from) else integer()
  n_in <- vapply(pages$final_url,function(u) if(u %in% names(incoming)) as.numeric(incoming[[u]]) else 0,numeric(1))
  n_out <- vapply(pages$final_url,function(u) if(u %in% names(outgoing)) as.numeric(outgoing[[u]]) else 0,numeric(1))
  data.frame(url=pages$final_url,title=pages$title,word_count=pages$word_count,
    https=grepl("^https://",pages$final_url),
    incoming=as.integer(n_in),
    outgoing=as.integer(n_out),
    crawl_depth=pages$depth,extract_ok=pages$error=="",
    stringsAsFactors=FALSE)
}
build_graph <- function(pages,edges) {
  if (!nrow(pages)) return(list(graph=make_empty_graph(),rank=data.frame()))
  nodes <- unique(c(pages$final_url,if(nrow(edges))edges$to))
  e <- if(nrow(edges)) edges[,c("from","to")] else data.frame(from=character(),to=character())
  g <- graph_from_data_frame(e,directed=TRUE,vertices=data.frame(name=nodes))
  pr <- page_rank(g,directed=TRUE)$vector
  ranks <- data.frame(url=names(pr),pagerank=as.numeric(pr),
                      indegree=degree(g,mode="in"),outdegree=degree(g,mode="out"),
                      stringsAsFactors=FALSE)
  ranks <- ranks[order(-ranks$pagerank),]
  list(graph=g,rank=ranks)
}

# -------------------------- PREPROCESAMIENTO DE TEXTO --------------------------
STOP_ES <- unique(c("para","como","desde","hasta","sobre","entre","cuando","donde",
 "porque","pero","esta","este","estos","estas","tambien","todo","toda","todos",
 "todas","unos","unas","una","uno","con","sin","por","del","las","los","que",
 "de","la","el","en","y","o","a","al","se","su","sus","es","son","ser","fue",
 "han","hay","más","muy","ya","no","si","lo","le","les","un","The","and","for",
 "with","from","this","that","are","was","were","have","has","will","into"))
preprocess_text <- function(pages) {
  if (!nrow(pages)) return(list(docs=data.frame(),terms=data.frame(),tfidf=data.frame()))
  docs <- pages[pages$error=="",c("final_url","title","text"),drop=FALSE]
  docs$clean <- tolower(docs$text)
  docs$clean <- stringi::stri_trans_general(docs$clean,"Latin-ASCII")
  docs$clean <- gsub("https?://\\S+|www\\.\\S+"," ",docs$clean)
  docs$clean <- gsub("[^a-z0-9 ]"," ",docs$clean)
  docs$clean <- gsub("\\b[0-9]+\\b"," ",docs$clean)
  docs$clean <- gsub("\\s+"," ",docs$clean)
  tokens <- do.call(rbind,lapply(seq_len(nrow(docs)),function(i) {
    w <- unlist(strsplit(trimws(docs$clean[i])," "))
    w <- w[nchar(w)>=3 & !(w %in% STOP_ES)]
    if(!length(w)) return(NULL)
    data.frame(url=docs$final_url[i],term=w,stringsAsFactors=FALSE)
  }))
  if (is.null(tokens) || !nrow(tokens))
    return(list(docs=docs,terms=data.frame(),tfidf=data.frame()))
  tf <- as.data.frame(table(tokens$url,tokens$term),stringsAsFactors=FALSE)
  names(tf) <- c("url","term","count"); tf <- tf[tf$count>0,]
  nd <- length(unique(tf$url))
  df <- aggregate(url~term,tf,function(x)length(unique(x)))
  names(df)[2] <- "document_frequency"
  tf <- merge(tf,df,by="term")
  tf$tf <- tf$count / ave(tf$count,tf$url,FUN=sum)
  tf$idf <- log((1+nd)/(1+tf$document_frequency))+1
  tf$tfidf <- tf$tf*tf$idf
  tf <- tf[order(-tf$tfidf),]
  list(docs=docs,terms=tokens,tfidf=tf)
}
top_terms <- function(x,n=30) {
  if (!nrow(x$tfidf)) return(data.frame(term=character(),score=numeric()))
  z <- aggregate(tfidf~term,x$tfidf,sum)
  z <- z[order(-z$tfidf),]; names(z)[2] <- "score"; head(z,n)
}

# ----------------------------- DATOS DE RIESGO --------------------------------
get_usgs <- function(days=7,minmag=2.5) {
  z <- http_get(USGS_URL,query=list(format="geojson",
    starttime=format(Sys.Date()-days),minmagnitude=minmag,
    minlatitude=BBOX[["south"]],maxlatitude=BBOX[["north"]],
    minlongitude=BBOX[["west"]],maxlongitude=BBOX[["east"]],
    orderby="time",limit=200))
  f <- fromJSON(z$body,simplifyVector=FALSE)$features
  if (!length(f)) return(data.frame())
  data.frame(mag=vapply(f,function(x)as.numeric(x$properties$mag %||% NA),0.0),
    lugar=vapply(f,function(x)as.character(x$properties$place %||% ""), ""),
    hora=as.POSIXct(vapply(f,function(x)as.numeric(x$properties$time %||% NA),0.0)/1000,
      origin="1970-01-01",tz="America/La_Paz"),
    lon=vapply(f,function(x)as.numeric(x$geometry$coordinates[[1]]),0.0),
    lat=vapply(f,function(x)as.numeric(x$geometry$coordinates[[2]]),0.0),
    prof=vapply(f,function(x)as.numeric(x$geometry$coordinates[[3]] %||% NA),0.0),
    url=vapply(f,function(x)as.character(x$properties$url %||% ""), ""),stringsAsFactors=FALSE)
}
get_firms <- function(days=1) {
  if (!nzchar(FIRMS_KEY)) return(data.frame())
  u <- sprintf("https://firms.modaps.eosdis.nasa.gov/api/area/csv/%s/VIIRS_SNPP_NRT/%s/%d",
               FIRMS_KEY,FIRMS_AREA,days)
  z <- http_get(u)
  d <- read.csv(text=z$body,stringsAsFactors=FALSE)
  if (!"latitude" %in% names(d)) stop("Respuesta NASA FIRMS inesperada; revisa la clave.")
  out <- data.frame(lat=d$latitude,lon=d$longitude,fecha=d$acq_date,
    hora=sprintf("%04d",as.integer(d$acq_time)),conf=as.character(d$confidence),
    frp=as.numeric(d$frp),stringsAsFactors=FALSE)
  out[out$conf!="l",,drop=FALSE]
}
get_abc <- function() {
  p <- scrape_page(ABC_URL)
  if (!p$ok) return(data.frame(aviso=p$reason))
  txt <- html_text2(html_element(p$doc,"body"))
  lines <- trimws(unlist(strsplit(txt,"\n")))
  lines <- unique(lines[grepl("bloque|transitab|restric|corte|habilitad",lines,ignore.case=TRUE) &
                          nchar(lines)>10 & nchar(lines)<350])
  if (!length(lines)) data.frame(aviso="Portal dinámico: consultar mapa oficial ABC.")
  else data.frame(aviso=head(lines,30))
}
dist_km <- function(a,b,c,d) {
  r<-pi/180; h<-sin((c-a)*r/2)^2+cos(a*r)*cos(c*r)*sin((d-b)*r/2)^2
  12742*asin(sqrt(h))
}

# ------------------------------- BÚSQUEDA --------------------------------------
SEARCH_QUERIES <- c("Bolivia bloqueos carreteras transitabilidad hoy",
 "site:sicoes.gob.bo/portal Convocatorias Búsqueda procesos de contratación",
 "Bolivia incendios focos de calor NASA",
 "Bolivia sismos actividad sísmica",
 "Bolivia emergencias desastres naturales alertas oficiales")
search_results <- do.call(rbind,lapply(SEARCH_QUERIES,function(q) {
  message("Buscando: ",q); search_web(q,10)
}))
if (is.null(search_results) || !nrow(search_results)) search_results <- data.frame(
 query=character(),title=character(),url=character(),snippet=character(),engine=character())
search_urls <- unique(search_results$url[!is.na(search_results$url) & grepl("^https?://", search_results$url, ignore.case=TRUE)])
texto_noticia <- paste(search_results$title, search_results$snippet, sep=" ")
es_bloqueo <- grepl("bloque|corte|cerrad|restric|transitabilidad", texto_noticia, ignore.case=TRUE)
bloqueos <- search_results[es_bloqueo, , drop=FALSE]
if (nrow(bloqueos)) {
  bloqueos_mapa <- do.call(rbind, lapply(seq_len(nrow(bloqueos)), function(i) {
    texto <- sin_acentos(paste(bloqueos$title[i], bloqueos$snippet[i]))
    hit <- which(vapply(sin_acentos(CIUDADES$nombre), function(ciudad) grepl(ciudad, texto, fixed=TRUE), logical(1)))
    if (!length(hit)) return(NULL)
    ciudad <- CIUDADES[hit[1], , drop=FALSE]
    data.frame(ciudad=ciudad$nombre, lat=ciudad$lat, lon=ciudad$lon,
      titulo=as.character(bloqueos$title[i]), url=as.character(bloqueos$url[i]),
      resumen=as.character(bloqueos$snippet[i]), stringsAsFactors=FALSE)
  }))
} else bloqueos_mapa <- NULL
if (is.null(bloqueos_mapa)) bloqueos_mapa <- data.frame(ciudad=character(),lat=numeric(),lon=numeric(),titulo=character(),url=character(),resumen=character())
write_csv_safe(bloqueos_mapa, "bloqueos_noticias.csv")
seed_urls <- unique(c(unname(SEED_URLS),head(search_urls,MAX_SEEDS)))
seed_urls <- head(seed_urls,MAX_SEEDS)

# ----------------------------- PIPELINE DE MINERÍA -----------------------------
pages <- crawl_web(seed_urls,MAX_PAGES,MAX_DEPTH)
edges <- evaluate_links(pages)
text_model <- preprocess_text(pages)
network <- build_graph(pages,edges)
quality <- link_quality(pages,edges)
terms <- top_terms(text_model,40)

# ----------------------------- HTML INTERACTIVO -------------------------------
theme_css <- "
:root{color-scheme:dark}body{background:#0b1118;color:#e8edf2;font-family:system-ui,Segoe UI,sans-serif}
.hero{padding:26px 30px;border-bottom:5px solid transparent;border-image:linear-gradient(90deg,#d52b1e 33%,#f9e300 33% 66%,#007934 66%) 1}
h1{color:#f9e300;letter-spacing:.16em;margin:0}.sub{color:#aab6c2}
.grid{display:grid;grid-template-columns:repeat(auto-fit,minmax(190px,1fr));gap:14px;margin:20px 0}
.kpi,.panel{background:#141e29;border:1px solid #293746;border-radius:14px;padding:16px;margin:14px 0}
.kpi b{font-size:1.8rem;display:block;color:#f9c74f}.kpi span{color:#aab6c2;font-size:.85rem}
a{color:#f9c74f}nav{display:flex;gap:8px;flex-wrap:wrap;margin:15px 0}
nav a{padding:8px 12px;background:#1c2a38;border-radius:8px;text-decoration:none}
.table{color:#e8edf2!important}.dataTables_wrapper{overflow-x:auto}.note{color:#aab6c2;font-size:.88rem}
iframe{max-width:100%}
"
make_table <- function(x,rows=20) {
  if (!is.data.frame(x) || !nrow(x)) return(tags$p("Sin registros para mostrar.",class="note"))
  DT::datatable(head(x,rows),rownames=FALSE,filter="top",
    options=list(pageLength=10,scrollX=TRUE,autoWidth=TRUE),
    class="compact stripe hover",escape=TRUE)
}
save_widget <- function(widget,name) {
  path <- file.path(OUT_DIR,name)
  htmlwidgets::saveWidget(widget,path,selfcontained=FALSE,libdir="libs",title=APP_NAME)
  tags$iframe(src=name,style="width:100%;height:620px;border:0;border-radius:10px;")
}
# Mapa de Bolivia con ciudades, bloqueos reportados, sismos y focos.
map <- leaflet(options=leafletOptions(preferCanvas=TRUE)) |>
  addProviderTiles(providers$OpenStreetMap.Mapnik, group="Mapa base · OpenStreetMap") |>
  fitBounds(BBOX[["west"]],BBOX[["south"]],BBOX[["east"]],BBOX[["north"]]) |>
  addCircleMarkers(data=CIUDADES,lng=~lon,lat=~lat,radius=4,color="#ffffff",
                   label=~nombre,group="Ciudades")
sismos <- tryCatch(get_usgs(),error=function(e){message("USGS: ",conditionMessage(e));data.frame()})
focos <- tryCatch(get_firms(),error=function(e){message("FIRMS: ",conditionMessage(e));data.frame()})
abc <- tryCatch(get_abc(),error=function(e)data.frame(aviso=conditionMessage(e)))
if(nrow(sismos)) map <- map |> addCircleMarkers(data=sismos,lng=~lon,lat=~lat,
  radius=~pmax(5,mag*3),color="#f9e300",fillOpacity=.55,
  popup=~paste0("M",mag," · ",htmlEscape(lugar)),group="Sismos")
if(nrow(focos)) map <- map |> addCircleMarkers(data=focos,lng=~lon,lat=~lat,
  radius=3,color="#ff6a00",stroke=FALSE,fillOpacity=.7,
  popup=~paste0("Foco térmico · ",fecha," · confianza ",conf),group="Focos de calor")
if(nrow(bloqueos_mapa)) map <- map |> addCircleMarkers(data=bloqueos_mapa,lng=~lon,lat=~lat,
  radius=9,color="#ff4d4d",fillOpacity=.85,
  popup=~paste0("<strong>Posible bloqueo reportado</strong><br>",htmlEscape(titulo),
    "<br>Ubicación aproximada: ",htmlEscape(ciudad),"<br><a href=\"",htmlEscape(url),
    "\" target=\"_blank\" rel=\"noopener\">Abrir noticia</a>"),group="Bloqueos reportados")
map <- map |> addLayersControl(overlayGroups=c("Ciudades","Sismos","Focos de calor","Bloqueos reportados"),
  options=layersControlOptions(collapsed=FALSE))
map_iframe <- save_widget(map,"mapa.html")

# Grafo interactivo: nodos enlazados, tamaño por PageRank.
if(vcount(network$graph)>0) {
  g <- network$graph
  nodes <- data.frame(id=V(g)$name,label=substr(V(g)$name,1,65),
    value=as.numeric(network$rank$pagerank[match(V(g)$name,network$rank$url)] %||% 1)*1000,
    title=V(g)$name,stringsAsFactors=FALSE)
  nodes$value[!is.finite(nodes$value)] <- 1
  ee <- as_data_frame(g,what="edges")
  if(!nrow(ee)) ee <- data.frame(from=character(),to=character())
  graph_widget <- visNetwork(nodes,ee,height="580px",width="100%") |>
    visNodes(shape="dot",scaling=list(min=8,max=35)) |>
    visEdges(arrows="to",smooth=TRUE) |>
    visOptions(highlightNearest=TRUE,nodesIdSelection=TRUE) |>
    visPhysics(solver="forceAtlas2Based")
  graph_iframe <- save_widget(graph_widget,"grafo.html")
} else graph_iframe <- tags$p("No se pudo construir un grafo: faltan páginas/enlaces.")

kpi <- function(value,label) tags$div(class="kpi",tags$b(value),tags$span(label))
nav <- tags$nav(
  tags$a(href="#riesgos","Riesgos"),tags$a(href="#busqueda","Motores de búsqueda"),
  tags$a(href="#mineria","Minería web"),tags$a(href="#grafo","Grafo y PageRank"),
  tags$a(href="#enlaces","Evaluación de enlaces"),tags$a(href="#metodo","Método y fuentes"))
search_form <- tags$form(action="https://duckduckgo.com/",method="get",target="_blank",
  tags$label("Buscar en la web: "),tags$input(type="search",name="q",placeholder="Consulta…"),
  tags$button(type="submit","Buscar"))
page <- tags$html(lang="es",
  tags$head(tags$meta(charset="utf-8"),tags$meta(name="viewport",content="width=device-width, initial-scale=1"),
    tags$title("MAPACHE · Minería web y riesgos de Bolivia"),tags$style(HTML(theme_css))),
  tags$body(
    tags$header(class="hero",tags$h1("MAPACHE"),
      tags$p(class="sub","Minería web, análisis de enlaces y monitoreo de riesgos · Bolivia"),
      tags$p(class="note",paste("Generado:",format(Sys.time(),"%d/%m/%Y %H:%M:%S %Z")))),
    tags$main(class="container",
      nav,
      tags$section(id="riesgos",tags$h2("Panorama de riesgos"),
        tags$div(class="grid",kpi(if(nrow(sismos))nrow(sismos) else "—","Sismos M≥2.5 · últimos 7 días"),
          kpi(if(nrow(focos))nrow(focos) else if(!nzchar(FIRMS_KEY))"Sin clave" else "0",
              "Focos térmicos VIIRS · últimas 24 h"),
          kpi(if(nrow(pages))sum(pages$error=="") else 0,"Páginas extraídas correctamente"),
          kpi(if(nrow(edges))nrow(edges) else 0,"Enlaces identificados")),
        tags$div(class="panel",tags$h3("Mapa interactivo"),map_iframe),
        tags$div(class="panel",tags$h3("Sismos recientes"),make_table(sismos,30)),
        tags$div(class="panel",tags$h3("Focos de calor"),make_table(focos,50)),
        tags$div(class="panel",tags$h3("Transitabilidad ABC"),
          tags$p("El portal puede renderizar datos dinámicamente. Verifica siempre el mapa oficial."),
          tags$a(href=ABC_URL,target="_blank","Abrir ABC ↗"),make_table(abc,30))),
      tags$section(id="busqueda",tags$h2("Motores de búsqueda y descubrimiento"),
        tags$div(class="panel",search_form,
          tags$p(class="note","El pipeline prueba Brave, Bing y Google CSE si hay claves configuradas; usa DuckDuckGo HTML como alternativa cuando el acceso está permitido."),
          make_table(search_results,100))),
      tags$section(id="mineria",tags$h2("Minería web y preprocesamiento"),
        tags$div(class="panel",tags$h3("Corpus extraído"),make_table(
          if(nrow(pages))pages[,c("final_url","title","word_count","depth","status","error")] else pages,100)),
        tags$div(class="panel",tags$h3("Términos relevantes (TF-IDF)"),make_table(terms,40))),
      tags$section(id="grafo",tags$h2("Grafo dirigido y PageRank"),
        tags$div(class="panel",tags$p(class="note","Cada nodo representa una URL; las aristas representan hipervínculos salientes. PageRank se calcula sobre el grafo dirigido observado."),
          graph_iframe,make_table(head(network$rank,50),50))),
      tags$section(id="enlaces",tags$h2("Evaluación de enlaces"),
        tags$div(class="panel",tags$p(class="note","Indicadores: HTTPS, profundidad de rastreo, cantidad de palabras, enlaces entrantes/salientes y extracción satisfactoria. No equivalen a una auditoría de seguridad ni certifican la veracidad de una fuente."),
          make_table(quality,100),tags$h3("Aristas extraídas"),make_table(edges,100))),
      tags$section(id="metodo",tags$h2("Método, configuración y fuentes"),
        tags$div(class="panel",
          tags$h3("Flujo de procesamiento"),
          tags$ol(tags$li("Descubrimiento: motores de búsqueda y semillas públicas."),
            tags$li("Control de acceso: HTTPS, robots.txt, identificación, pausas, caché y límites de rastreo."),
            tags$li("Extracción: título, descripción, texto, URL canónica e hipervínculos."),
            tags$li("Preprocesamiento: normalización Unicode, minúsculas, eliminación de URL/puntuación/números y stopwords."),
            tags$li("Minería: frecuencias, TF-IDF, grafo dirigido, centralidad y PageRank."),
            tags$li("Evaluación: disponibilidad HTTP, HTTPS, profundidad, volumen textual y estructura de enlaces."),
            tags$li("Visualización: páginas HTML interactivas independientes de Shiny.")),
          tags$h3("Fuentes operativas"),
          tags$ul(tags$li(tags$a(href=ABC_URL,target="_blank","ABC · Transitabilidad")),
            tags$li(tags$a(href="https://firms.modaps.eosdis.nasa.gov/",target="_blank","NASA FIRMS · focos de calor")),
            tags$li(tags$a(href="https://earthquake.usgs.gov/",target="_blank","USGS · sismos"))),
          tags$p(class="note","Limitaciones: los resultados dependen de disponibilidad de red, claves de API, políticas robots.txt y contenido accesible. Los buscadores pueden cambiar sus interfaces; no se eluden autenticaciones, CAPTCHA ni controles de acceso. La información es orientativa y debe contrastarse con autoridades."))),
      tags$footer(class="note","MAPACHE · Herramienta de apoyo. No reemplaza alertas ni instrucciones oficiales.")
    )
  )
)
htmltools::save_html(page,file.path(OUT_DIR,"index.html"),libdir="libs")
# Exportables para análisis reproducible y auditoría.
write_csv_safe(search_results,"search_results.csv")
write_csv_safe(pages[,setdiff(names(pages),"text"),drop=FALSE],"pages.csv")
write_csv_safe(edges,"edges.csv")
write_csv_safe(quality,"link_quality.csv")
write_csv_safe(network$rank,"pagerank.csv")
write_csv_safe(terms,"tfidf_terms.csv")
write_csv_safe(sismos,"earthquakes.csv")
write_csv_safe(focos,"thermal_anomalies.csv")
write_csv_safe(abc,"abc_notices.csv")
writeLines(toJSON(list(generated=as.character(Sys.time()),pages=nrow(pages),
  extracted=sum(pages$error==""),edges=nrow(edges),search_results=nrow(search_results),
  pagerank=network$rank),auto_unbox=TRUE,pretty=TRUE),
  file.path(OUT_DIR,"data","run_summary.json"))
cat("\nMAPACHE terminado.\nHTML: ",normalizePath(file.path(OUT_DIR,"index.html"),winslash="/"),
    "\nDatos: ",normalizePath(file.path(OUT_DIR,"data"),winslash="/"),"\n",sep="")
