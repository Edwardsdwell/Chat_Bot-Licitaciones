# Pruebas del recolector con HTML de ejemplo (no requieren red).
# Uso desde la raíz del proyecto:  Rscript tests/test_collect.R
Sys.setenv(LA_PALA_DATA_DIR = tempfile("lp_data"), LA_PALA_LOG_DIR = tempfile("lp_logs"))
source("scraping/collect.R", local = FALSE)
fallos <- 0L
verificar <- function(nombre, condicion) {
  ok <- isTRUE(condicion)
  if (!ok) fallos <<- fallos + 1L
  cat(sprintf("[%s] %s\n", if (ok) "OK " else "FALLA", nombre))
}
fuente <- list(id = "t", nombre = "Fuente de prueba", url = "https://ejemplo.gob.bo/lista", categoria = "contrataciones", lista = TRUE, oficial = TRUE)

# 1. Bug de fechas (min-salud): una cadena cualquiera ya no tumba la fuente
verificar("fecha_segura no falla con texto", is.na(fecha_segura("Pasar al contenido principal")))
verificar("parsear_fecha_publicada devuelve vacío con texto", identical(parsear_fecha_publicada("Pasar al contenido principal INSTITUCIONAL"), ""))
verificar("parsear_fecha_publicada acepta ISO", identical(parsear_fecha_publicada("2026-11-10T08:00:00Z"), "2026-11-10"))
verificar("esta_vencida con basura = FALSE", !esta_vencida("no es fecha"))

# 2. HTTP admitido solo en .bo
verificar("http://*.gob.bo válido", url_https_valida("http://www.gacetaoficialdebolivia.gob.bo/contenidos/escala"))
verificar("http://*.com rechazado", !url_https_valida("http://ejemplo.com/"))
verificar("mismo_dominio http/https", mismo_dominio("http://www.a.gob.bo/x", "https://a.gob.bo/y"))

# 3. robots.txt caído => continuar (antes: omitir la fuente)
obtener_real <- obtener
obtener <- function(url, accept = "", intento = 1L) NULL
rm(list = ls(envir = REGLAS_ROBOTS, all.names = TRUE), envir = REGLAS_ROBOTS)
reglas <- revisar_robots("https://caido.gob.bo/licitaciones")
verificar("robots sin respuesta => permitido", isTRUE(reglas$permitido) && isTRUE(reglas$sin_respuesta))
Sys.setenv(LA_PALA_ROBOTS_ESTRICTO = "1")
rm(list = ls(envir = REGLAS_ROBOTS, all.names = TRUE), envir = REGLAS_ROBOTS)
verificar("modo estricto conserva el bloqueo", !isTRUE(revisar_robots("https://caido.gob.bo/licitaciones")$permitido))
Sys.unsetenv("LA_PALA_ROBOTS_ESTRICTO")
obtener <- obtener_real

# 4. Menús ya no se convierten en oportunidades (caso BCB)
menu <- "Pasar al contenido principal INSTITUCIONAL HISTORIA El Banco Central de Bolivia Más de 100 años de Historia ESTRUCTURA ORGÁNICA Organigrama Directorio TRANSPARENCIA"
verificar("menú BCB descartado", !es_texto_util("Contrataciones Menores", menu))
verificar("título de sección descartado", !es_texto_util("Licitación Pública", "Convocatoria abierta de proveedores"))
verificar("convocatoria real aceptada", es_texto_util("Adquisición de equipos de cómputo para la gerencia", "CUCE 26-0123-00-1234567-1-1 Presupuesto Bs 120.000 Fecha límite 30/11/2026"))

html_menu <- xml2::read_html("<html><head><title>Contrataciones Menores</title></head><body><main><h1>Contrataciones Menores</h1><p>Texto largo de navegación y enlaces institucionales sin ninguna publicación concreta, solo un índice de secciones del sitio para orientar al visitante del portal.</p></main></body></html>")
verificar("índice de sección => NULL", is.null(extraer_oportunidad(html_menu, fuente, "https://ejemplo.gob.bo/?q=x")))
html_det <- xml2::read_html("<html><head><title>Convocatoria</title></head><body><main><h1>Servicio de limpieza de oficinas centrales gestión 2027</h1><p>Presupuesto referencial Bs 85.000. Fecha límite de presentación: 30/11/2026. Los proponentes deben presentar sus propuestas en la dirección indicada en el pliego de condiciones.</p></main></body></html>")
det <- extraer_oportunidad(html_det, fuente, "https://ejemplo.gob.bo/detalle/1")
verificar("ficha real => oportunidad con fecha", !is.null(det) && identical(det$fecha_limite, "2026-11-30"))

# 5. Tarjetas (bolsas de empleo / agregadores)
tarjetas <- paste0("<div class='card-job'><h3><a href='/empleo/", 1:4, "'>Asistente administrativo ", 1:4, " en La Paz</a></h3><p>Empresa Demo SRL · La Paz · Publicado hace ", 1:4, " días · Jornada completa</p></div>", collapse = "")
doc_t <- xml2::read_html(paste0("<html><body><nav><ul><li><a href='/'>Inicio</a></li><li><a href='/empleos'>Empleos disponibles</a></li></ul></nav><main>", tarjetas, "</main></body></html>"))
f_emp <- modifyList(fuente, list(categoria = "empleo_publico", url = "https://ejemplo.bo/empleos/la-paz"))
res_t <- oportunidades_de_tarjetas(doc_t, "https://ejemplo.bo/empleos/la-paz", f_emp)
verificar("4 tarjetas extraídas, sin menú", length(res_t) == 4L)
verificar("tarjeta con título y enlace de detalle", length(res_t) > 0 && grepl("Asistente administrativo 1", res_t[[1]]$titulo) && grepl("/empleo/1$", res_t[[1]]$url))

# 6. Tabla (licitaciones con enlace de detalle)
filas <- paste0("<tr><td>", 1:3, "</td><td>Adquisición de material de oficina lote ", 1:3, "</td><td>Fecha límite 3", 1:3, "/12/2026</td><td><a href='/proceso/", 1:3, "'>Ver</a></td></tr>", collapse = "")
doc_tab <- xml2::read_html(paste0("<html><body><table>", filas, "</table></body></html>"))
res_tab <- oportunidades_de_tabla(doc_tab, "https://ejemplo.gob.bo/lista", fuente)
verificar("tabla: 3 filas con fecha", length(res_tab) == 3L && identical(res_tab[[1]]$fecha_limite, "2026-12-31"))

# 7. Título = objeto de la contratación, no la columna de tipo (caso YPFB Transierra)
f_ypfb <- modifyList(fuente, list(url = "https://ypfbtransierra.com.bo/contrataciones/"))
doc_y <- xml2::read_html("<html><body><table><tr><td>5000006166</td><td>ADQUISICION DE JUGUETES NAVIDENOS</td><td>Bienes</td><td>Contratación menor simple</td><td>En presentación de propuestas</td><td><a href='/contrataciones/6166'>Ver</a></td></tr></table></body></html>")
res_y <- oportunidades_de_tabla(doc_y, "https://ypfbtransierra.com.bo/contrataciones/", f_ypfb)
verificar("YPFB: título es el objeto, no el tipo", length(res_y) == 1L && identical(res_y[[1]]$titulo, "ADQUISICION DE JUGUETES NAVIDENOS"))

cat(sprintf("\n%s\n", if (fallos == 0L) "TODAS LAS PRUEBAS PASARON" else paste(fallos, "prueba(s) fallaron")))
quit(status = if (fallos == 0L) 0L else 1L)
