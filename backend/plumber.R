# Entrada opcional con plumber (LA_PALA_MOTOR=plumber). La lógica vive en nucleo.R.
suppressPackageStartupMessages(library(plumber))
source(if (file.exists("backend/nucleo.R")) "backend/nucleo.R" else "nucleo.R", local = .GlobalEnv)

#* Cabeceras de seguridad para archivos servidos por el navegador.
#* @filter seguridad
function(req, res) {
  for (n in names(CABECERAS)) res$setHeader(n, CABECERAS[[n]])
  plumber::forward()
}

#* Estado del servicio y del catálogo local.
#* @serializer unboxedJSON list(na = "null", null = "null")
#* @get /api/health
function() RUTAS[["/api/health"]]()

#* Categorías de oportunidades admitidas.
#* @serializer unboxedJSON list(na = "null", null = "null")
#* @get /api/categories
function() RUTAS[["/api/categories"]]()

#* Lista oportunidades vigentes.
#* @param categoria Categoría del catálogo.
#* @param limit Cantidad máxima de resultados.
#* @serializer unboxedJSON list(na = "null", null = "null")
#* @get /api/opportunities
function(categoria = "", limit = 6L) RUTAS[["/api/opportunities"]](categoria = categoria, limit = limit)

#* Compatibilidad con el antiguo listado de empleos.
#* @param limit Cantidad máxima de resultados.
#* @serializer unboxedJSON list(na = "null", null = "null")
#* @get /api/jobs
function(limit = 6L) RUTAS[["/api/jobs"]](limit = limit)

#* Busca en títulos, descripciones, categorías y fechas límite.
#* @param q Consulta en lenguaje natural.
#* @param categoria Categoría opcional.
#* @param limit Cantidad máxima de resultados.
#* @serializer unboxedJSON list(na = "null", null = "null")
#* @get /api/search
function(q = "", categoria = "", limit = 6L) RUTAS[["/api/search"]](q = q, categoria = categoria, limit = limit)

#* Responde consultas como "becas antes de fin de mes".
#* @param q Consulta de la persona.
#* @param limit Cantidad máxima de resultados.
#* @serializer unboxedJSON list(na = "null", null = "null")
#* @get /api/chat
function(q = "", limit = 6L) RUTAS[["/api/chat"]](q = q, limit = limit)

#* Lista observaciones de precios con procedencia y fecha.
#* @param q Nombre o descripción del producto.
#* @param departamento Lugar de la observación.
#* @param limit Cantidad máxima de resultados.
#* @serializer unboxedJSON list(na = "null", null = "null")
#* @get /api/prices
function(q = "", departamento = "", limit = 12L) RUTAS[["/api/prices"]](q = q, departamento = departamento, limit = limit)

#* Resumen real de publicaciones, cierres y mediciones.
#* @serializer unboxedJSON list(na = "null", null = "null")
#* @get /api/stats
function() RUTAS[["/api/stats"]]()

#* Estadísticas, comparativas, relaciones, grafo, PageRank y fraude del catálogo.
#* @serializer unboxedJSON list(na = "null", null = "null")
#* @get /api/analisis/resumen
function() RUTAS[["/api/analisis/resumen"]]()

#* Compara el SEO de hasta 7 páginas HTTPS públicas (separadas por coma).
#* @param urls Direcciones separadas por coma.
#* @serializer unboxedJSON list(na = "null", null = "null")
#* @get /api/analisis/sitio
function(urls = "", res) RUTAS[["/api/analisis/sitio"]](urls = urls, res = res)

#* Rastreo corto de un sitio con PageRank de sus enlaces internos.
#* @param url Dirección HTTPS inicial.
#* @param max Páginas máximas (2 a 12).
#* @serializer unboxedJSON list(na = "null", null = "null")
#* @get /api/analisis/rastreo
function(url = "", max = 8L, res) RUTAS[["/api/analisis/rastreo"]](url = url, max = max, res = res)

#* Inicia una recolección manual protegida por la clave privada del servidor.
#* @serializer unboxedJSON list(na = "null", null = "null")
#* @post /api/admin/collect
function(req, res) RUTAS[["/api/admin/collect"]](req = req, res = res)

#* Consulta el estado de la recolección y sus estadísticas finales.
#* @param solicitud_id Identificador de la ejecución que se desea consultar.
#* @serializer unboxedJSON list(na = "null", null = "null")
#* @get /api/collection-status
function(solicitud_id = "") RUTAS[["/api/collection-status"]](solicitud_id = solicitud_id)

#* Fuentes registradas y su estado de configuración.
#* @serializer unboxedJSON list(na = "null", null = "null")
#* @get /api/sources
function() RUTAS[["/api/sources"]]()

#* Estado o resultado de un análisis en segundo plano.
#* @param id Identificador del trabajo.
#* @serializer unboxedJSON list(na = "null", null = "null")
#* @get /api/analisis/trabajo
function(id = "") RUTAS[["/api/analisis/trabajo"]](id = id)

#* Archivos de la interfaz.
#* @assets ../frontend /
list()
