# MAPACHE · Bolivia

MAPACHE reemplaza la aplicación Shiny por un sitio HTML interactivo generado con R.

## Ejecución

1. Instala R (4.2 o superior).
2. Instala dependencias:

```r
install.packages(c(
  "httr", "xml2", "robotstxt", "jsonlite", "leaflet",
  "DT", "visNetwork", "htmltools", "htmlwidgets"
))
```

3. Ejecuta desde la raíz del proyecto:

```bash
Rscript mapache_site/mapache_bolivia.R
```

Genera la única salida en `Chat_Bot-Public-Services/mapache/`. Pages la publica directamente y el servidor local la sirve bajo `/mapache/`. No hay servidor Shiny ni copias de salida guardadas en esta carpeta.

## Qué procesa

- Descubrimiento mediante Brave, Bing, Google CSE o DuckDuckGo HTML (según disponibilidad y permisos).
- Rastreo acotado y respetuoso: HTTPS, comprobación `robots.txt`, User-Agent identificable,
  caché, pausas entre solicitudes, límite de páginas y profundidad, y sin eludir controles.
- Extracción de título, descripción, URL canónica, texto y enlaces.
- Preprocesamiento de texto y ponderación TF-IDF.
- Grafo dirigido de hipervínculos y PageRank.
- Evaluación estructural de enlaces: HTTPS, enlaces entrantes/salientes, profundidad y volumen textual.
- Mapa interactivo de ciudades, sismos USGS y focos NASA FIRMS.
- Capa de posibles bloqueos basada en noticias encontradas: se marca una ciudad solo cuando el titular o resumen la nombra; el punto es aproximado y el popup conserva el enlace de la noticia. Se exporta a `data/bloqueos_noticias.csv`.
- Exportación CSV y JSON en el subdirectorio `data/` de la salida configurada.

## Configuración opcional

Variables de entorno: `MAPACHE_MAX_PAGES`, `MAPACHE_MAX_DEPTH`, `MAPACHE_MAX_SEEDS`,
`MAPACHE_DELAY_SEC` (8 segundos por defecto), `MAPACHE_CACHE_MIN`, `MAPACHE_TIMEOUT` y `MAPACHE_OUT`.

La búsqueda API requiere claves propias; sin ellas el programa prueba DuckDuckGo HTML
si `robots.txt` permite el acceso. Los buscadores pueden modificar endpoints o exigir
autenticación. La ejecución no garantiza que todas las fuentes respondan.

## Consideraciones

La información de riesgos es orientativa: confirma bloqueos, incendios y emergencias
con las autoridades. Un PageRank alto indica centralidad dentro del grafo observado,
no autoridad ni veracidad. La evaluación de enlaces es estructural, no un análisis
de ciberseguridad. El rastreador no accede a contenido privado, no resuelve CAPTCHA
ni evita límites de acceso.
