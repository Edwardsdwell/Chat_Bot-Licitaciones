# Notas del MVP · qué se cambió y cómo probarlo

## Problemas encontrados (con el log de Render del 15-nov-2026)
| Síntoma | Causa | Arreglo |
|---|---|---|
| `omitida · robots.txt no_verificable` en casi todo | Un timeout/DNS/5xx en `robots.txt` descartaba la fuente entera | Ahora se continúa con cautela (1 solo intento). 401/403/429 siguen bloqueando. `LA_PALA_ROBOTS_ESTRICTO=1` restaura el modo anterior |
| `la cadena de caracteres no está en un formato estándar inequívoco` (min-salud) | `as.Date()` y `as.POSIXct()` **lanzan error** con texto que no es fecha | `fecha_segura()`, `esta_vencida()` y `tryCatch` en `parsear_fecha_publicada()` |
| Catálogo lleno de menús ("Contrataciones Menores", "Licitación Pública") con el menú del sitio como descripción | Regex de títulos de sección no cubría "licitacion" en singular; sin filtro de menús | `es_texto_util()` + exigir plazo/señal de convocatoria en fichas; el catálogo viejo se purga al guardar |
| Título = "Contratación menor simple" (YPFB Transierra) | Se tomaba la columna de tipo | Se elige el texto más descriptivo que no sea tipo/estado/número |
| Gaceta (`http://`) nunca se leía | Solo se aceptaba HTTPS | `http://` permitido solo en dominios `.bo` |
| `mhe.gob.bo` 403; `mdrt`, `minculturas`, `justicia` sin DNS; `mmaya` timeout | Sitios caídos o que bloquean IPs de fuera de Bolivia | Quitados de `sources.json` (eran portadas). Los 403 se respetan |
| Cada despertar de Render lanzaba una recolección de >1 h | `start.R` la programaba al arrancar | `LA_PALA_RECOLECCION_AUTO=0` en `render.yaml`: Render solo sirve el catálogo |
| Recolección lenta | 12 páginas por fuente + 8 s entre solicitudes | 6 por defecto; fuentes con `"lista": true` no siguen enlaces si ya extrajeron ≥5 ítems |

## Nuevo
- `scraping/diagnostico.R`: informa por fuente robots/HTTP/tipo/tablas/tarjetas y guarda el HTML en `logs/muestras/`.
- `oportunidades_de_tarjetas()`: extractor para listados en `article/li/div` (Trabajito, Trabajopolis, agregadores).
- `.github/workflows/recolectar.yml`: recolección diaria 06:00 (Bolivia) en GitHub Actions, commit del catálogo, re-publicación de Pages, logs como artefacto.
- `tests/test_collect.R`: 18 pruebas sin red (`Rscript tests/test_collect.R`).
- `sources.json`: campo `lista: true` y SIGEP con `metodo: "navegador"` (deshabilitada: es una app JavaScript).

## Cómo seguir
1. Sube estos archivos y ejecuta en GitHub: *Actions → Recolectar oportunidades → Run workflow*.
2. Descarga el artefacto `logs-recoleccion` y mira `recoleccion.log`.
3. En tu PC o en Actions: `Rscript scraping/diagnostico.R` y pega `logs/diagnostico.csv`: con eso se escriben extractores exactos por sitio.

## Límites conocidos
- Las pruebas usan HTML de ejemplo; **no pude abrir los sitios reales** desde mi entorno. Falta validar cada fuente con `diagnostico.R`.
- GitHub Actions y Render salen desde EE. UU.: si un portal bloquea IPs extranjeras, hará falta un proxy o correr el recolector en una máquina en Bolivia.
- Sin paginación: solo se lee la primera página de cada listado.
- Al omitir `robots.txt` caído se actúa con cautela, pero es una decisión tuya: pon `LA_PALA_ROBOTS_ESTRICTO=1` si prefieres el modo estricto.
