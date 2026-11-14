# La Pala · paquete de entrega

Este paquete contiene la aplicación local completa y una copia estática para GitHub Pages. Ambas comparten el catálogo publicado; su modo de actualización es distinto.

## Aplicación local completa

Desde esta carpeta, instala R y las dependencias indicadas por `start.R`, luego ejecuta:

```powershell
Rscript start.R
```

El servidor local atiende la interfaz, el chatbot, los workers, el análisis y los registros en `logs/`. Para el botón de recolección administrativa configura `LA_PALA_ADMIN_PASSWORD` en el entorno local. No incluyas contraseñas ni el archivo `.Renviron` en una entrega o repositorio.

## GitHub Pages

`Chat_Bot-Public-Services/` es el sitio estático publicado por `.github/workflows/pages.yml`. No necesita un paso de compilación: GitHub Actions publica sus archivos. El chatbot busca dentro de `data/catalogo.json` y MAPACHE presenta la última salida generada. Pages no ejecuta R, no hace rastreo en vivo y no escribe los logs locales; para actualizarlo hay que regenerar el catálogo y publicar los archivos.

## Render y monitoreo

La carpeta raíz incluye `render.yaml` y `Dockerfile` para alojar la app R completa en el plan gratuito de Render. La guía [DEPLOY_RENDER.md](DEPLOY_RENDER.md) explica el Blueprint, la clave secreta de MAPACHE, el almacenamiento temporal y cómo programar comprobaciones de salud con UptimeRobot o cron-job.org.

## Estructura principal

- `backend/`: API y lógica de la aplicación.
- `scraping/`: recolector y fuentes públicas configuradas.
- `search/`: normalización y ranking de consultas.
- `analisis/`: análisis del catálogo, SEO y trabajos en segundo plano.
- `frontend/`: interfaz de la aplicación local y salida de MAPACHE.
- `Chat_Bot-Public-Services/`: cliente estático para Pages.
- `data/`: catálogo local.
- `mapache_site/`: generador de la vista autónoma MAPACHE.
- `INFORMES/`: informes PDF y requerimientos del proyecto.

Los errores de ejecución se crean localmente en `logs/fallas.txt`. La carpeta `logs/` se excluye del paquete para no distribuir estados de una máquina o una ejecución anterior.
