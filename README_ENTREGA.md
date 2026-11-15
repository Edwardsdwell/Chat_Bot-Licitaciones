# La Pala · paquete de entrega

**Objetivo:** reunir y verificar oportunidades públicas de empleo, becas y compras en Bolivia, y permitir buscarlas con un catálogo actualizado.

El chat ayuda a consultar ese catálogo; MAPACHE y la analítica SEO sirven para revisar fuentes y enlaces.

## Aplicación local completa

Desde esta carpeta, instala R y las dependencias indicadas por `start.R`, luego ejecuta:

```powershell
Rscript start.R
```

El servidor local atiende la interfaz, el chatbot, los workers, el análisis y los registros en `logs/`. Configura `LA_PALA_ADMIN_PASSWORD` solo en el entorno privado del servidor.

## GitHub Pages

`Chat_Bot-Public-Services/` es el sitio estático publicado por su propio workflow en `Chat_Bot-Public-Services/.github/workflows/pages.yml`. No necesita un paso de compilación: GitHub Actions publica sus archivos. El chatbot busca dentro de `data/catalogo.json` y MAPACHE presenta la última salida generada. Pages no ejecuta R, no hace rastreo en vivo y no escribe los logs locales; para actualizarlo hay que regenerar el catálogo y publicar los archivos.

## Render y monitoreo

La carpeta raíz incluye `render.yaml` y `Dockerfile` para alojar la app R completa en el plan gratuito de Render. La guía [DEPLOY_RENDER.md](DEPLOY_RENDER.md) explica el Blueprint, la clave secreta de MAPACHE, el almacenamiento temporal y cómo programar comprobaciones de salud con UptimeRobot o cron-job.org.

## Estructura principal

- `backend/`: API y lógica de la aplicación.
- `scraping/`: recolector y fuentes públicas configuradas.
- `analisis/`: ranking, SEO y trabajos en segundo plano.
- `frontend/`: interfaz de la aplicación local.
- `Chat_Bot-Public-Services/`: cliente estático para Pages.
- `data/`: catálogo local.
- `mapache_site/`: generador R de MAPACHE; la salida única vive en `Chat_Bot-Public-Services/mapache/`.

Los errores de ejecución se crean localmente en `logs/fallas.txt`. La carpeta `logs/` se excluye del paquete para no distribuir estados de una máquina o una ejecución anterior.
