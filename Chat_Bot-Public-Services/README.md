# La Pala

Chat estático para buscar oportunidades públicas de Bolivia en `data/catalogo.json`. Cada coincidencia lleva a su publicación original; el navegador no rastrea portales en vivo.

## Publicación

GitHub Actions publica este repositorio en GitHub Pages al actualizar `main`. Configura **Settings → Pages → Build and deployment → Source: GitHub Actions**.

## Contenido

- `index.html`, `styles.css`, `app.js`: interfaz y búsqueda local.
- `data/`: catálogo y fuentes que se publican.
- `mapache/`: última salida estática de MAPACHE.

Para actualizar el catálogo o MAPACHE, genera y revisa los datos antes de publicarlos. Las fechas, requisitos y estados deben confirmarse en la fuente oficial.
