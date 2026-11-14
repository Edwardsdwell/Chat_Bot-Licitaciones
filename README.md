# La Pala · Chatbot estático para GitHub Pages

Versión sin servidor del chatbot La Pala. El navegador busca localmente dentro de `data/catalogo.json`; no hay llamadas a una API ni recopilación directa desde los portales. Las fuentes están enlazadas para confirmar avisos, plazos y requisitos.

## Publicar

El workflow `.github/workflows/pages.yml` publica el sitio al hacer push a `main` o al ejecutarlo manualmente. En GitHub: **Settings → Pages → Build and deployment → Source: GitHub Actions**. La dirección de proyecto será `https://edwardsdwell.github.io/Chat_Bot-Public-Services/` si Pages está habilitado para el repositorio.

## Actualizar catálogo

Reemplaza `data/catalogo.json` por una exportación revisada. Solo publica datos cuya licencia y sensibilidad permitan distribución pública. El archivo de esta versión contiene los registros del catálogo local que estaban vigentes y verificados en la fecha de exportación. El sitio no los actualiza automáticamente.

## Alcance de Pages

GitHub Pages solo entrega archivos estáticos. Este chatbot ofrece una búsqueda local funcional sobre el catálogo publicado; no puede rastrear sitios, iniciar una recolección, consultar estado en vivo ni escribir logs del servidor. Para extracción programada hace falta un backend o una tarea externa que regenere el catálogo. No se evaden CAPTCHA, autenticación, robots.txt ni límites de las fuentes.

MAPACHE se incluye como sitio estático dentro de `mapache/`. La capa de bloqueos reflejará la última salida generada, no un feed en tiempo real.
