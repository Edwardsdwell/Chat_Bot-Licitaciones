# Despliegue de La Pala en Render

El servicio Render ejecuta la aplicación completa de R: API, chat, recolección, analítica y MAPACHE. GitHub Pages conserva la edición estática y no reemplaza este backend.

`MARK IV` es la carpeta fuente y no tiene todavía un repositorio Git propio. `Chat_Bot-Public-Services/` contiene el repositorio independiente de GitHub Pages. Para desplegar la app R en Render, conecta un repositorio cuya raíz contenga `Dockerfile` y `render.yaml`; no empujes la carpeta Pages como si fuera la raíz de MARK IV ni la anides como submódulo accidental.

El repositorio independiente `Chat_Bot-Public-Services/` publica su sitio estático con su propio workflow de Pages. Pages no ejecuta R; el servicio completo y las consultas en vivo se atienden desde Render.

## Preparar el repositorio

Sube la carpeta raíz de `MARK IV` a un repositorio GitHub y deja `render.yaml` junto al `Dockerfile`. En Render crea un **Blueprint** desde ese repositorio y revisa los valores de `render.yaml` antes de confirmar el servicio.

El Blueprint usa un servicio web Docker en el plan gratuito. Render duerme estos servicios tras 15 minutos sin tráfico, tarda alrededor de un minuto en despertar y no ofrece disco persistente en este plan. La configuración guarda catálogo y registros en `/tmp`; se pueden perder al dormir, reiniciar o desplegar. El catálogo inicial se copia de nuevo cuando la app arranca.

## Clave MAPACHE

Después de crear el servicio, abre **Environment → Add Environment Variable** y define `MAPACHE_ACCESS_PASSWORD` con una clave privada. El valor `sync: false` evita guardarla en Git. El formulario administrativo la envía sobre HTTPS en el encabezado de autorización y el servidor la compara en memoria. Para instalaciones anteriores también se acepta `LA_PALA_ADMIN_PASSWORD`.

No agregues valores reales al `.env.example`, al repositorio, a un ZIP público ni a capturas. Usa el apartado **Environment** de Render para rotar la clave.

## Verificar el servicio

Render realiza comprobaciones HTTP en `/api/health` durante el despliegue y mientras el servicio está activo. Cuando el estado sea **Live**, abre `https://<nombre-del-servicio>.onrender.com/api/health`. La respuesta debe incluir `"estado":"disponible"` y `"catalogo":true`; la interfaz está en la raíz del mismo dominio.

## Mantener actividad y recibir alertas

UptimeRobot Free permite comprobar cada 5 minutos; cron-job.org también permite solicitudes programadas y acepta una frecuencia de hasta una por minuto. Para este servicio basta con una solicitud GET cada 5 minutos a `/api/health`; no hace falta golpearlo cada minuto.

### UptimeRobot

1. Crea un monitor **HTTP(s)** y ponle el nombre `La Pala · Render`.
2. URL: `https://<nombre-del-servicio>.onrender.com/api/health`.
3. Intervalo: **5 minutes**. Activa las notificaciones disponibles y guarda.
4. Confirma que la respuesta figure como **Up**.

### Alternativa: cron-job.org

1. Crea una cuenta y un **Cronjob** nuevo.
2. URL: la misma dirección `https://<nombre-del-servicio>.onrender.com/api/health` y método **GET**.
3. Programa cada **5 minutos**, ejecuta **Test run** y luego guarda.
4. Activa el aviso por correo ante fallos.

Una petición periódica cuenta como tráfico entrante y debería mantener el servicio activo mientras llegue antes del límite de inactividad. Esto no garantiza disponibilidad: el programador puede retrasarse o fallar y Render puede reiniciar o suspender un servicio gratuito. Una comprobación de 5 minutos usaría cerca de 720–744 horas de instancia en un mes de 30–31 días, de las 750 horas gratuitas compartidas por el espacio de Render. El monitor solo comprueba salud; no ejecuta el recolector.

## Variables de motores opcionales

Los conectores de búsqueda que requieren API se habilitan agregando sus claves al Environment de Render (`BRAVE_SEARCH_KEY`, `BING_SEARCH_KEY`, `GOOGLE_CSE_KEY` y `GOOGLE_CSE_CX`). Son opcionales; el recolector mantiene sus fuentes directas permitidas cuando no hay claves. El navegador no recibe esos secretos.
