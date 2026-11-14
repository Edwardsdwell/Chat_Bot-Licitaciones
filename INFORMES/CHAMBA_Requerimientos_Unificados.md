## ENFOQUE VIGENTE DEL PRODUCTO

La Pala es un asistente en español para oportunidades y datos públicos de Bolivia. Este enfoque sustituye el alcance anterior centrado en la búsqueda general de empleo; se conservan de los requisitos siguientes las capacidades que sigan siendo útiles y compatibles.

Áreas actuales:

1. Convocatorias públicas y empleo del Estado.
2. Licitaciones y contrataciones públicas, incluidas las publicaciones de SICOES.
3. Concursos de mérito y selección institucional.
4. Becas, pasantías y convocatorias académicas para estudiantes, con fechas límite explícitas.
5. Fondos concursables y financiamiento para emprendimientos, ONG y organizaciones.
6. Seguimiento de precios locales con lugar, unidad, fuente y fecha para construir tendencias.
7. Normativa y publicaciones de la Gaceta Oficial.

El chatbot debe responder consultas prácticas como «¿Qué becas puedo pedir antes de fin de mes?». Debe usar fechas vigentes, distinguir plazos conocidos de datos ausentes y enlazar cada resultado con la publicación original. No puede inventar publicaciones, importes, requisitos ni fechas.

El idioma visible de la aplicación, la documentación y los mensajes del recolector será español. Se mantienen en su forma original los nombres de bibliotecas, funciones integradas, protocolos, direcciones web y campos técnicos que formen parte de interfaces o datos intercambiados.

La arquitectura debe permanecer ligera: interfaz web, API en R, recolección diaria conservadora y un catálogo local. Se priorizan páginas públicas, enlaces, datos estructurados y mapas del sitio. Si una fuente exige JavaScript, autenticación, CAPTCHA, permisos no claros o bloquea la automatización, se omite la página sin fabricar resultados ni intentar eludir la restricción. Las publicaciones no se retiran por una falla de lectura; la vigencia se decide comparando la fuente y su fecha límite.

Las páginas de precios se separan de las convocatorias y requieren historial propio para mostrar cambios. Las normas se guardan como referencias duraderas; las oportunidades con fecha límite se conservan hasta vencer o hasta que la fuente confirme su cierre. Los datos históricos se compactan según su tipo para mantener liviano el catálogo.

---

# La Pala · Lista histórica de requisitos
Metabuscador y chatbot de oportunidades públicas y datos locales de Bolivia

1\. OBJETIVO Y ALCANCE  
General:  
1.1  Levantar un servicio de chatbot/metabuscador para buscar trabajo en el territorio boliviano.  
Específicos:  
1.2  Incluir ofertas publicadas en Bolivia y ofertas remotas disponibles para residentes bolivianos.  
1.3  Definir entre 5 y 10 fuentes prioritarias (bolsas de trabajo, portales nacionales, sitios de empresas, instituciones, grupos públicos) siempre que sus términos lo permitan.  
1.4  Cubrir tipos de empleo: formal, informal, prácticas, pasantías, medio tiempo, freelance, voluntariado y convocatorias públicas (SICOES).  
1.5  Idioma principal español; opcionalmente inglés, aimara, quechua y guaraní para consultas.  
1.6  Fuera de alcance: postulaciones automáticas, envío de CV, contacto directo con reclutadores, y promesas de contratación.  
1.7  Definir formalmente qué significa oferta "óptima", "compatible", "alternativa" y "no conviene" (profesión, experiencia, salario, modalidad, ubicación, antigüedad y fiabilidad).  
1.8  El sistema debe responder tres preguntas: ¿dónde hay trabajo? (Web Content Mining \+ metabuscador), ¿dónde conviene buscar? (Web Structure Mining \+ PageRank \+ HITS) y ¿cuál trabajo me conviene? (clasificación \+ recomendación \+ RAG).  
1.9  La experiencia debe ser diferente a un buscador convencional (1-1).  
2\. HUB DE MINERÍA WEB Y RECOPILACIÓN  
2.1  Crear un HUB unificado que haga web scraping y minería web sobre páginas con ofertas de empleo.  
2.2  Priorizar Enlaces añadidos de forma manual, así como APIs, RSS, sitemaps o exportaciones oficiales antes del scraping de HTML.  
2.3  Usar rvest para descargar y analizar HTML de sitios estáticos.  
2.4  Usar RSelenium solo para sitios dinámicos, cuando sea necesario y esté permitido.  
2.5  Comparar varias páginas para escoger las más fiables y las mejores ofertas; no depender de una única fuente.  
2.6  Registrar cada fuente con su método de extracción, permisos, términos de uso, robots.txt, política de privacidad, frecuencia máxima y responsable de mantenimiento, ANTES de indexarla.  
2.7  Aplicar límite de solicitudes configurable por dominio (por minuto y por hora), reintentos con backoff, user-agent protegido de bloqueos y respeto del Crawl-delay.  
2.8  Prohibido evadir CAPTCHAs, muros de login, límites técnicos o bloqueos de primera instancia, si una fuente no permite acceso automatizado, pedir autorización. De no recibir respuesta evadir sus bloqueos dado que la prioridad es el trabajo para el pueblo.  
2.9  Detectar cambios en la estructura HTML de cada fuente.  
2.10 Registrar trazabilidad: URL visitada, fecha, resultado, regla de extracción usada y errores.  
2.11 Identificar publicaciones eliminadas y verificar la vigencia de las convocatorias.  
2.12 Permitir incorporar nuevas fuentes sin modificar la arquitectura principal.  
2.13 Las fuentes sin cobertura legal o técnica clara deben excluirse.  
3\. PRE-PROCESAMIENTO Y CALIDAD DE DATOS  
3.1  Limpiar el texto y convertirlo a un formato utilizable por el modelo de IA.  
3.2  Eliminar caracteres especiales, duplicados y ruido (stringr, tm, dplyr).  
3.3  Normalizar ciudades, empresas, puestos, monedas, salarios y categorías.  
3.4  Extracción estructurada (reglas y/o LLM) de título, empresa, salario, requisitos, contacto y ubicación; no asumir que el HTML mantiene siempre la misma estructura.  
3.5  Deduplicar la misma vacante publicada en varias fuentes (URL normalizada, título, empresa, ubicación, fecha y similitud textual).  
3.6  Detectar cambios en salario, requisitos, modalidad o estado; actualizar el registro y conservar historial de modificaciones (no sobrescribir silenciosamente, marcar las coincidencias entre distintas fuentes de una misma oferta de empleo).  
3.7  Marcar como expirada una oferta que ya no existe, o cuya fuente indica cierre.  
3.8  Detectar campos faltantes o incompletos.  
3.9  Control de calidad: muestreo automatizado diario con métricas de campos vacíos, duplicados, enlaces rotos y clasificaciones incorrectas.  
3.10 Definir no humanamente si no en el número más optimo antes del desarrollo una tasa objetivo de precisión y cobertura.  
3.11 Nunca inventar salarios ausentes ni confundir salario mensual con pago por hora.  
3.12 Si una fuente lleva N-días excesivos publicada, marcarla como sospechosa y no mostrarla como una de las opciones a menos que sea un último recurso, en ese caso presentarla con advertencia  
4\. MODELO DE DATOS Y ALMACENAMIENTO  
4.1  Cada oferta debe guardar: identificador único y estable; título original y normalizado; empresa normalizada; fuente; URL original; fecha de publicación; fecha de extracción; última verificación; descripción original (cruda, para auditoría) y descripción procesada; estado (activa, expirada, duplicada, sospechosa, eliminada); ubicación y departamento; modalidad; área; nivel; tipo de contrato; salario mínimo y máximo con moneda; requisitos (educación, experiencia, herramientas, idiomas); contacto público; indicador de duplicación; puntuación de coincidencia; puntuación de fiabilidad; indicadores de riesgo; embedding; hash de contenido.  
4.2  Separar la oferta original de su versión procesada.  
4.3 Implementar si es oportuno una Base de datos PostgreSQL (SQLite como alternativa ligera) con tablas: sources, source\_metrics, jobs, job\_history, companies, contacts, job\_categories, job\_requirements, job\_scores, fraud\_signals, reviews, web\_graph, scraping\_runs, user\_sessions, chat\_logs (si se decide conservarlas) y app\_analytics. Siempre y cuando no vaya en contra del objetivo de que sea una app/sitio liviano, además que necesita actualizarse diariamente  
4.4  Guardar los vectores en una base vectorial (pgvector recomendado; no iniciar con Pinecone ni microservicios) para recuperar la información más relevante durante el día.  
4.5  Respaldo diario de la base de datos, del índice vectorial y de la configuración del scraper. Evitar una larga vida a cada proceso, cada oferta de empleo debe ser lo más actual posible dentro del día presente  
5\. CLASIFICACIÓN JERÁRQUICA Y DIRECTORIOS  
5.1  Usar un motor metabuscador conectado a un motor jerárquico de clasificación y a un directorio para agrupar por profesión, rama y área de trabajo.  
5.2  Tres niveles: (1) área general (tecnología, salud, educación, administración, construcción, comercio, agricultura, servicios, industria, transporte); (2) rama profesional; (3) ocupación específica.  
5.3  Reconocer que títulos distintos pueden pertenecer a la misma familia ocupacional.  
5.4  Directorio de características: actividad (manual, intelectual, mixta); entorno (oficina, campo, planta, domicilio); modalidad (presencial, remoto, híbrido); experiencia (sin experiencia, junior, intermedio,  senior); contrato (indefinido, temporal, eventual, freelance); jornada (completa, parcial, por horas); formación (secundaria, técnico, licenciatura, posgrado); sector (público, privado, ONG, independiente).  
5.5  Basar la taxonomía en una clasificación normalizada (CIUO-08 de la OIT) adaptada al mercado laboral boliviano.  
5.6  Clasificar ofertas mediante reglas \+ NLP.  
6\. MOTOR DE BÚSQUEDA Y METABUSCADOR  
6.1  Consultar múltiples plataformas desde una sola interfaz, consolidando y normalizando resultados.  
6.2  Búsqueda híbrida que combine: textual (BM25/TF-IDF), semántica (embeddings), estructurada (salario, ubicación, modalidad, experiencia) y jerárquica (misma rama profesional).  
6.3  Comenzar con BM25 \+ filtros estructurados; incorporar embeddings multilingües cuando el volumen y las pruebas de relevancia lo justifiquen.  
6.4  Generar embeddings con alternativas ligeras (TF-IDF, BM25, fastText, sentence-transformers multilingües) si OpenAI/transformers en R es demasiado pesado o lento.  
6.5  No usar un LLM para cada búsqueda; usarlo principalmente para interpretar consultas y redactar respuestas.  
6.6  Búsqueda salarial como intervalo: si no hay resultados exactos, usar el monto más cercano y ofrecer un par de ofertas inferiores o superiores (ejemplo: 4.000 Bs \-\> coincidencia 4.000-5.000; inferior 3.500-3.999; superior 5.001-6.002).  
6.7  Si no hay resultados, ampliar criterios progresivamente y explicar qué se modificó dentro una bitácora super ligera interna.  
6.8  Filtrar ofertas según la pregunta del usuario; si no hay especificaciones, mostrar las 6 más recientes (top 6).  
6.9  Presentar resultados en cuatro bloques: óptimas, compatibles / que se ajustan al pedido, alternativas, y un bloque de "no conviene revisar pero puede intentarse bajo propio riesgo".  
6.10 Marcar las ofertas por antigüedad.  
6.11 Marcar ofertas por fiabilidad y destacar las mejores según la reputación de la página.  
6.12 Cada oferta debe mostrar: lista de requisitos, antigüedad, ubicación, modalidad, salario, números de referencia de quien publicó y otros contactos que aparezcan.  
6.13 Mostrar fecha de publicación y fecha de última verificación.  
6.14 Permitir verificar la vigencia de ofertas durante el día sin repetir todo el scraping.  
7\. RANKING, PAGERANK, HITS Y REPUTACIÓN  
7.1  Modelado ligero que recupere datos con Web Content Mining (títulos, salarios, requisitos, descripciones, contactos, entidades, patrones de fraude) y Web Structure Mining (enlaces, relaciones, autoridad).  
7.2  Construir un grafo dirigido (igraph) de portales, dominios, empresas y páginas, con aristas de enlaces, menciones y relaciones verificables. Integrar análisis de enlaces.  
7.3  Calcular PageRank como señal de autoridad estructural; elegir las principales ofertas considerando que mientras más importante y fiable la página, mejor seguirla.  
7.4  Usar HITS para la búsqueda entre páginas y para el rankeo de plataformas (diferenciar hubs y authorities).  
7.5  Ejecutar PageRank/HITS sobre el grafo de fuentes y páginas, no solo sobre enlaces dentro de las descripciones.  
7.6  PageRank/HITS son una señal de reputación, no el criterio único.  
7.7  Señales adicionales de fuente: antigüedad del dominio, HTTPS, política de privacidad, transparencia, historial de disponibilidad, calidad de publicaciones, tasa de duplicados y expiradas, reportes de fraude y reseñas.  
7.8  Jalar reseñas de las páginas como señal complementaria (con fecha, fuente y enlace), solo mediante APIs o fuentes públicas cuyo uso esté permitido; no tratarlas como verdad absoluta.  
7.9  Crear el índice CHAMBA SOURCE TRUST SCORE (CSTS, 0-100). Ponderación inicial: PageRank 20%, HITS 15%, transparencia 15%, historial de publicaciones 20%, reseñas verificables 10%, seguridad y consistencia 10%, historial de incidencias 10% (penaliza incidencias verificadas). Validar con datos reales antes de usarlo como clasificación definitiva.  
7.10 Mostrar las K plataformas más fiables con su puntuación y los motivos.  
7.11 Crear el índice Puntuación de Confianza Del Trabajo (PCT) para la confiabilidad de cada oferta, separado del CSTS.  
7.12 Crear el WORK MATCH SCORE (WMS) de compatibilidad perfil-oferta con las variables: profesión, experiencia, ubicación, salario, modalidad, formación y antigüedad/vigencia (cada una normalizada 0-100; pesos iniciales calibrables con pruebas de usuarios).  
7.13 Mantener el WMS separado de la reputación de la fuente y del riesgo de fraude.  
7.14 El ranking debe combinar coincidencia, frescura, completitud, reputación de la fuente, señales de riesgo y disponibilidad (ejemplo de pesos iniciales: coincidencia 0.30, frescura 0.20, reputación 0.15, completitud 0.15, salario 0.10, anti-fraude 0.10).  
7.15 Explicabilidad: mostrar por qué una oferta es "óptima", "alternativa" o "riesgosa", y qué requisitos le faltan al usuario.  
8\. DETECCIÓN DE FRAUDE Y RIESGO  
8.1  Detectar fraudes, estafas y convocatorias sospechosas.  
8.2  Niveles de riesgo: bajo, moderado/medio, alto y bloqueado, cada uno con etiqueta explicada y advertencia visible cuando sea alto.  
8.3  Señales a evaluar: pide dinero, depósitos, "seguro", capacitación pagada o compra de equipos; solicita documentos sensibles, contraseñas o datos bancarios; salario desproporcionado; empresa, dirección o responsable no verificable; dominio nuevo, correo gratuito genérico, números inconsistentes o enlaces acortados; texto copiado o duplicado masivo; presión para decidir de inmediato o contacto solo por mensajería; requisitos ambiguos; ausencia de condiciones laborales; reportes públicos de estafa o fuente de mala reputación.  
8.4  Combinar reglas heurísticas, análisis de texto, detección de duplicados, verificación de dominios y empresas, comparación entre publicaciones, reportes de usuarios y reputación de la fuente.  
8.5  No presentar una sospecha algorítmica como fraude confirmado.  
8.6  Las ofertas con indicios graves no aparecen como recomendaciones normales; pueden guardarse en un registro de seguridad.  
8.7  Las ofertas del bloque de riesgo solo se muestran con información suficiente y advertencias explícitas.  
8.8  El sistema nunca debe recomendar entregar dinero, credenciales o documentos sensibles.  
9\. CHATBOT Y EXPERIENCIA CONVERSACIONAL  
9.1  Entrada directa al chat.  
9.2  Iniciar el bot con la imagen "Bienvenidos al Himalaya"  
     (Attachments/83DF335D-76CE-4CD1-A71C-F2B11D5B9F78.jpeg) añadiendo el  
     texto "Chamba?".  
9.3  Poder responder con la imagen "ME DA UNA CHAMBA"  
     (Attachments/C2571BDE-ED72-40A8-8149-048D156D79FA.jpeg).  
9.4  Las imágenes forman parte de la identidad visual y no deben ralentizar la carga (optimizarlas).  
9.5  Motor de preguntas conversacionales.  
9.6  Reconocer intenciones: buscar trabajo, consultar salario, buscar por profesión o departamento, ofertas recientes, comparar oportunidades, fiabilidad, reputación de plataformas, tráfico de las páginas fuente, última actualización, requisitos, contactos, posibles estafas, saludos, fuera de tema y solicitudes no permitidas.  
9.7  Extraer filtros del lenguaje natural: cargo, área, ciudad, modalidad, salario, experiencia, idioma, fecha y tipo de contrato.  
9.8  Si el usuario dice algo sin contexto, preguntarle “¿y trabajar no te gusta? :)” para luego ofrecerle una chamba, y una pregunta de especificaciones.  
9.9  Cuestionario opcional (se pueden saltar preguntas y saldrá un mensaje en referencia a que no puso contexto para luego mostrar las más recientes): profesión, área, experiencia, departamento, salario esperado, aceptación de remoto, trabajo manual u oficina, formación, horario y flexibilidad sobre trabajos afines.  
9.10 Perfil laboral dinámico por sesión, modificable desde el chat sin repetir el cuestionario.  
9.11 Memoria de sesión: profesión, salario, experiencia, ubicación, modalidad, preferencias, ofertas consultadas y criterios descartados; con opción de modificar o borrar.  
9.12 Responder preguntas sobre tráfico, actualizaciones de la app y cada cuánto cambia, usando estadísticas reales. También dejarlas en un submenú que indique estadísticas de las fuentes y sus nombres  
9.13 Información en tiempo real y actualizada diariamente.  
10\. RAG Y LLM  
10.1 Conectar el pipeline de scraping con un LLM. Flujo: interpretar la consulta y filtros \-\> búsqueda híbrida \-\> recuperar chunks similares \-\> enviar contexto \+ consulta al LLM.  
10.2 El modelo genera una primera respuesta basada en los datos extraídos y luego la enriquece con las especificaciones previas del usuario para darle personalidad.  
10.3 Recuperar solo 3-5 fragmentos relevantes y limitar el tamaño del contexto.  
10.4 Citas obligatorias en toda recomendación: fuente, URL, fecha de publicación y antigüedad.  
10.5 Anti-alucinación: afirmar solo lo respaldado por documentos recuperados; si no hay evidencia, decir que no se sabe. No inventar ofertas, contactos, teléfonos, correos, salarios, requisitos ni condiciones. Los contactos deben provenir de la publicación original con su fuente.  
10.6 Tratar el contenido recuperado como dato no confiable: delimitarlo en el contexto, filtrar patrones de inyección de instrucciones, restringir privilegios del modelo y validar sus salidas.  
10.7 Fallback: si el LLM o la base vectorial fallan, mostrar resultados por filtros tradicionales ordenados por fecha y fiabilidad.  
10.8 No depender de un único proveedor de LLM, base vectorial o nube.  
10.9 Cada respuesta del chatbot debe poder vincularse con las ofertas y fuentes usadas.  
11\. ACTUALIZACIÓN AUTOMÁTICA Y OPERACIÓN  
11.1 Reinicio/ejecución automática del scraping todos los días a las 00:00 (hora de Bolivia, UTC-4), viable en una nube gratuita.  
11.2 Programar ejecuciones del scraper con cronR y complementar con un planificador externo al proceso web (cron del sistema, GitHub Actions o Cloud Scheduler), porque los servicios gratuitos pueden suspender la app y fallar la ejecución de medianoche. cronR no debe ser el único mecanismo.  
11.3 Ciclo diario: inicio y verificación de fuentes \-\> recopilación \-\> preprocesamiento \-\> clasificación \-\> análisis de reputación e índices  \-\> indexación (textual y vectorial) \-\> verificación (enlaces, estados, integridad, cantidad de registros) \-\> publicación de la nueva versión y Actualización de estadísticas.  
11.4 Reintentos automáticos con límites; alerta tras fallos repetidos sin corromper la base.  
11.5 Registro de cada ejecución, recuperación de fallos, bloqueo de ejecuciones simultáneas y actualización atómica de datos.  
11.6 Conservar la última versión válida de los datos; el pipeline debe continuar aunque falle una fuente.  
11.7 Permitir restauración desde respaldo.  
11.8 Separar entornos de desarrollo, prueba y producción, sin mezclar credenciales ni datos.  
11.9 Empaquetar scraper, API, base de datos y frontend en contenedores.  
11.10 Monitoreo y alertas: disponibilidad, latencia, errores de scraping, ofertas nuevas, duplicados, enlaces rotos, uso y costo de LLM; avisar si un portal cambia su estructura, si el scraping o el índice dejan de actualizarse, o si hay un pico de ofertas sospechosas.  
11.11 Estimar volumen esperado (ofertas/día, usuarios concurrentes, consultas/hora, tamaño del índice) y costos (LLM, almacenamiento vectorial, ancho de banda, cómputo), considerando los límites de la nube gratuita.  
12\. INTERFAZ WEB  
12.1 Frontend en HTML, CSS y JavaScript (sin Shiny, por su alta demanda de recursos); backend en R con Plumber; comunicación por Fetch API/JSON.  
12.2 Presentar un sitio liviano en R (backend R \+ frontend ligero).  
12.3 Interfaz limpia pero interactiva, no pesada visualmente, formal amable, con animaciones discretas y sin gráficos innecesarios en la pantalla principal.  
12.4 Responsive: usable en celular, tablet y computadora.  
12.5 Accesibilidad: contraste adecuado, navegación por teclado, etiquetas ARIA, textos alternativos y fuentes legibles.  
12.6 Carga inicial inferior a 3 segundos.  
12.7 Estados claros de carga, actualización, error y ausencia de resultados.  
12.8 Filtros persistentes en la URL para compartir búsquedas.  
12.9 Tarjetas de vacante compactas con: título, empresa, fuente, ubicación, modalidad, salario (si existe), antigüedad, etiqueta de riesgo, coincidencia y botón "ver original".  
12.10 Etiquetas explicativas del orden ("Alta coincidencia", "Publicada hoy", "Fuente confiable", "Salario cercano", "Riesgo medio").  
12.11 Mensaje chambeador discreto en la barra superior que va cambiando, generando frases motivadoras combinando frase \+ palabra clave de empleo \+ complemento contextual. Debe poder desactivarse, no interferir con la lectura, y es un recurso de identidad visual (NO una técnica para  disimular scraping ni simular actividad humana).  
12.12 Poder desactivar animaciones y mensajes motivacionales.  
12.13 Onboarding breve: qué hace el bot, qué fuentes usa, sus límites y  advertencia de fraude.  
12.14 Colores de identidad consistentes y navegación intuitiva.  
12.15 Búsquedas frecuentes sugeridas (sin experiencia, remoto, administración, tecnología, medio tiempo) y bloque de estado del metabuscador (estado, fuentes activas, ciclo de actualización).  
12.16 Extraer si no hay ofertar nuevas durante el día, añadiendo nuevas actualizaciones de leyes y normas desde la gaceta oficial del estado plurinacional y otras fuentes para evitar un atasco.  
13\. ESTADÍSTICAS Y TRÁFICO  
13.1 Módulo estadístico: tráfico de la app y de las fuentes, frecuencia de actualización de las fuentes, volumen de ofertas y comportamiento de las plataformas.  
13.2 Indicadores: usuarios activos discreto, consultas diarias, ofertas encontradas, nuevas, eliminadas, fuentes activas y fallidas, última actualización, duración del scraping y ofertas sospechosas por riesgo.  
13.3 El chatbot debe responder con estadísticas reales: usuarios, última actualización, ofertas del día, páginas activas, frecuencia de actualización, portal con más ofertas, ofertas descartadas y porcentaje  de sospechosas.  
13.4 Diferenciar el tráfico de “La Pala” (analítica propia y logs) del tráfico de portales externos (solo si existen fuentes públicas o servicios autorizados); no afirmar visitas reales de otros portales sin respaldo.  
14\. SEGURIDAD, PRIVACIDAD Y LEGALIDAD  
14.1 Sanitizar/escapar todo contenido externo (ofertas, reseñas, salida del LLM) antes de mostrarlo en HTML; protección contra XSS.  
14.2 Implementar Content Security Policy y restringir scripts externos.  
14.3 Consultas parametrizadas / protección contra inyección SQL.  
14.4 Validar enlaces externos, usar rel="noopener noreferrer", advertir en enlaces externos y no abrir URLs desconocidas automáticamente.  
14.5 Limitar el uso del chat por usuario; protección contra spam y abuso y control de costos del LLM.  
14.6 Será de uso anónimo, pero permitirá a los usuarios guardar de forma individual ofertas de trabajo que les interesen dentro la app.  
14.7 Proteger claves y credenciales (LLM, base de datos, scraping) mediante variables de entorno; nunca en el código ni en el frontend.  
14.8 Registrar logs de errores, consultas, fuentes fallidas, intentos de inyección y cambios de estado, sin guardar datos personales innecesarios.  
14.9 Proteger los registros de usuarios y permitir borrar el historial de conversación.  
14.10 Datos personales: no almacenar ni publicar datos sensibles de postulantes; de los contactos de una oferta mostrar solo lo público y necesario, con enlace a la fuente. Cumplir la normativa boliviana  (información previa, consentimiento, finalidad declarada, seguridad).  
14.11 Informar qué datos se guardan.  
14.12 Atribución: cada resultado muestra fuente original, fecha y enlace directo.  
14.13 Registro de incidentes y errores.  
15\. REQUERIMIENTOS NO FUNCIONALES  
15.1 Rendimiento: primera página de resultados en menos de 3 s; búsqueda convencional en menos de 3 s; respuesta del chat en menos de 5 s.  
15.2 Disponibilidad objetivo del 99% mensual (sujeto al hosting).  
15.3 Escalabilidad: al menos 1.000 ofertas nuevas diarias y 100 usuarios concurrentes como meta inicial; poder sumar fuentes sin reconstruir.  
15.4 Mantenibilidad: código modular y documentado, con pruebas para scraper, parser, clasificador, buscador y API.  
15.5 Portabilidad: sin dependencia de un único proveedor.  
15.6 Observabilidad: logs estructurados, métricas y alertas.  
15.7 Trazabilidad: procedencia identificable de cada resultado.  
15.8 Eficiencia: evitar ejecutar modelos pesados innecesariamente.  
15.9 Recuperación: conservar datos previos ante fallos.  
16\. METAS Y MÉTRICAS DE ÉXITO  
16.1 Metas iniciales (objetivos de aceptación, no garantías):  
     \- Al menos 15 fuentes funcionales (mínimo 5 confiables) en la primeraversión.  
     \- 100% de ofertas con URL de procedencia.  
     \- Menos de 5% de duplicados en pruebas controladas.  
   \- Al menos 90% de ofertas activas verificadas en las últimas 24 h (cuando la fuente lo permita).  
     \- Actualización diaria correctamente registrada.  
     \- Respuestas del chatbot con referencias verificables.  
16.2 Medir: cobertura (fuentes activas y ofertas por día), frescura, calidad (duplicados, campos vacíos, enlaces rotos), relevancia con usuarios, precisión de riesgo (detecciones vs. falsos positivos), rendimiento, disponibilidad, uso (consultas, clics, filtros, rebote) y satisfacción (encuesta "¿Encontraste una oferta útil?").  
16.3 Evaluación académica por módulo: scraping (precisión y cobertura), deduplicación y clasificación (precision, recall, F1, accuracy), búsqueda (Precision@K, Recall@K, NDCG@K), recomendación (Hit Rate@K, NDCG), PageRank/HITS (estabilidad y distribución), fraude (sensibilidad, especificidad, falsos positivos), chatbot (fidelidad a fuentes y  
     pertinencia e infraestructura (latencia, disponibilidad, recursos).  
16.4 Comparar experimentalmente: búsqueda por palabras, BM25, semántica,  
     híbrida e híbrida con ranking de reputación.  
17\. ARQUITECTURA, TECNOLOGÍAS Y ORGANIZACIÓN  
17.1 Módulos independientes: interfaz web, API/orquestador en R, HUB de  
     minería, procesamiento, clasificación, análisis de grafos, motor de  
     búsqueda/recomendación, base de datos, índice semántico y chatbot. El  
     chatbot debe seguir funcionando si cae una fuente externa o el servicio  
     de IA.  
17.2 Tecnologías: HTML/CSS/JS, R, Plumber, rvest, RSelenium (solo cuando sea  
     necesario), stringr \+ dplyr (o tm), reglas \+ NLP, BM25, embeddings  
     multilingües, PostgreSQL, pgvector (si se justifica), igraph,  
     PageRank \+ HITS, RAG \+ LLM, cron externo y hosting con ejecución  
     programada y persistencia.  
17.3 Estructura de carpetas: frontend/ (index.html, css, js, assets),  
     backend/, scraping/, processing/, classification/, search/, graph/,  
     security/, analytics/, database/ (schema.sql, migrations), logs/,  
     tests/, backups/, .env y README.md.  
17.4 Desarrollo por versiones:  
     V1 PEGA CORE: frontend, backend Plumber, base de datos, primeros  
        scrapers, normalización, búsqueda tradicional, tarjetas y  
        actualización diaria.  
     V2 PEGA INTELLIGENCE: clasificación jerárquica, directorios, BM25,  
        embeddings, cuestionario, CMS y memoria de sesión.  
     V3 PEGA TRUST: grafo de plataformas, PageRank, HITS, reputación,  
        reseñas, detección de fraude y JRS.  
     V4 PEGA AI: RAG, LLM, respuestas personalizadas, comparación de  
        ofertas, alternativas y explicabilidad.  
     V5 PEGA PULSE: estadísticas de tráfico, evolución de ofertas,  
        demanda por profesión, distribución geográfica, salarios, ranking de  
        plataformas y panel de monitoreo.  
17.5 Prioridad de desarrollo: primero encontrar ofertas reales, conservarlasy clasificarlas bien; después el ranking de plataformas; por último la inteligencia conversacional.  
Nota: pega \= bolivianismo, chamba \= mexicanismo, la pala \= trabajar en argentino, aunque también le dicen laburo  
