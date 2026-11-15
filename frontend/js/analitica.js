"use strict";
// Panel analítico: estadísticas, comparativas, grafo con PageRank, fraude y SEO.
(() => {
  const $ = (s, r = document) => r.querySelector(s), NS = "http://www.w3.org/2000/svg";
  const crear = (ns, t, a = {}, ...k) => { const e = ns ? document.createElementNS(NS, t) : document.createElement(t); for (const [n, v] of Object.entries(a)) n === "class" ? e.setAttribute("class", v) : e.setAttribute(n, v); e.append(...k.flat().filter((x) => x != null && x !== false)); return e; };
  const h = (t, a, ...k) => crear(false, t, a, ...k), sv = (t, a, ...k) => crear(true, t, a, ...k);
  const lista = (x) => (Array.isArray(x) ? x : x == null ? [] : [x]), num = (x, d = 0) => (x == null ? "—" : Number(x).toLocaleString("es-BO", { maximumFractionDigits: d }));
  const etiquetas = { convocatorias: "Convocatorias", empleo_publico: "Empleo del Estado", contrataciones: "Compras y SICOES", merito: "Concursos de mérito", becas: "Becas y pasantías", fondos: "Fondos", precios: "Precios", normativa: "Normativa" };
  const tipos = { s: "Sitio", c: "Categoría", d: "Departamento", e: "Entidad" };
  const apiBase = String(window.LA_PALA_API || "").replace(/\/+$/, "");
  const api = async (ruta) => {
    const r = await fetch(`${apiBase}${ruta}`, { headers: { Accept: "application/json" } });
    const d = await r.json().catch(() => ({}));
    if (!r.ok) throw new Error(d.mensaje || `Error HTTP ${r.status}`);
    return d;
  };
  const seguro = (u) => { try { const x = new URL(u); return x.protocol === "https:" ? x.href : ""; } catch { return ""; } };
  const barra = (v, max, cls = "") => { const b = h("span", { class: `barra-an ${cls}` }, h("i")); b.firstChild.style.width = `${Math.max(2, (100 * v) / (max || 1))}%`; return b; };
  const tarjeta = (titulo, sub, ...k) => h("section", { class: "tarjeta-an" }, h("header", {}, h("h3", {}, titulo), sub && h("p", {}, sub)), ...k);
  const distribucion = (titulo, datos, nombre = (x) => x) => { const m = Math.max(1, ...datos.map((d) => d.n)); return tarjeta(titulo, null, h("ul", { class: "distribucion" }, datos.length ? datos.map((d) => h("li", {}, h("span", {}, nombre(d.nombre)), barra(d.n, m), h("b", {}, num(d.n)))) : h("li", { class: "vacio-an" }, "Sin datos"))); };
  const raiz = () => $("#analitica");
  const esperar = async (id) => { for (let i = 0; i < 400; i++) { await new Promise((r) => setTimeout(r, 1500)); const d = await api(`/api/analisis/trabajo?id=${id}`); if (d.estado === "completado") return d.resultado; if (["fallido", "invalido", "desconocido"].includes(d.estado)) throw new Error(d.error || d.mensaje || `El análisis terminó en estado ${d.estado}.`); } throw new Error("El análisis superó el tiempo máximo de espera. Puedes reintentar el sitio seleccionado."); };

  function kpis(t) {
    const k = [["Publicaciones", t.oportunidades], ["Sitios", t.sitios], ["Nodos del grafo", t.nodos], ["Aristas", t.aristas], ["Densidad", t.densidad, 3], ["Componentes", t.componentes], ["Comunidades", t.comunidades], ["Riesgo medio", `${num(t.riesgo_medio, 1)}/100`], ["Riesgo alto", t.riesgo_alto]];
    return h("div", { class: "kpis-an" }, k.map(([n, v, d]) => h("div", {}, h("strong", {}, typeof v === "string" ? v : num(v, d)), h("span", {}, n))));
  }

  function tabla(filas) {
    const cols = [["sitio", "Sitio"], ["n", "Pubs."], ["con_fecha", "% con fecha"], ["descripcion_media", "Desc. media"], ["dias_cierre_medio", "Días al cierre"], ["riesgo_medio", "Riesgo"], ["pagerank", "PageRank"], ["authority", "HITS · Authority"], ["hub", "HITS · Hub"]];
    let clave = "pagerank", asc = false;
    const cuerpo = h("tbody"), cab = h("tr"), mx = Math.max(...filas.map((f) => f.pagerank || 0), 1e-9);
    const pintar = () => {
      cuerpo.replaceChildren(...[...filas].sort((a, b) => ((a[clave] ?? -1) > (b[clave] ?? -1) ? 1 : -1) * (asc ? 1 : -1)).map((f) => h("tr", {}, h("td", {}, f.sitio, f.oficial && h("em", { class: "sello-an" }, "oficial")), ...cols.slice(1, 5).map(([c]) => h("td", {}, num(f[c]))), h("td", { class: f.riesgo_medio >= 25 ? "riesgo-an" : "" }, num(f.riesgo_medio, 1)), h("td", {}, barra(f.pagerank, mx), h("small", {}, num(f.pagerank, 4))), h("td", {}, num(f.authority, 4)), h("td", {}, num(f.hub, 4)))));
      cab.querySelectorAll("th").forEach((t) => t.setAttribute("aria-sort", t.dataset.c === clave ? (asc ? "ascending" : "descending") : "none"));
    };
    cols.forEach(([c, t]) => { const th = h("th", { "data-c": c, scope: "col" }, h("button", { type: "button" }, t)); th.firstChild.addEventListener("click", () => { asc = clave === c ? !asc : c === "sitio"; clave = c; pintar(); }); cab.append(th); });
    pintar();
    return h("div", { class: "tabla-an" }, h("table", {}, h("thead", {}, cab), cuerpo));
  }

  function grafo(g) {
    const n = g.nodos.length, W = 640, H = 420; if (!n) return h("p", { class: "vacio-an" }, "Sin grafo disponible.");
    const p = g.nodos.map((_, i) => ({ x: W / 2 + 170 * Math.cos((2 * Math.PI * i) / n), y: H / 2 + 150 * Math.sin((2 * Math.PI * i) / n), dx: 0, dy: 0 })), ar = g.aristas.map((e) => [e.de - 1, e.a - 1]);
    for (let it = 0; it < 220; it++) {
      const t = 1 - it / 220;
      p.forEach((a, i) => { a.dx = (W / 2 - a.x) * 0.01; a.dy = (H / 2 - a.y) * 0.01; p.forEach((b, j) => { if (i === j) return; const x = a.x - b.x, y = a.y - b.y, d = Math.max(Math.hypot(x, y), 1), f = 2600 / (d * d); a.dx += (x / d) * f; a.dy += (y / d) * f; }); });
      ar.forEach(([i, j]) => { const x = p[i].x - p[j].x, y = p[i].y - p[j].y, d = Math.hypot(x, y) || 1, f = (d - 70) * 0.04; p[i].dx -= (x / d) * f; p[i].dy -= (y / d) * f; p[j].dx += (x / d) * f; p[j].dy += (y / d) * f; });
      p.forEach((a) => { a.x = Math.min(W - 20, Math.max(20, a.x + Math.max(-12, Math.min(12, a.dx)) * t)); a.y = Math.min(H - 20, Math.max(20, a.y + Math.max(-12, Math.min(12, a.dy)) * t)); });
    }
    const mx = Math.max(...g.nodos.map((x) => x.pagerank)), etiquetados = new Set([...g.nodos.keys()].sort((a, b) => g.nodos[b].pagerank - g.nodos[a].pagerank).slice(0, 14));
    const svg = sv("svg", { viewBox: `0 0 ${W} ${H}`, role: "img", "aria-label": "Grafo dirigido de sitios, categorías, departamentos y entidades; tamaño según PageRank", class: "grafo-an" });
    svg.append(sv("defs", {}, sv("marker", { id: "flecha-grafo-an", viewBox: "0 0 10 10", refX: "8", refY: "5", markerWidth: "5", markerHeight: "5", orient: "auto-start-reverse" }, sv("path", { d: "M 0 0 L 10 5 L 0 10 z" }))));
    ar.forEach(([i, j]) => svg.append(sv("line", { x1: p[i].x, y1: p[i].y, x2: p[j].x, y2: p[j].y, "marker-end": "url(#flecha-grafo-an)" })));
    g.nodos.forEach((x, i) => svg.append(sv("g", { class: `nodo-${x.tipo}` }, sv("title", {}, `${tipos[x.tipo]}: ${x.nombre} · PageRank ${num(x.pagerank, 4)} · Authority ${num(x.authority, 4)} · Hub ${num(x.hub, 4)} · grado ${x.grado}`), sv("circle", { cx: p[i].x, cy: p[i].y, r: 4 + 15 * Math.sqrt(x.pagerank / mx) }), etiquetados.has(i) && sv("text", { x: p[i].x, y: p[i].y - 9 - 12 * Math.sqrt(x.pagerank / mx), "text-anchor": "middle" }, (etiquetas[x.nombre] || x.nombre).slice(0, 22)))));
    return h("div", {}, svg, h("ul", { class: "leyenda-an" }, Object.entries(tipos).map(([k, v]) => h("li", { class: `nodo-${k}` }, h("i"), v))));
  }

  function serieTemporal(filas) {
    filas = lista(filas); if (!filas.length) return h("p", { class: "vacio-an" }, "Aún no hay fechas de publicación interpretables para construir una serie temporal.");
    const max = Math.max(1, ...filas.map((x) => Number(x.n || 0)));
    return h("div", { class: "serie-temporal" }, ...filas.map((x) => h("div", { class: "punto-temporal" }, h("span", {}, x.fecha), h("i", {}, h("b", { style: `width:${Math.max(3, 100 * Number(x.n || 0) / max)}%` })), h("strong", {}, num(x.n)))));
  }
  function anomalías(d) {
    if (!d || d.sitios_actuales < d.sitios_requeridos) return h("p", { class: "vacio-an" }, `IQR espera ${d?.sitios_requeridos || 4} sitios; hay ${d?.sitios_actuales || 0}. Se omiten alertas estadísticas sin muestra suficiente.`);
    const filas = lista(d.hallazgos);
    return filas.length ? h("ul", { class: "mejorar-an" }, ...filas.map((x) => h("li", {}, `${x.sitio}: ${x.publicaciones} publicaciones, fuera del rango IQR`))) : h("p", { class: "vacio-an" }, "IQR no marcó volúmenes atípicos entre las fuentes disponibles.");
  }
  function pipelineNlp() {
    const codigo = `tokenizar <- function(x) {
  t <- strsplit(normalizar_texto(x), " ", fixed = TRUE)[[1]]
  t <- t[nchar(t) > 2 & !t %in% PARADAS]
  sub("(?<=.{3})(es|s)$", "", t, perl = TRUE)
}`;
    return tarjeta("Preprocesamiento y recuperación de información", "Pipeline aplicado antes del ranking; el código operativo está en scraping/collect.R y analisis/motor.R.",
      h("ol", { class: "pipeline-an" },
        h("li", {}, h("b", {}, "Extracción."), " Lee HTML, feeds RSS/Atom, mapas del sitio, tablas, JSON-LD y documentos que el extractor admite; conserva título, texto, fechas, entidad y URL de origen."),
        h("li", {}, h("b", {}, "Limpieza."), " Retira etiquetas residuales, entidades HTML, saltos y espacios repetidos; limita longitud y omite navegación/ruido cuando el portal ofrece estructura interpretable."),
        h("li", {}, h("b", {}, "Normalización y tokens."), " Pasa a minúsculas, translitera acentos, quita URL y puntuación, separa términos y elimina stopwords en español."),
        h("li", {}, h("b", {}, "Reducción."), " Aplica recorte ligero de sufijos; es stemming heurístico, no lematización lingüística."),
        h("li", {}, h("b", {}, "Recuperación."), " BM25 pondera título, entidad, requisitos y descripción; añade tolerancia a errores de una letra y refuerzo de frase. TF-IDF se usa en la similitud entre fuentes.")),
      h("details", { class: "codigo-metodo-an" }, h("summary", {}, "Ver función de tokenización en R"), h("pre", {}, h("code", {}, codigo))));
  }

  function relaciones(r, cantidadSitios = 0) {
    const pares = lista(r.similares), cor = r.correlaciones, ks = cor ? Object.keys(cor) : [];
    const clase = (v) => (v >= 0.6 ? "cor-alta" : v >= 0.3 ? "cor-media" : v <= -0.3 ? "cor-neg" : "");
    const control = h("input", { type: "number", min: "2", max: "20", value: "4", "aria-label": "Mínimo de sitios para Pearson" });
    const panelCor = () => { const min = Math.max(2, Number(control.value || 4)); return cantidadSitios < min || !ks.length ? h("p", { class: "vacio-an" }, `Pearson requiere ${min} sitios; el catálogo actual tiene ${cantidadSitios}.`) : h("div", { class: "tabla-an" }, h("table", { class: "cor-an" }, h("thead", {}, h("tr", {}, h("th"), ks.map((k) => h("th", {}, k.replace("_", " "))))), h("tbody", {}, ks.map((a) => h("tr", {}, h("th", {}, a.replace("_", " ")), ks.map((b) => h("td", { class: clase(cor[a][b]) }, num(cor[a][b], 2)))))))); };
    const correlaciones = tarjeta("Correlaciones entre métricas", "Pearson · mínimo ajustable (4 sitios por defecto)", h("label", { class: "umbral-cor-label" }, "Mínimo de sitios", control), panelCor());
    control.addEventListener("input", () => { correlaciones.lastChild.replaceWith(panelCor()); });
    return [tarjeta("Sitios más parecidos", "Similitud coseno TF-IDF entre vocabularios", h("ul", { class: "distribucion" }, pares.length ? pares.map((x) => h("li", {}, h("span", {}, `${x.a} ↔ ${x.b}`), barra(x.similitud, 1), h("b", {}, num(x.similitud, 2)))) : h("li", { class: "vacio-an" }, "Se necesitan al menos 2 sitios"))), correlaciones];
  }

  function fraude(f) {
    f = lista(f);
    if (!f.some((x) => x.demostracion)) f = [{ titulo: "EJEMPLO FICTICIO · Convocatoria solicita pago urgente", fuente: "Demostración, no es oferta real", motivos: ["pago anticipado", "urgencia artificial"], puntaje: 82, nivel: "alto", url: "", demostracion: true }, ...f];
    return tarjeta("Detección de fraude", "Señales detectadas; el caso de ejemplo está rotulado como ficticio", h("ul", { class: "fraude-an" }, f.map((x) => h("li", { class: `nivel-${x.nivel}` }, h("div", {}, seguro(x.url) ? h("a", { href: seguro(x.url), target: "_blank", rel: "noopener noreferrer" }, x.titulo) : h("span", {}, x.titulo), h("small", {}, `${x.fuente} · ${lista(x.motivos).join(" · ")}`)), h("b", {}, `${x.puntaje}`)))));
  }

  function resultadoSeo(r) {
    if (r.error) return h("article", { class: "seo-an" }, h("h4", {}, r.url || "Página"), h("p", { class: "riesgo-an" }, r.error));
    const col = r.puntaje >= 80 ? "alto" : r.puntaje >= 50 ? "medio" : "bajo";
    return h("article", { class: "seo-an" }, h("div", { class: "seo-cab" }, h("b", { class: `puntaje-${col}` }, num(r.puntaje)), h("div", {}, h("h4", {}, r.titulo || r.url), h("small", {}, r.url))),
      h("dl", {}, [["Palabras", r.palabras], ["H1", r.h1], ["Imágenes sin alt", `${r.imagenes_sin_alt}/${r.imagenes}`], ["Enlaces internos", r.enlaces_internos], ["Enlaces externos", r.enlaces_externos], ["Dominios externos", r.dominios_externos], ["Respuesta", `${num(r.tiempo_ms)} ms`]].map(([a, b]) => h("div", {}, h("dt", {}, a), h("dd", {}, String(b))))),
      lista(r.terminos).length > 0 && h("p", { class: "terminos-an" }, lista(r.terminos).slice(0, 6).map((t) => h("span", {}, `${t.termino} ${num(t.densidad, 1)}%`))),
      lista(r.mejorar).length > 0 && h("ul", { class: "mejorar-an" }, lista(r.mejorar).map((m) => h("li", {}, m))));
  }

  function formularioSeo() {
    const sal = h("div", { class: "seo-salida", "aria-live": "polite" }), sitios = h("select", { class: "seo-sitios", multiple: "", size: "7", "aria-label": "Selecciona hasta siete sitios con mayor PageRank" });
    const etiqueta = h("label", { class: "seo-etiqueta" }, "Siete sitios principales por PageRank", sitios);
    const ejecutar = async (ruta, pintar, boton, msg) => { boton.disabled = true; sal.replaceChildren(h("p", { class: "vacio-an" }, msg)); try { const d = await api(ruta); sal.replaceChildren(...pintar(d.trabajo ? await esperar(d.trabajo) : d)); } catch (e) { sal.replaceChildren(h("p", { class: "riesgo-an" }, e.message)); } boton.disabled = false; };
    const b1 = h("button", { type: "submit" }, "Analizar seleccionados"), b2 = h("button", { type: "button", class: "secundario" }, "Rastrear seleccionado");
    const seleccionados = () => [...sitios.selectedOptions].map((o) => o.value).filter(Boolean);
    const f = h("form", { class: "form-seo" }, etiqueta, h("div", { class: "seo-acciones" }, b1, b2));
    f.addEventListener("submit", (e) => { e.preventDefault(); const urls = seleccionados(); if (urls.length) ejecutar(`/api/analisis/sitio?urls=${encodeURIComponent(urls.join(","))}`, (d) => lista(d).map(resultadoSeo), b1, `Analizando ${urls.length} sitios…`); });
    b2.addEventListener("click", () => { const u = seleccionados()[0]; if (u) ejecutar(`/api/analisis/rastreo?url=${encodeURIComponent(u)}&max=8`, (d) => d.error ? [h("p", { class: "riesgo-an" }, d.error)] : [h("p", { class: "vacio-an" }, `${d.sitio}: ${d.paginas_leidas} páginas leídas, ${d.nodos} nodos, ${d.aristas} enlaces, SEO medio ${d.puntaje_medio}/100.`), h("div", { class: "tabla-an" }, h("table", {}, h("thead", {}, h("tr", {}, ["Página", "PageRank", "HITS Authority", "HITS Hub", "Entrantes", "Salientes", "SEO"].map((c) => h("th", {}, c)))), h("tbody", {}, lista(d.paginas).map((p) => h("tr", {}, h("td", {}, p.titulo || p.url, h("small", {}, p.url)), h("td", {}, barra(p.pagerank, d.paginas[0].pagerank), h("small", {}, num(p.pagerank, 4))), h("td", {}, num(p.authority, 4)), h("td", {}, num(p.hub, 4)), h("td", {}, p.entrantes), h("td", {}, p.salientes), h("td", {}, p.puntaje ?? "—"))))))], b2, "Rastreando el sitio elegido en segundo plano…"); });
    return { f, sal, sitios };
  }

  async function cargarSitiosFuertes(filas) {
    if (!seo?.sitios) return;
    let top = lista(filas).filter((x) => seguro(x.url)).sort((a, b) => (b.pagerank || 0) - (a.pagerank || 0)).slice(0, 7);
    if (top.length < 7) {
      try {
        const d = await api("/api/sources"), extra = lista(d.fuentes).filter((x) => seguro(x.url) && !top.some((t) => new URL(t.url).hostname === new URL(x.url).hostname));
        top = [...top, ...extra.slice(0, 7 - top.length).map((x) => ({ sitio: x.nombre, url: x.url, pagerank: 0, oficial: true }))];
      } catch { /* El selector conserva los sitios que sí entregó el catálogo. */ }
    }
    seo.sitios.replaceChildren(...top.map((x) => { const o = h("option", { value: seguro(x.url) }, `${x.sitio} · PageRank ${num(x.pagerank, 4)}`); o.selected = true; return o; }));
    if (!top.length) seo.sitios.replaceChildren(h("option", { value: "" }, "No hay enlaces HTTPS en el catálogo"));
  }

  let cargado = false, seo;
  function armar() {
    const r = raiz(); seo = formularioSeo();
    r.replaceChildren(h("header", { class: "cab-an" }, h("div", {}, h("p", { class: "saludo" }, "Minería web · análisis"), h("h2", {}, "Analítica del catálogo y de la web"), h("p", { class: "bajada" }, "Estadística, tendencias, grafos, riesgos y auditorías de SEO con explicación de métodos.")), h("button", { type: "button", id: "refrescar-an", class: "secundario" }, "Actualizar")),
      h("div", { id: "datos-an" }, h("p", { class: "vacio-an" }, "Calculando…")), pipelineNlp(), tarjeta("SEO y rastreo de enlaces", "Solo HTTPS público, respetando robots.txt, con una pausa entre solicitudes", seo.f, seo.sal));
    $("#refrescar-an").addEventListener("click", cargar);
  }
  async function cargar() {
    const caja = $("#datos-an"); caja.replaceChildren(h("p", { class: "vacio-an" }, "Calculando…"));
    try {
      const d = await api("/api/analisis/resumen"); cargado = true;
      cargarSitiosFuertes(d.comparativa);
      if (d.vacio) return caja.replaceChildren(h("p", { class: "vacio-an" }, "Aún no hay publicaciones vigentes para analizar. Inicia la minería web y actualiza."));
      const dist = d.distribuciones, ds = dist.descripcion;
      caja.replaceChildren(kpis(d.totales),
        h("div", { class: "rejilla-an" }, distribucion("Por categoría", lista(dist.categoria), (x) => etiquetas[x] || x), distribucion("Por departamento", lista(dist.departamento)), distribucion("Por sitio", lista(dist.fuente)), distribucion(`Días al cierre (${num(dist.sin_fecha)} sin fecha)`, lista(dist.cierre))),
        tarjeta("Estadísticos de la descripción", `Entropía de categorías: ${num(dist.entropia_categorias, 2)} (0 = concentrada, 1 = uniforme)`, h("p", { class: "terminos-an" }, [["media", ds.media], ["mediana", ds.mediana], ["desv. est.", ds.desviacion], ["Q1", ds.q1], ["Q3", ds.q3]].map(([a, b]) => h("span", {}, `${a} ${num(b, 1)} car.`)))),
        tarjeta("Comparativa entre sitios", "Clic en un encabezado para ordenar", tabla(lista(d.comparativa))),
        tarjeta("Publicaciones en el tiempo", "Frecuencia por mes de publicación detectada", serieTemporal(dist.publicaciones_por_mes)),
        tarjeta("Detección de anomalías", "Regla IQR sobre el volumen de publicaciones por fuente", anomalías(d.anomalias)),
        tarjeta("Grafo dirigido · PageRank y HITS", "PageRank mide importancia global; Authority recibe enlaces relevantes; Hub apunta a buenas autoridades. El grafo representa relaciones del catálogo y no equivale al ranking de un buscador.", grafo(d.grafo), h("p", { class: "interpretacion-an" }, "La autoridad de enlace puede ayudar a priorizar una auditoría SEO. La relación entre SEO y posiciones reales no se estima sin datos observados de resultados de búsqueda.")),
        h("div", { class: "rejilla-an" }, ...relaciones(d.relaciones, d.totales.sitios)), fraude(d.fraude));
    } catch (e) { caja.replaceChildren(h("p", { class: "riesgo-an" }, `No se pudo calcular: ${e.message}`)); }
  }
  const main = () => $("#contenido"), boton = () => $("#ir-analitica");
  function abrir(sitio) {
    window.cerrarMapache?.();
    window.marcarVistaPrincipal?.("analitica");
    if (!seo) armar(); main().classList.add("vista-analitica"); raiz().hidden = false; boton().setAttribute("aria-current", "page");
    document.querySelectorAll("[data-categoria]").forEach((b) => b.removeAttribute("aria-current"));
    if (typeof sitio === "string") { seo.sal.replaceChildren(h("p", { class: "vacio-an" }, "Analizando…")); esperar(sitio).then((r) => seo.sal.replaceChildren(...lista(r).map(resultadoSeo))).catch((e) => seo.sal.replaceChildren(h("p", { class: "riesgo-an" }, e.message))); }
    else if (sitio) { seo.entrada.value = sitio.url || ""; seo.sal.replaceChildren(resultadoSeo(sitio)); }
    if (!cargado) cargar(); raiz().scrollIntoView({ behavior: "smooth", block: "start" });
  }
  function cerrar() { if (!seo) return; main().classList.remove("vista-analitica"); raiz().hidden = true; boton().removeAttribute("aria-current"); }
  window.abrirAnalitica = abrir; window.cerrarAnalitica = cerrar;
  boton().addEventListener("click", () => abrir());
  document.querySelectorAll("[data-categoria]").forEach((b) => b.addEventListener("click", cerrar));
})();
