"use strict";

const $ = (selector) => document.querySelector(selector);
const catalogUrl = "data/catalogo.json";
const sourcesUrl = "data/fuentes.json";
const stopWords = new Set(["hay", "para", "con", "del", "las", "los", "una", "uno", "por", "que", "antes", "desde", "hasta", "este", "esta", "mes", "dia", "como", "donde"]);
let catalog = { oportunidades: [] };

function normalize(value) {
  return String(value ?? "")
    .normalize("NFD")
    .replace(/[\u0300-\u036f]/g, "")
    .toLowerCase()
    .replace(/[^a-z0-9]+/g, " ")
    .trim();
}

function safeUrl(value) {
  try {
    const url = new URL(value, location.href);
    return url.protocol === "https:" ? url.href : "";
  } catch {
    return "";
  }
}

function search(query) {
  const terms = normalize(query).split(" ").filter((term) => term.length > 2 && !stopWords.has(term));
  if (!terms.length) return [];

  return catalog.oportunidades
    .map((item) => {
      const fields = [
        [item.titulo, 5], [item.categoria, 4], [item.departamento, 3],
        [item.requisitos, 2], [item.descripcion, 1], [item.entidad, 1], [item.fuente_nombre, 1],
      ];
      const values = fields.map(([value]) => normalize(value));
      const matched = terms.filter((term) => values.some((value) => value.includes(term)));
      const score = terms.reduce((total, term) => total + fields.reduce(
        (sum, [, weight], index) => sum + (values[index].includes(term) ? weight : 0), 0,
      ), 0);
      return { item, score, matched: matched.length };
    })
    .filter((result) => result.score > 0)
    .sort((a, b) => b.matched - a.matched || b.score - a.score)
    .map(({ item }) => item);
}

function appendText(parent, tag, className, text) {
  const element = document.createElement(tag);
  if (className) element.className = className;
  element.textContent = text;
  parent.append(element);
  return element;
}

function appendMessage(kind, text) {
  const message = document.createElement("article");
  message.className = `message ${kind}`;
  const body = document.createElement("div");
  body.className = "message-body";
  if (kind === "assistant") {
    appendText(message, "span", "message-mark", "P");
    appendText(body, "p", "message-author", "La Pala");
  } else {
    appendText(body, "p", "message-author", "Tú");
  }
  appendText(body, "p", "message-copy", text);
  message.append(body);
  $("#mensajes").append(message);
  return body;
}

function appendResults(body, items) {
  const list = document.createElement("div");
  list.className = "match-list";
  list.setAttribute("aria-label", "Publicaciones encontradas");

  for (const item of items.slice(0, 5)) {
    const row = document.createElement("article");
    row.className = "match";
    const meta = document.createElement("div");
    meta.className = "match-meta";
    const category = String(item.categoria || "Oportunidad").replaceAll("_", " ");
    meta.append(document.createTextNode(category));
    if (item.departamento) meta.append(document.createTextNode(` · ${item.departamento}`));
    row.append(meta);
    appendText(row, "h2", "", item.titulo || "Publicación pública");
    if (item.fecha_limite) appendText(row, "p", "", `Fecha límite: ${item.fecha_limite}`);
    if (item.descripcion) {
      const summary = item.descripcion.length > 230 ? `${item.descripcion.slice(0, 227).trim()}…` : item.descripcion;
      appendText(row, "p", "", summary);
    }

    const href = safeUrl(item.url) || safeUrl(item.fuente_url);
    if (href) {
      const link = document.createElement("a");
      link.href = href;
      link.target = "_blank";
      link.rel = "noopener noreferrer";
      link.textContent = "Abrir publicación original ↗";
      row.append(link);
    } else {
      appendText(row, "p", "no-link", "Esta publicación no tiene un enlace válido.");
    }
    list.append(row);
  }
  body.append(list);
}

function appendMapacheLink(body) {
  const link = document.createElement("a");
  link.className = "mapache-link";
  link.href = "mapache/index.html";
  link.textContent = "Abrir MAPACHE · mapa y reportes ↗";
  body.append(link);
}

function reply(query, results) {
  const normalized = normalize(query);
  const isMapache = /bloqueo|transitabilidad|sismo|incendio/.test(normalized);
  const body = appendMessage(
    "assistant",
    isMapache
      ? "MAPACHE reúne el mapa y los reportes de su última generación. La información puede estar desactualizada; contrástala con las autoridades."
      : results.length
        ? `Encontré ${results.length} ${results.length === 1 ? "publicación relacionada" : "publicaciones relacionadas"}. Revisa cada aviso en su fuente oficial.`
        : "No encontré coincidencias en el catálogo. Prueba otra categoría, entidad o palabra del título.",
  );

  if (isMapache) appendMapacheLink(body);
  else if (results.length) appendResults(body, results);
  else appendText(body, "p", "no-results", "El catálogo contiene las publicaciones disponibles para esta edición estática.");
  $("#mensajes").scrollTop = $("#mensajes").scrollHeight;
}

function submitQuery(value) {
  const query = value.trim();
  if (!query) return;
  appendMessage("user", query);
  reply(query, search(query));
}

function renderSources(sources) {
  const list = $("#source-list");
  const active = sources.filter((source) => source.habilitada && safeUrl(source.url));
  list.replaceChildren();
  if (!active.length) {
    appendText(list, "li", "source-empty", "No hay fuentes activas en el archivo publicado.");
    return;
  }
  for (const source of active) {
    const item = document.createElement("li");
    const link = document.createElement("a");
    link.href = safeUrl(source.url);
    link.target = "_blank";
    link.rel = "noopener noreferrer";
    link.textContent = source.nombre || "Fuente pública";
    item.append(link);
    list.append(item);
  }
}

function setStatus(text, state) {
  $("#estado").textContent = text;
  $(".status-dot").className = `status-dot ${state}`;
}

function resetConversation() {
  const thread = $("#mensajes");
  thread.replaceChildren();
  const body = appendMessage("assistant", "Busca empleos, becas, convocatorias o compras públicas. Te mostraré los avisos del catálogo y el enlace para revisarlos en su fuente.");
  const suggestions = document.createElement("div");
  suggestions.className = "suggestions";
  suggestions.setAttribute("aria-label", "Prueba una búsqueda");
  for (const [label, query] of [["Convocatorias", "Convocatoria"], ["Avisos laborales", "Laboral"], ["Concursos de méritos", "Concurso de méritos"]]) {
    const button = document.createElement("button");
    button.type = "button";
    button.dataset.query = query;
    button.textContent = label;
    suggestions.append(button);
  }
  body.append(suggestions);
  bindSuggestions();
  $("#query").focus();
}

function bindSuggestions() {
  document.querySelectorAll("[data-query]").forEach((button) => {
    button.addEventListener("click", () => submitQuery(button.dataset.query));
  });
}

$("#form").addEventListener("submit", (event) => {
  event.preventDefault();
  const input = $("#query");
  submitQuery(input.value);
  input.value = "";
  input.focus();
});
$("#limpiar").addEventListener("click", resetConversation);
bindSuggestions();

Promise.allSettled([
  fetch(catalogUrl).then((response) => {
    if (!response.ok) throw new Error("No se pudo cargar el catálogo.");
    return response.json();
  }),
  fetch(sourcesUrl).then((response) => {
    if (!response.ok) throw new Error("No se pudieron cargar las fuentes.");
    return response.json();
  }),
]).then(([catalogResult, sourcesResult]) => {
  if (catalogResult.status !== "fulfilled" || !catalogResult.value || !Array.isArray(catalogResult.value.oportunidades)) {
    setStatus("Catálogo no disponible", "error");
    return;
  }

  catalog = catalogResult.value;
  $("#query").disabled = false;
  $("#send").disabled = false;
  const updated = catalog.actualizado_en ? ` · actualizado ${catalog.actualizado_en}` : "";
  setStatus(`${catalog.oportunidades.length} publicaciones${updated}`, "ready");
  if (sourcesResult.status === "fulfilled") renderSources(sourcesResult.value?.fuentes || []);
  else {
    const list = $("#source-list");
    list.replaceChildren();
    appendText(list, "li", "source-empty", "No se pudo cargar la lista de fuentes.");
  }
});
