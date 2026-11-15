"use strict";

const urlApiLaPala = String(window.LA_PALA_API || "").replace(/\/+$/, "");
const buscarElemento = (selector, raiz = document) => raiz.querySelector(selector);
const estadoLaPala = {
  categoria: "todas",
  oportunidades: [],
  fuentes: [],
  precios: [],
  ocupado: false,
  secuenciaConsulta: 0,
  turnosChat: [],
  ultimosIdsChat: [],
  seleccionChat: "",
};

const categoriasLaPala = {
  convocatorias: "Convocatoria pública",
  empleo_publico: "Empleo del Estado",
  contrataciones: "Compra pública",
  merito: "Concurso de mérito",
  becas: "Beca o pasantía",
  fondos: "Fondo o financiamiento",
  precios: "Precio local",
  normativa: "Normativa",
};

function textoSeguro(valor) {
  return String(valor ?? "");
}

function escaparHtml(valor) {
  return textoSeguro(valor)
    .replaceAll("&", "&amp;")
    .replaceAll("<", "&lt;")
    .replaceAll(">", "&gt;")
    .replaceAll('"', "&quot;")
    .replaceAll("'", "&#39;");
}

function urlSegura(valor) {
  try {
    const url = new URL(textoSeguro(valor));
    return url.protocol === "https:" ? url.href : "";
  } catch {
    return "";
  }
}

function fechaLocal(valor) {
  if (!valor) return "Fecha no indicada";
  const fecha = /^\d{4}-\d{2}-\d{2}$/.test(String(valor))
    ? new Date(`${valor}T12:00:00-04:00`)
    : new Date(valor);
  if (Number.isNaN(fecha.getTime())) return "Fecha no indicada";
  return new Intl.DateTimeFormat("es-BO", {
    day: "numeric",
    month: "short",
    year: "numeric",
    timeZone: "America/La_Paz",
  }).format(fecha);
}

function textoFechaCierre(valor) {
  if (!valor) return "Sin fecha límite confirmada";
  const limite = new Date(`${valor}T23:59:59-04:00`);
  if (Number.isNaN(limite.getTime())) return "Sin fecha límite confirmada";
  const diferencia = Math.ceil((limite.getTime() - Date.now()) / 86400000);
  if (diferencia < 0) return "Plazo vencido";
  if (diferencia === 0) return "Cierra hoy";
  if (diferencia === 1) return "Cierra mañana";
  if (diferencia <= 7) return `Cierra en ${diferencia} días`;
  return `Cierre: ${fechaLocal(valor)}`;
}

function cambiarEstado(mensaje, tipo = "normal") {
  const texto = buscarElemento("#estado-texto");
  const punto = document.querySelector(".punto-estado");
  if (texto) texto.textContent = mensaje;
  if (punto) {
    punto.classList.toggle("disponible", tipo === "disponible");
    punto.classList.toggle("error", tipo === "error");
  }
}

async function solicitarJson(ruta) {
  const controlador = new AbortController();
  const temporizador = window.setTimeout(() => controlador.abort(), 20000);
  try {
    const respuesta = await fetch(`${urlApiLaPala}${ruta}`, {
      headers: { Accept: "application/json" },
      cache: "no-store",
      signal: controlador.signal,
    });
    const datos = await respuesta.json().catch(() => ({}));
    if (!respuesta.ok) {
      throw new Error(datos.mensaje || `El servicio respondió HTTP ${respuesta.status}.`);
    }
    return datos;
  } catch (error) {
    if (error.name === "AbortError") throw new Error("La consulta tardó demasiado. Inténtalo otra vez.");
    if (error instanceof TypeError) throw new Error("No hay conexión con La Pala. Comprueba que start.R siga ejecutándose.");
    throw error;
  } finally {
    window.clearTimeout(temporizador);
  }
}

function bloquearControlesConsulta(bloqueados) {
  [
    "#consulta-chat",
    "#enviar-consulta",
    "[data-consulta]",
  ].forEach((selector) => {
    document.querySelectorAll(selector).forEach((control) => {
      control.disabled = bloqueados;
    });
  });
}

function contextoChat() {
  return JSON.stringify({ turnos: estadoLaPala.turnosChat.slice(-5), ultimos_ids: estadoLaPala.ultimosIdsChat.slice(0, 6), seleccion: estadoLaPala.seleccionChat });
}

function registrarTurnoChat(consulta, respuesta) {
  const ids = (respuesta?.oportunidades || []).map((item) => String(item.id || "")).filter(Boolean).slice(0, 6);
  estadoLaPala.turnosChat.push({ consulta, ids, categoria: String(respuesta?.categoria || "") });
  estadoLaPala.turnosChat = estadoLaPala.turnosChat.slice(-5);
  if (ids.length) estadoLaPala.ultimosIdsChat = ids;
  if (respuesta?.detalle_solicitado && ids.length === 1) estadoLaPala.seleccionChat = ids[0];
}

function urlChat(consulta, contexto = contextoChat(), seguimiento = false) {
  return `/api/chat?q=${encodeURIComponent(consulta)}&limit=6&contexto=${encodeURIComponent(contexto)}${seguimiento ? "&seguimiento=1" : ""}`;
}

function escribirTexto(globo, texto) {
  globo.querySelectorAll(".texto-respuesta").forEach((p) => p.remove());
  const nodos = String(texto || "").split(/\n{2,}/).map((t) => t.trim()).filter(Boolean).map((t) => {
    const p = document.createElement("p");
    p.className = "texto-respuesta";
    p.textContent = t;
    return p;
  });
  globo.prepend(...nodos);
}

async function solicitarRecoleccion(clave) {
  const respuesta = await fetch(`${urlApiLaPala}/api/admin/collect`, {
    method: "POST",
    headers: { Accept: "application/json", Authorization: `Bearer ${clave}` },
  });
  const datos = await respuesta.json().catch(() => ({}));
  if (!respuesta.ok) throw new Error(datos.mensaje || `La solicitud respondió HTTP ${respuesta.status}.`);
  return datos;
}

async function esperarRecoleccion(solicitudId, informarEstado) {
  const limite = Date.now() + 30 * 60 * 1000;
  let fallosConexion = 0;
  while (Date.now() < limite) {
    let datos;
    try {
      datos = await solicitarJson(`/api/collection-status?solicitud_id=${encodeURIComponent(solicitudId)}`);
      fallosConexion = 0;
    } catch {
      fallosConexion += 1;
      if (fallosConexion >= 5) throw new Error("No pude confirmar el estado de la actualización.");
      informarEstado("La revisión sigue en curso; actualizaré el estado en cuanto vuelva la conexión.", {});
      await new Promise((resolver) => window.setTimeout(resolver, 5000));
      continue;
    }
    if (datos.solicitud_id === solicitudId && datos.estado === "completada") {
      informarEstado(datos.mensaje, datos.resumen || {});
      return datos;
    }
    if (datos.solicitud_id === solicitudId && datos.estado === "fallida") {
      throw new Error(datos.mensaje || "La recolección terminó con un error.");
    }
    if (datos.solicitud_id === solicitudId && datos.mensaje) informarEstado(datos.mensaje, datos.resumen || {});
    await new Promise((resolver) => window.setTimeout(resolver, 5000));
  }
  throw new Error("La recolección sigue en curso. Revisa el registro de actividad para ver su avance.");
}

function renderizarOportunidad(oportunidad) {
  const url = urlSegura(oportunidad.url);
  const imagen = urlSegura(oportunidad.imagen_url);
  const titulo = escaparHtml(oportunidad.titulo || "Título no disponible");
  const categoria = escaparHtml(categoriasLaPala[oportunidad.categoria] || "Oportunidad");
  const departamento = escaparHtml(oportunidad.departamento || "Bolivia");
  const cierre = textoFechaCierre(oportunidad.fecha_limite);
  const publicada = oportunidad.fecha_publicacion
    ? `Publicada ${escaparHtml(fechaLocal(oportunidad.fecha_publicacion))}`
    : "Fecha de publicación no indicada";
  const verificacion = oportunidad.verificado_en
    ? `Revisada ${escaparHtml(fechaLocal(oportunidad.verificado_en))}`
    : "Aún sin revisión automática";
  const estadoVigencia = oportunidad.estado === "por_verificar"
    ? "Vigencia pendiente de confirmación"
    : "";
  const cierreClase = oportunidad.fecha_limite && cierre !== "Plazo vencido" ? "fecha-cierre" : "";
  let descripcionPlana = String(oportunidad.descripcion || "").replace(/\s+/g, " ").trim();
  const inicioContenido = descripcionPlana.toLocaleLowerCase("es").indexOf(String(oportunidad.titulo || "").toLocaleLowerCase("es"));
  if (inicioContenido > 0) descripcionPlana = descripcionPlana.slice(inicioContenido).trim();
  const resumen = descripcionPlana.length > 360 ? `${descripcionPlana.slice(0, 357).trimEnd()}…` : descripcionPlana;
  const descripcion = resumen ? `<p class="oportunidad-entidad">${escaparHtml(resumen)}</p>` : "";
  const requisitos = String(oportunidad.requisitos || "").trim();
  const detalles = requisitos ? `<details class="detalles-oferta"><summary>Ver detalles y requisitos</summary><p>${escaparHtml(requisitos)}</p></details>` : "";
  const enlace = url
    ? `<a class="enlace-oportunidad" href="${escaparHtml(url)}" target="_blank" rel="noopener noreferrer">Abrir oferta completa</a>`
    : "";
  const imagenHtml = imagen
    ? `<img class="imagen-oportunidad" src="${escaparHtml(imagen)}" alt="" loading="lazy" decoding="async" referrerpolicy="no-referrer">`
    : "";

  return `
    <article class="oportunidad${imagen ? " con-imagen" : " sin-imagen"}">
      ${imagenHtml}
      <div class="contenido-oportunidad">
        <h3>${url ? `<a href="${escaparHtml(url)}" target="_blank" rel="noopener noreferrer">${titulo}</a>` : titulo}</h3>
        ${descripcion}
        ${detalles}
        <div class="oportunidad-datos">
          <span class="marca-categoria">${categoria}</span>
          <span>${departamento}</span>
          <span class="${cierreClase}">${escaparHtml(cierre)}</span>
          <span>${publicada}</span>
          <span>${verificacion}</span>
          ${estadoVigencia ? `<span>${escaparHtml(estadoVigencia)}</span>` : ""}
        </div>
      </div>
      ${enlace}
    </article>`;
}

function mostrarOportunidades(oportunidades, titulo = "Cierres próximos", detalle = "Oportunidades vigentes ordenadas por fecha límite.") {
  const lista = buscarElemento("#lista-oportunidades");
  const vacio = buscarElemento("#vacio-oportunidades");
  const cantidad = buscarElemento("#cantidad-oportunidades");
  buscarElemento("#titulo-oportunidades").textContent = titulo;
  buscarElemento("#subtitulo-oportunidades").textContent = detalle;
  lista.innerHTML = oportunidades.map(renderizarOportunidad).join("");
  lista.querySelectorAll(".imagen-oportunidad").forEach((imagen) => {
    imagen.addEventListener("error", () => {
      imagen.closest(".oportunidad")?.classList.remove("con-imagen");
      imagen.closest(".oportunidad")?.classList.add("sin-imagen");
      imagen.remove();
    }, { once: true });
  });
  vacio.hidden = oportunidades.length > 0;
  cantidad.textContent = String(oportunidades.length);
  if (oportunidades.length) {
    vacio.querySelector("h3").textContent = "No encontramos coincidencias";
    vacio.querySelector("p").textContent = "Prueba con otra palabra o abre una categoría para ampliar la búsqueda.";
  } else {
    vacio.querySelector("h3").textContent = "Todavía no hay publicaciones para mostrar";
    vacio.querySelector("p").textContent = "La Pala seguirá mostrando las publicaciones recientes verificables en cuanto las fuentes aporten resultados.";
  }
}

function presentarDetallesRespuesta(globo, respuesta, parcial = false) {
  globo.querySelectorAll(".conteo-resultados-chat, .resultados-chat, .fuentes-chat, .estado-exploracion-chat, .detalle-extraido-chat").forEach((elemento) => elemento.remove());
  if (!parcial) globo.querySelectorAll(".sugerencias-chat, .sello-agente").forEach((elemento) => elemento.remove());
  if (respuesta?.oportunidades?.length) {
    const conteo = document.createElement("p");
    conteo.className = "texto-secundario conteo-resultados-chat";
    conteo.textContent = `Encontré ${respuesta.total || respuesta.oportunidades.length}. Estas son las coincidencias más cercanas a tu pregunta:`;
    globo.append(conteo);
    const resultadosChat = document.createElement("div");
    resultadosChat.className = "resultados-chat";
    respuesta.oportunidades.slice(0, 3).forEach((oportunidad, indice) => {
      const tarjeta = document.createElement("article");
      tarjeta.className = "resultado-chat";
      const titulo = document.createElement("strong");
      titulo.textContent = `${indice + 1}. ${oportunidad.titulo || "Publicación sin título"}`;
      tarjeta.append(titulo);
      const entidad = document.createElement("p");
      entidad.textContent = [oportunidad.entidad || oportunidad.fuente_nombre, oportunidad.departamento, oportunidad.fecha_limite ? textoFechaCierre(oportunidad.fecha_limite) : "Fecha límite no indicada"].filter(Boolean).join(" · ");
      tarjeta.append(entidad);
      const confiable = oportunidad.confianza_extraccion !== "requiere_revision";
      const resumen = String(oportunidad.descripcion || "").replace(/\s+/g, " ").trim();
      if (confiable && resumen.length > 35) {
        const extracto = document.createElement("p");
        extracto.className = "extracto-resultado-chat";
        extracto.textContent = resumen.length > 230 ? `${resumen.slice(0, 227).trimEnd()}…` : resumen;
        tarjeta.append(extracto);
      }
      const acciones = document.createElement("div");
      acciones.className = "acciones-resultado-chat";
      const url = urlSegura(oportunidad.url);
      if (url) {
        const enlace = document.createElement("a");
        enlace.href = url;
        enlace.target = "_blank";
        enlace.rel = "noopener noreferrer";
        enlace.textContent = "Abrir ficha exacta ↗";
        acciones.append(enlace);
      }
      if (oportunidad.id) {
        const boton = document.createElement("button");
        boton.type = "button";
        boton.className = "boton-texto boton-detalle-chat";
        boton.dataset.verDetalles = oportunidad.id;
        boton.textContent = "Consultar requisitos y detalles";
        acciones.append(boton);
      }
      tarjeta.append(acciones);
      resultadosChat.append(tarjeta);
    });
    globo.append(resultadosChat);
  }

  if (respuesta?.detalle) {
    const detalle = respuesta.detalle;
    const panel = document.createElement("section");
    panel.className = "detalle-extraido-chat";
    const encabezado = document.createElement("strong");
    encabezado.textContent = detalle.estado === "ok" ? "Información leída de la ficha" : "Ficha oficial";
    panel.append(encabezado);
    if (detalle.estado === "ok") {
      const campos = detalle.campos || {};
      const etiquetas = { requisitos: "Requisitos", ubicacion: "Ubicación o modalidad", postulacion: "Postulación y contacto", remuneracion: "Remuneración" };
      Object.entries(etiquetas).forEach(([clave, etiqueta]) => {
        (campos[clave] || []).slice(0, 2).forEach((valor) => {
          const parrafo = document.createElement("p");
          parrafo.textContent = `${etiqueta}: ${valor}`;
          panel.append(parrafo);
        });
      });
      if (detalle.fecha_limite) {
        const fecha = document.createElement("p");
        fecha.textContent = `Fecha límite en la ficha: ${fechaLocal(detalle.fecha_limite)}`;
        panel.append(fecha);
      }
    }
    const enlace = document.createElement("a");
    enlace.href = urlSegura(detalle.url || respuesta.oportunidades?.[0]?.url) || "#";
    enlace.target = "_blank";
    enlace.rel = "noopener noreferrer";
    enlace.textContent = "Ver publicación oficial ↗";
    panel.append(enlace);
    globo.append(panel);
  }

  if (respuesta?.fuentes_recomendadas?.length) {
    const fuentes = document.createElement("div");
    fuentes.className = "fuentes-chat";
    const titulo = document.createElement("p");
    titulo.className = "texto-secundario";
    titulo.textContent = ["en_cola", "en_curso"].includes(respuesta.exploracion?.estado)
      ? "Fuentes para consultar mientras exploro:"
      : "Fuentes públicas relacionadas:";
    fuentes.append(titulo);
    respuesta.fuentes_recomendadas.slice(0, 3).forEach((fuente) => {
      const enlace = document.createElement("a");
      enlace.href = urlSegura(fuente.url) || "#";
      enlace.textContent = fuente.nombre || "Fuente pública";
      if (enlace.getAttribute("href") === "#") enlace.removeAttribute("href");
      else {
        enlace.target = "_blank";
        enlace.rel = "noopener noreferrer";
      }
      fuentes.append(enlace);
      const detalle = document.createElement("small");
      detalle.textContent = fuente.habilitada ? "Portal habilitado · ficha bajo consulta" : "Fuente de referencia pública";
      fuentes.append(detalle);
    });
    globo.append(fuentes);
  }

  if (respuesta?.exploracion?.mensaje) {
    const progreso = document.createElement("p");
    progreso.className = "texto-secundario estado-exploracion-chat";
    progreso.textContent = respuesta.exploracion.mensaje;
    globo.append(progreso);
  }

  if (!parcial && respuesta?.agente?.usado) {
    const sello = document.createElement("small");
    sello.className = "sello-agente";
    sello.textContent = "Respuesta redactada con IA a partir de los datos de las fuentes";
    globo.append(sello);
  }

  if (!parcial && Array.isArray(respuesta?.sugerencias) && respuesta.sugerencias.length) {
    const caja = document.createElement("div");
    caja.className = "sugerencias-chat";
    respuesta.sugerencias.slice(0, 4).forEach((texto) => {
      const boton = document.createElement("button");
      boton.type = "button";
      boton.dataset.sugerencia = String(texto);
      boton.textContent = String(texto);
      caja.append(boton);
    });
    globo.append(caja);
  }
}

function agregarMensaje(texto, tipo = "asistente", respuesta = null) {
  const fila = document.createElement("div");
  fila.className = `fila-mensaje ${tipo === "persona" ? "mensaje-usuario" : "mensaje-asistente"}`;
  const globo = document.createElement("div");
  globo.className = "globo";
  escribirTexto(globo, texto);

  if (respuesta) presentarDetallesRespuesta(globo, respuesta);

  fila.append(globo);
  buscarElemento("#mensajes").append(fila);
  while (buscarElemento("#mensajes").children.length > 60) buscarElemento("#mensajes").firstElementChild.remove();
  buscarElemento("#mensajes").scrollTop = buscarElemento("#mensajes").scrollHeight;
  return fila;
}

async function seguirExploracion(identificador, consulta, filaRespuesta, contexto) {
  const fin = Date.now() + 30 * 60 * 1000;
  let ultimaMarca = "";
  while (Date.now() < fin && identificador === estadoLaPala.secuenciaConsulta) {
    await new Promise((resolver) => window.setTimeout(resolver, 12000));
    if (identificador !== estadoLaPala.secuenciaConsulta) return;
    if (document.hidden) continue;
    let avance;
    try {
      avance = await solicitarJson("/api/collection-status");
    } catch {
      return;
    }
    if (avance.estado === "fallida" || avance.estado === "sin_registro") return;
    if (avance.estado === "completada") {
      cambiarEstado(avance.modo === "consulta" ? "Consulta en línea completa" : "Exploración diaria completa", "disponible");
    } else {
      const resumen = avance.resumen || {};
      const hechas = resumen.fuentes_completadas || 0;
      const total = resumen.total_fuentes || 0;
      cambiarEstado(total ? `Recolección · ${hechas}/${total} fuentes` : "Recolección en curso");
    }

    // Solo repite la consulta pesada al chat si el avance cambió.
    const marca = `${avance.estado}|${avance.resumen?.fuentes_completadas || 0}`;
    if (marca === ultimaMarca) continue;
    ultimaMarca = marca;

    try {
      const datos = await solicitarJson(urlChat(consulta, contexto, true));
      if (identificador !== estadoLaPala.secuenciaConsulta) return;
      const globoRespuesta = filaRespuesta.querySelector(".globo");
      // Si la respuesta ya la redactó el agente IA, el refresco automático solo actualiza las tarjetas.
      if (!filaRespuesta.dataset.agente && datos.respuesta) escribirTexto(globoRespuesta, datos.respuesta);
      presentarDetallesRespuesta(globoRespuesta, datos, true);
      if (Array.isArray(datos.oportunidades)) {
        estadoLaPala.oportunidades = datos.oportunidades;
        estadoLaPala.ultimosIdsChat = datos.oportunidades.map((item) => String(item.id || "")).filter(Boolean).slice(0, 6);
        const titulo = datos.alternativa_reciente === "empleos" ? "Empleos más recientes" : "Resultados de tu consulta";
        mostrarOportunidades(datos.oportunidades, titulo, "Hallazgos parciales de la exploración; se actualizan al consultar cada fuente.");
      }
      if (avance.estado === "completada") {
        await Promise.all([cargarPanel(), cargarOportunidades(estadoLaPala.categoria)]);
        return;
      }
    } catch {
      return;
    }
  }
}

function normalizar(valor) {
  return textoSeguro(valor)
    .toLocaleLowerCase("es-BO")
    .normalize("NFD")
    .replace(/[\u0300-\u036f]/g, "");
}

async function consultarChat(consulta) {
  const texto = consulta.trim();
  if (!texto || estadoLaPala.ocupado) return;
  if (/^(hay bloqueos|bloqueos en las fuentes|fuentes bloqueadas)[?.! ]*$/i.test(texto.normalize("NFD").replace(/[\u0300-\u036f]/g, "").replace(/[¿¡]/g, ""))) { abrirMapache(); return; }

  estadoLaPala.ocupado = true;
  const identificador = ++estadoLaPala.secuenciaConsulta;
  const contexto = contextoChat();
  bloquearControlesConsulta(true);
  agregarMensaje(texto, "persona");
  const boton = buscarElemento("#enviar-consulta");
  boton.disabled = true;
  boton.textContent = "Consultando";
  cambiarEstado("Buscando en el catálogo");
  const filaEspera = document.createElement("div");
  filaEspera.className = "fila-mensaje mensaje-asistente";
  filaEspera.innerHTML = '<div class="globo"><p>Revisando oportunidades y fechas disponibles…</p></div>';
  buscarElemento("#mensajes").append(filaEspera);

  try {
    const datos = await solicitarJson(urlChat(texto, contexto));
    if (datos.fallo) throw new Error("Consulta fallida");
    filaEspera.remove();
    const filaRespuesta = agregarMensaje(datos.respuesta || "La consulta terminó sin una respuesta disponible.", "asistente", datos);
    if (datos.agente?.usado) filaRespuesta.dataset.agente = "1";
    registrarTurnoChat(texto, datos);
    if (["en_cola", "en_curso"].includes(datos.exploracion?.estado)) {
      void seguirExploracion(identificador, texto, filaRespuesta, contexto);
    }
    if (datos.vista) window.abrirAnalitica?.(datos.trabajo || datos.analisis_sitio);
    else if (Array.isArray(datos.oportunidades)) {
      estadoLaPala.oportunidades = datos.oportunidades;
      const titulo = datos.alternativa_reciente === "empleos"
        ? "Empleos más recientes"
        : datos.alternativa_reciente
          ? "Publicaciones más recientes"
          : "Resultados de tu consulta";
      const detalle = datos.alternativa_reciente
        ? datos.alternativa_reciente === "empleos"
          ? "Empleos públicos vigentes ordenados por fecha de publicación."
          : "Publicaciones vigentes ordenadas por fecha de publicación."
        : datos.filtro_fecha
          ? `Con fecha límite hasta ${fechaLocal(datos.filtro_fecha)}.`
          : "Coincidencias encontradas en las fuentes registradas.";
      mostrarOportunidades(datos.oportunidades, titulo, detalle);
    }
    if (datos.categoria === "precios" && Array.isArray(datos.precios)) {
      estadoLaPala.precios = datos.precios;
      mostrarPrecios(datos.precios);
    }
    cambiarEstado("Catálogo consultado", "disponible");
  } catch (error) {
    filaEspera.remove();
    const filaError = agregarMensaje("No pude completar tu consulta ahora. Puedes reintentar en unos minutos.");
    const reintentar = document.createElement("button");
    reintentar.type = "button";
    reintentar.className = "boton-texto";
    reintentar.textContent = "Reintentar consulta";
    reintentar.addEventListener("click", () => {
      reintentar.disabled = true;
      void consultarChat(texto);
    }, { once: true });
    filaError.querySelector(".globo").append(reintentar);
    cambiarEstado("Servicio no disponible", "error");
  } finally {
    estadoLaPala.ocupado = false;
    bloquearControlesConsulta(false);
    boton.textContent = "Consultar";
  }
}

function dibujarTendencia(precios) {
  const ordenados = [...precios]
    .filter((dato) => Number.isFinite(Number(dato.precio)) && dato.precio > 0)
    .sort((a, b) => String(a.observado_en).localeCompare(String(b.observado_en)))
    .slice(-12);
  if (ordenados.length < 2) return "";

  const valores = ordenados.map((dato) => Number(dato.precio));
  const minimo = Math.min(...valores);
  const maximo = Math.max(...valores);
  const amplitud = maximo - minimo || 1;
  const puntos = valores.map((valor, indice) => {
    const x = 8 + (indice / (valores.length - 1)) * 264;
    const y = 49 - ((valor - minimo) / amplitud) * 37;
    return `${x.toFixed(1)},${y.toFixed(1)}`;
  }).join(" ");

  return `<svg class="grafico-precio" viewBox="0 0 280 60" role="img" aria-label="Tendencia de precios observados, de ${escaparHtml(fechaLocal(ordenados[0].observado_en))} a ${escaparHtml(fechaLocal(ordenados.at(-1).observado_en))}"><line x1="8" y1="50" x2="272" y2="50" /><polyline points="${puntos}" /><circle cx="272" cy="${49 - ((valores.at(-1) - minimo) / amplitud) * 37}" r="3" /></svg><div class="etiquetas-grafico"><span>${escaparHtml(fechaLocal(ordenados[0].observado_en))}</span><span>${escaparHtml(fechaLocal(ordenados.at(-1).observado_en))}</span></div>`;
}

function mostrarPrecios(precios) {
  const contenedor = buscarElemento("#resumen-precios");
  const vacio = buscarElemento("#vacio-precios");
  const recientes = [...precios]
    .filter((dato) => Number.isFinite(Number(dato.precio)) && dato.precio > 0)
    .sort((a, b) => String(b.observado_en).localeCompare(String(a.observado_en)))
    .slice(0, 4);

  vacio.hidden = recientes.length > 0;
  contenedor.innerHTML = recientes.map((dato) => {
    const moneda = dato.moneda ? `${escaparHtml(dato.moneda)} ` : "";
    const unidad = dato.unidad ? ` / ${escaparHtml(dato.unidad)}` : "";
    const lugar = [dato.ciudad, dato.departamento].filter(Boolean).join(", ") || "Lugar no indicado";
    const enlace = urlSegura(dato.fuente_url);
    const importe = `${moneda}${Number(dato.precio).toLocaleString("es-BO")}${unidad}${dato.moneda ? "" : " (moneda no indicada)"}`;
    return `<article class="fila-precio"><strong>${escaparHtml(dato.producto || "Producto")}</strong><span class="valor-precio">${importe}</span><small>${escaparHtml(lugar)} · ${escaparHtml(fechaLocal(dato.observado_en))}</small>${enlace ? `<a href="${escaparHtml(enlace)}" target="_blank" rel="noopener noreferrer">Fuente del precio</a>` : ""}</article>`;
  }).join("");

  if (recientes.length) {
    const porProducto = new Map();
    for (const dato of precios) {
      const llave = `${dato.producto || "Producto"}|${dato.ciudad || ""}`;
      porProducto.set(llave, [...(porProducto.get(llave) || []), dato]);
    }
    const primeraSerie = [...porProducto.values()].find((serie) => serie.length > 1);
    if (primeraSerie) contenedor.insertAdjacentHTML("beforeend", dibujarTendencia(primeraSerie));
  }
}

function renderizarFuentes(fuentes) {
  const resumen = buscarElemento("#resumen-fuentes");
  const dialogo = buscarElemento("#lista-dialogo-fuentes");
  if (!fuentes.length) {
    resumen.innerHTML = '<p class="estado-fuentes">Aún no hay fuentes configuradas.</p>';
    dialogo.innerHTML = '<p class="estado-fuentes">No se han registrado fuentes.</p>';
    return;
  }

  const crearFila = (fuente) => {
    const enlace = urlSegura(fuente.url);
    const nombre = escaparHtml(fuente.nombre || "Fuente sin nombre");
    const categoria = escaparHtml(categoriasLaPala[fuente.categoria] || "Referencia");
    const estadoFuente = escaparHtml(fuente.estado || "Registrada");
    const htmlNombre = enlace
      ? `<a href="${escaparHtml(enlace)}" target="_blank" rel="noopener noreferrer">${nombre}</a>`
      : `<span>${nombre}</span>`;
    return `<article class="fila-fuente">${htmlNombre}<span>${categoria}</span><small>${estadoFuente}</small></article>`;
  };

  resumen.innerHTML = fuentes.slice(0, 4).map(crearFila).join("");
  dialogo.innerHTML = fuentes.map(crearFila).join("");
}

function activarCategoria(categoria) {
  estadoLaPala.categoria = categoria;
  document.querySelectorAll("[data-categoria]").forEach((boton) => {
    const activo = boton.dataset.categoria === categoria;
    boton.classList.toggle("activo", activo);
    if (activo) boton.setAttribute("aria-current", "page");
    else boton.removeAttribute("aria-current");
  });

  const etiqueta = categoria === "todas" ? "Cierres próximos" : categoriasLaPala[categoria] || "Oportunidades";
  cargarOportunidades(categoria, etiqueta);
}

function prepararMenuMovil() {
  const consultaMovil = window.matchMedia("(max-width: 620px)");
  const menu = buscarElemento("#menu-movil");
  const contenido = buscarElemento("#contenido-menu-movil");
  const selectores = [
    "#abrir-recoleccion",
    ".estado-servicio",
    "#navegacion",
    ".nota-lateral",
    ".encabezado-pagina",
    ".franja-resumen",
    ".rejilla-principal",
    ".aviso-seguridad",
    ".pie-pagina",
  ];
  const piezas = selectores.map((selector) => {
    const nodo = buscarElemento(selector);
    const marcador = document.createComment(`posición original ${selector}`);
    nodo.parentNode.insertBefore(marcador, nodo);
    return { nodo, marcador };
  });

  const ajustar = () => {
    if (consultaMovil.matches) {
      piezas.forEach(({ nodo }) => contenido.append(nodo));
      return;
    }
    menu.open = false;
    [...piezas].reverse().forEach(({ nodo, marcador }) => marcador.parentNode.insertBefore(nodo, marcador.nextSibling));
  };

  ajustar();
  consultaMovil.addEventListener("change", ajustar);
  document.addEventListener("click", (evento) => {
    if (menu.open && !menu.contains(evento.target)) menu.open = false;
  });
  contenido.addEventListener("click", (evento) => {
    if (evento.target.closest("button, a")) window.setTimeout(() => { menu.open = false; }, 0);
  });
}

async function cargarOportunidades(categoria = "todas", titulo = "Cierres próximos") {
  const parametro = categoria && categoria !== "todas" ? `&categoria=${encodeURIComponent(categoria)}` : "";
  try {
    const datos = await solicitarJson(`/api/opportunities?limit=6${parametro}`);
    const oportunidades = Array.isArray(datos.oportunidades) ? datos.oportunidades : [];
    estadoLaPala.oportunidades = oportunidades;
    mostrarOportunidades(oportunidades, titulo, categoria === "todas" ? "Oportunidades abiertas ordenadas por fecha límite." : `Publicaciones registradas en ${titulo.toLocaleLowerCase("es-BO")}.`);
  } catch (error) {
    mostrarOportunidades([], titulo, "No se pudo cargar la lista en este momento.");
    cambiarEstado("Servicio no disponible", "error");
    console.warn("Fallo al cargar oportunidades:", error);
  }
}

async function actualizarEstadoRecoleccion() {
  try {
    const datos = await solicitarJson("/api/collection-status");
    const linea = buscarElemento("#estado-chat-worker");
    const resumen = datos.resumen || {};
    if (datos.estado === "en_cola") {
      linea.textContent = "La revisión está en cola. Puedes seguir buscando mientras empieza.";
      cambiarEstado("Revisión en cola");
    } else if (datos.estado === "en_curso") {
      const hechas = resumen.fuentes_completadas || 0;
      const total = resumen.total_fuentes || 0;
      const fuente = resumen.fuente_actual ? ` · ${resumen.fuente_actual}` : "";
      const solicitudes = resumen.solicitudes_http || 0;
      linea.textContent = `Revisando ${hechas} de ${total} fuentes · ${solicitudes} solicitudes${fuente}. Los avisos aparecerán a medida que se publiquen.`;
      cambiarEstado(`Revisión · ${hechas}/${total}`);
    } else if (datos.estado === "completada") {
      linea.textContent = "Exploración de hoy completada. Las respuestas usan publicaciones verificadas y sus fuentes originales.";
      if (buscarElemento("#estado-texto").textContent.startsWith("Revisión")) cambiarEstado("Catálogo actualizado", "disponible");
    } else if (datos.estado === "fallida") {
      linea.textContent = "La revisión tuvo un problema. Aún puedes consultar las fuentes registradas.";
    }
  } catch {
    // El chat conserva su catálogo local mientras la API se recupera.
  }
}

async function cargarPanel() {
  const resultados = await Promise.allSettled([
    solicitarJson("/api/stats"),
    solicitarJson("/api/prices?limit=12"),
    solicitarJson("/api/sources"),
  ]);

  if (resultados[0].status === "fulfilled") {
    const resumen = resultados[0].value;
    const total = resumen.oportunidades_activas || 0;
    buscarElemento("#resumen-catalogo").textContent = total
      ? `${total} oportunidades abiertas en ${resumen.categorias_con_datos || 0} áreas; ${resumen.cierres_proximos || 0} vencen en los próximos siete días.`
      : resumen.actualizado_en
        ? "No hay publicaciones verificadas hoy. Las publicaciones de días anteriores se ocultan hasta volver a confirmarse en la fuente."
        : "Aún no se ha realizado la primera recolección. Actualiza las fuentes para consultar el catálogo.";
    buscarElemento("#fecha-actualizacion").textContent = resumen.actualizado_en
      ? `Actualizado ${fechaLocal(resumen.actualizado_en)}`
      : "Sin actualización registrada";
    cambiarEstado("Fuentes consultadas", "disponible");
  }

  if (resultados[1].status === "fulfilled") {
    estadoLaPala.precios = resultados[1].value.precios || [];
    mostrarPrecios(estadoLaPala.precios);
  }

  if (resultados[2].status === "fulfilled") {
    estadoLaPala.fuentes = resultados[2].value.fuentes || [];
    renderizarFuentes(estadoLaPala.fuentes);
  }

  if (resultados.every((resultado) => resultado.status === "rejected")) {
    cambiarEstado("Servicio no disponible", "error");
  }
}

buscarElemento("#form-chat").addEventListener("submit", (evento) => {
  evento.preventDefault();
  const entrada = buscarElemento("#consulta-chat");
  const consulta = entrada.value;
  entrada.value = "";
  consultarChat(consulta);
});

document.querySelectorAll("[data-consulta]").forEach((boton) => {
  boton.addEventListener("click", () => consultarChat(boton.dataset.consulta));
});

document.addEventListener("click", (evento) => {
  const sugerida = evento.target.closest("[data-sugerencia]");
  if (!sugerida || estadoLaPala.ocupado) return;
  void consultarChat(sugerida.dataset.sugerencia);
});

document.addEventListener("click", (evento) => {
  const boton = evento.target.closest("[data-ver-detalles]");
  if (!boton) return;
  estadoLaPala.seleccionChat = boton.dataset.verDetalles;
  void consultarChat("Dame los requisitos y detalles de esta publicación");
});

async function abrirMapache() {
  window.cerrarAnalitica?.();
  window.marcarVistaPrincipal?.("mapache");
  document.querySelector(".hoja")?.classList.add("vista-mapache");
  document.querySelectorAll("[data-categoria], #ir-analitica").forEach((b) => b.removeAttribute("aria-current"));
  buscarElemento("#ir-mapache")?.setAttribute("aria-current", "page");
  const panel = buscarElemento("#mapache-integrado");
  panel.hidden = false;
  panel.scrollIntoView({ behavior: "smooth", block: "start" });
  const resumen = buscarElemento("#mapache-resumen"), lista = buscarElemento("#mapache-fuentes");
  resumen.textContent = "Actualizando el estado local de las fuentes…";
  try {
    const datos = await solicitarJson("/api/mapache/estado");
    resumen.textContent = datos.con_barrera ? `${datos.con_barrera} fuentes publican restricciones de acceso. La revisión continúa en las demás fuentes.` : "No hay bloqueos confirmados en las fuentes revisadas. Revisa también los reportes de prensa recientes.";
    const novedades = datos.normativa || [], listaGaceta = buscarElemento("#lista-gaceta"), estadoGaceta = buscarElemento("#estado-gaceta"), enlaceGaceta = buscarElemento("#enlace-gaceta");
    enlaceGaceta.href = urlSegura(datos.gaceta_url) || "https://www.gacetaoficialdebolivia.gob.bo/";
    estadoGaceta.textContent = novedades.length ? `${novedades.length} publicaciones normativas en el catálogo. Abre cada fuente para consultar el texto oficial.` : "Aún no hay novedades de la Gaceta en el catálogo. Abre el portal oficial para revisar la publicación vigente.";
    listaGaceta.replaceChildren(...novedades.map((n) => { const fila = document.createElement("article"), titulo = document.createElement("a"), fecha = document.createElement("small"); fila.className = "fila-mapache"; titulo.textContent = n.titulo || "Publicación normativa"; titulo.href = urlSegura(n.url) || enlaceGaceta.href; titulo.target = "_blank"; titulo.rel = "noopener noreferrer"; fecha.textContent = n.fecha_publicacion || n.observado_en || "Fecha no indicada"; fila.append(titulo, fecha); return fila; }));
    lista.replaceChildren(...(datos.fuentes || []).map((f) => {
      const fila = document.createElement("article"); fila.className = "fila-mapache";
      const enlace = document.createElement("a"), estadoFuente = document.createElement("span"), motivo = document.createElement("small");
      enlace.textContent = f.nombre || f.id || "Fuente"; enlace.href = f.url || "#"; enlace.target = "_blank"; enlace.rel = "noopener noreferrer";
      estadoFuente.textContent = ({ no_verificada: "Acceso no confirmado", bloqueada_por_reglas: "El portal limita el rastreo", no_consultada: "Pendiente de revisión", revisada_con_datos: "Se encontraron publicaciones", revisada_sin_datos: "Sin publicaciones nuevas", sin_revisar: "Pendiente de revisión", error: "No respondió en la última revisión" })[f.estado] || "Estado sin actualizar";
      motivo.textContent = f.revisado_en ? `Última revisión: ${fechaLocal(f.revisado_en)}` : "Se volverá a revisar en el siguiente ciclo"; fila.append(enlace, estadoFuente, motivo); return fila;
    }));
    const noticias = datos.noticias || {}, hallazgos = noticias.hallazgos || [], estadoNoticias = buscarElemento("#estado-noticias-bloqueos"), listaNoticias = buscarElemento("#lista-noticias-bloqueos");
    buscarElemento("#fuentes-noticias-bloqueos").replaceChildren(...(datos.fuentes_noticias || []).map((f) => { const fila = document.createElement("article"); fila.className = "fila-mapache"; const enlace = document.createElement("a"), feed = document.createElement("small"); enlace.textContent = f.nombre; enlace.href = f.url; enlace.target = "_blank"; enlace.rel = "noopener noreferrer"; feed.textContent = f.feed_url || f.url; fila.append(enlace, feed); return fila; }));
    estadoNoticias.textContent = noticias.consultado_en ? hallazgos.length ? `${hallazgos.length} reportes encontrados en feeds revisados durante las últimas ${noticias.ventana_horas || 72} horas. Revisión: ${fechaLocal(noticias.consultado_en)}.` : `No aparecieron coincidencias en los feeds que respondieron. Esto no confirma que no habrá bloqueos. Revisión: ${fechaLocal(noticias.consultado_en)}.` : "Aún no hay un escaneo de noticias. Inicia o espera el próximo ciclo de recolección.";
    listaNoticias.replaceChildren(...hallazgos.map((n) => {
      const tarjeta = document.createElement("article"); tarjeta.className = "noticia-bloqueo";
      const medio = document.createElement("strong"), titulo = document.createElement("span"), detalle = document.createElement("small"), enlace = document.createElement("a");
      medio.textContent = `${n.medio || "Medio"}${n.publicado_en ? ` · ${n.publicado_en}` : ""}`; titulo.textContent = n.titulo || "Mención de bloqueo o paro"; detalle.textContent = n.resumen || ""; enlace.href = n.url || "#"; enlace.target = "_blank"; enlace.rel = "noopener noreferrer"; enlace.textContent = "Abrir noticia original"; tarjeta.append(medio, titulo, detalle, enlace); return tarjeta;
    }));
  } catch { resumen.textContent = "MAPACHE no pudo actualizarse ahora. Inténtalo de nuevo más tarde."; }
}
buscarElemento("#ir-mapache")?.addEventListener("click", abrirMapache);
buscarElemento("#actualizar-mapache")?.addEventListener("click", abrirMapache);
function cerrarMapache() { document.querySelector(".hoja")?.classList.remove("vista-mapache"); const panel = buscarElemento("#mapache-integrado"); if (panel) panel.hidden = true; buscarElemento("#ir-mapache")?.removeAttribute("aria-current"); }
window.cerrarMapache = cerrarMapache;
window.marcarVistaPrincipal = (vista) => document.querySelectorAll("[data-vista-app]").forEach((pestana) => {
  const activa = pestana.dataset.vistaApp === vista;
  pestana.classList.toggle("activa", activa);
  pestana.setAttribute("aria-selected", String(activa));
  if (activa) pestana.setAttribute("aria-current", "page"); else pestana.removeAttribute("aria-current");
});
document.querySelectorAll("[data-vista-app]").forEach((pestana) => pestana.addEventListener("click", () => {
  const vista = pestana.dataset.vistaApp;
  if (vista === "analitica") return window.abrirAnalitica?.();
  if (vista === "mapache") return abrirMapache();
  window.cerrarAnalitica?.(); cerrarMapache(); window.marcarVistaPrincipal("chat");
  buscarElemento("#titulo-asistente")?.scrollIntoView({ behavior: "smooth", block: "start" });
}));
document.addEventListener("click", (evento) => {
  if (evento.target.closest("[data-categoria]")) { cerrarMapache(); window.cerrarAnalitica?.(); window.marcarVistaPrincipal("chat"); }
  else if (evento.target.closest("#ir-analitica")) cerrarMapache();
});

prepararMenuMovil();

document.querySelectorAll("[data-categoria]").forEach((boton) => {
  boton.addEventListener("click", () => activarCategoria(boton.dataset.categoria));
});

buscarElemento("#limpiar-chat").addEventListener("click", () => {
  estadoLaPala.turnosChat = [];
  estadoLaPala.ultimosIdsChat = [];
  estadoLaPala.seleccionChat = "";
  estadoLaPala.secuenciaConsulta += 1;
  buscarElemento("#mensajes").replaceChildren();
  agregarMensaje("Hola, soy La Pala. Cuéntame qué buscas y en qué departamento; te mostraré avisos con su enlace de origen.");
  mostrarOportunidades(estadoLaPala.oportunidades, "Cierres próximos", "Oportunidades abiertas ordenadas por fecha límite.");
  buscarElemento("#consulta-chat").focus();
});

const dialogoFuentes = buscarElemento("#dialogo-fuentes");
buscarElemento("#boton-fuentes").addEventListener("click", () => dialogoFuentes.showModal());
buscarElemento("#abrir-fuentes").addEventListener("click", () => dialogoFuentes.showModal());
buscarElemento("#cerrar-fuentes").addEventListener("click", () => dialogoFuentes.close());
dialogoFuentes.addEventListener("click", (evento) => {
  if (evento.target === dialogoFuentes) dialogoFuentes.close();
});

const dialogoRecoleccion = buscarElemento("#dialogo-recoleccion");
async function prepararDialogoRecoleccion() {
  const progreso = buscarElemento("#progreso-recoleccion");
  const barra = buscarElemento("#relleno-progreso-recoleccion");
  buscarElemento("#estado-recoleccion-manual").textContent = "";
  progreso.hidden = true;
  barra.value = 0;
  barra.style.width = "2%";
  buscarElemento("#progreso-porcentaje").textContent = "0%";
  buscarElemento("#progreso-fuentes").textContent = "0 de 0 fuentes completadas";
  buscarElemento("#progreso-mensaje").textContent = "Preparando revisión…";
  dialogoRecoleccion.showModal();

  try {
    const datos = await solicitarJson("/api/collection-status");
    const resumen = datos.resumen || {};
    if (datos.estado === "completada") {
      buscarElemento("#estado-recoleccion-manual").textContent = `Última revisión completada: ${resumen.oportunidades_activas || 0} oportunidades vigentes.`;
    } else if (datos.estado === "en_curso" || datos.estado === "en_cola") {
      buscarElemento("#estado-recoleccion-manual").textContent = "Hay una revisión en curso. El progreso se actualizará aquí.";
      progreso.hidden = false;
      const porcentaje = Math.max(0, Math.min(100, Number(resumen.porcentaje) || 0));
      buscarElemento("#progreso-porcentaje").textContent = `${porcentaje}%`;
      buscarElemento("#progreso-fuentes").textContent = `${resumen.fuentes_completadas || 0} de ${resumen.total_fuentes || 0} fuentes completadas · ${resumen.solicitudes_http || 0} solicitudes`;
      buscarElemento("#progreso-mensaje").textContent = resumen.fuente_actual ? `Ahora: ${resumen.fuente_actual}` : datos.mensaje;
      barra.value = porcentaje;
      barra.style.width = `${Math.max(2, porcentaje)}%`;
    } else if (datos.estado === "fallida") {
      buscarElemento("#estado-recoleccion-manual").textContent = "La última revisión no pudo completarse. Puedes iniciar una nueva.";
    }
  } catch {
    buscarElemento("#estado-recoleccion-manual").textContent = "No pude consultar el estado de la revisión.";
  }
}

buscarElemento("#abrir-recoleccion").addEventListener("click", () => { void prepararDialogoRecoleccion(); });
buscarElemento("#cerrar-recoleccion").addEventListener("click", () => dialogoRecoleccion.close());
dialogoRecoleccion.addEventListener("click", (evento) => {
  if (evento.target === dialogoRecoleccion) dialogoRecoleccion.close();
});

buscarElemento("#form-recoleccion-manual").addEventListener("submit", async (evento) => {
  evento.preventDefault();
  const entrada = buscarElemento("#clave-recoleccion");
  const boton = buscarElemento("#iniciar-recoleccion");
  const botonPrincipal = buscarElemento("#abrir-recoleccion");
  const estadoAccion = buscarElemento("#estado-recoleccion-manual");
  const bloqueProgreso = buscarElemento("#progreso-recoleccion");
  const barraProgreso = buscarElemento("#relleno-progreso-recoleccion");
  let clave = entrada.value;
  if (!clave || boton.disabled) return;
  boton.disabled = true;
  botonPrincipal.disabled = true;
  bloqueProgreso.hidden = false;
  barraProgreso.style.width = "2%";
  buscarElemento("#progreso-porcentaje").textContent = "…";
  estadoAccion.textContent = "Iniciando la revisión completa…";

  try {
    const inicio = await solicitarRecoleccion(clave);
    clave = "";
    entrada.value = "";
    estadoAccion.textContent = "Recolección en curso. Consultando las fuentes habilitadas…";
    const resultado = await esperarRecoleccion(inicio.solicitud_id, (mensaje, avance) => {
      estadoAccion.textContent = mensaje || "Revisando fuentes oficiales…";
      const porcentaje = Math.max(0, Math.min(100, Number(avance.porcentaje) || 0));
      barraProgreso.value = porcentaje;
      buscarElemento("#progreso-porcentaje").textContent = porcentaje >= 100 ? "100%" : "En curso";
      buscarElemento("#progreso-fuentes").textContent = `Fuente ${Math.min((avance.fuentes_completadas || 0) + 1, avance.total_fuentes || 1)} de ${avance.total_fuentes || 0} · ${avance.fuentes_completadas || 0} completadas · ${avance.solicitudes_http || 0} solicitudes consultadas`;
      barraProgreso.style.width = `${Math.max(2, porcentaje)}%`;
      buscarElemento("#progreso-mensaje").textContent = avance.fuente_actual ? `Ahora: ${avance.fuente_actual}` : (mensaje || "Revisando fuentes oficiales…");
    });
    const resumen = resultado.resumen || {};
    const fuentesConHallazgos = resumen.fuentes_con_hallazgos || 0;
    const solicitudes = resumen.solicitudes_http || 0;
    const duracion = Math.round(resumen.duracion_segundos || 0);
    estadoAccion.textContent = `Revisión completa: ${resumen.oportunidades_activas || 0} oportunidades vigentes, ${fuentesConHallazgos} fuentes con hallazgos, ${solicitudes} solicitudes en ${duracion} s y ${resumen.observaciones_precios || 0} precios registrados.`;
    const titulo = estadoLaPala.categoria === "todas" ? "Cierres próximos" : categoriasLaPala[estadoLaPala.categoria] || "Oportunidades";
    await Promise.all([cargarPanel(), cargarOportunidades(estadoLaPala.categoria, titulo)]);
  } catch (error) {
    estadoAccion.textContent = "No pude completar la actualización. Inténtalo de nuevo más tarde.";
  } finally {
    clave = "";
    entrada.value = "";
    boton.disabled = false;
    botonPrincipal.disabled = false;
  }
});

Promise.all([cargarPanel(), cargarOportunidades()]).catch((error) => {
  cambiarEstado("Servicio no disponible", "error");
  console.warn("Fallo al iniciar el panel:", error);
});

let temporizadorRecoleccion = 0;
async function vigilarRecoleccion() {
  window.clearTimeout(temporizadorRecoleccion);
  if (!document.hidden) await actualizarEstadoRecoleccion();
  const activo = (buscarElemento("#estado-texto")?.textContent || "").startsWith("Revisión");
  temporizadorRecoleccion = window.setTimeout(() => { void vigilarRecoleccion(); }, activo ? 10000 : 60000);
}
document.addEventListener("visibilitychange", () => { if (!document.hidden) void vigilarRecoleccion(); });
void vigilarRecoleccion();
