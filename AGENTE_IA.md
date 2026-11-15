# Agente IA para La Pala

El chat funciona **sin** IA (respuestas redactadas con reglas).
Al configurar un agente, redactar la respuesta final usando solo los datos ya filtrados del catálogo.
Si falla o no hay clave, el chat vuelve solo a la respuesta base.

## Proveedor
Configura un endpoint compatible con OpenAI:
- `LA_PALA_LLM_KEY=...`
- `LA_PALA_LLM_URL` (def. `https://api.openai.com/v1/chat/completions`)
- `LA_PALA_LLM_MODEL` (def. `gpt-4o-mini`)
- `LA_PALA_AGENTE=off` desactiva la IA; `openai` la activa explícitamente.

Sin clave o si el servicio falla, el chat conserva su respuesta basada en reglas y datos del catálogo.

## Controles
`LA_PALA_AGENTE=off` desactiva el agente. Límites: `LA_PALA_AGENTE_MAX_POR_MIN` (15), `LA_PALA_AGENTE_MAX_DIA` (300), `LA_PALA_AGENTE_TIMEOUT` (8 s). Tras 3 fallos seguidos se pausa 5 minutos. Las respuestas iguales se guardan 15 minutos.

## Ten en cuenta
- R atiende una petición a la vez: mientras espera a la IA (hasta 8 s) el servidor no responde a otros.
- Se eliminan de la respuesta los enlaces que no estén en los datos del catálogo, pero la IA puede equivocarse al redactar: las tarjetas con enlace a la fuente siguen siendo la referencia.
