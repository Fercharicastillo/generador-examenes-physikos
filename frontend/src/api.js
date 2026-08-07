const API_URL =
  import.meta.env.VITE_API_URL || "http://127.0.0.1:8000";

async function leerRespuesta(response) {
  const contenido = await response.json();

  if (!response.ok) {
    const mensaje =
      contenido.detalle ||
      contenido.error ||
      "Ocurrió un error en el servidor.";

    throw new Error(mensaje);
  }

  return contenido;
}

export async function verificarSalud() {
  const response = await fetch(`${API_URL}/salud`);

  return leerRespuesta(response);
}

export async function obtenerPlantillas() {
  const response = await fetch(`${API_URL}/plantillas`);

  return leerRespuesta(response);
}

export async function obtenerPreguntas() {
  const response = await fetch(
    `${API_URL}/preguntas`
  );

  return leerRespuesta(response);
}

export async function obtenerPlantillasLatex() {
  const response = await fetch(
    `${API_URL}/plantillas-latex`
  );

  return leerRespuesta(response);
}

export async function crearGeneracion(configuracion) {
  const response = await fetch(`${API_URL}/generaciones`, {
    method: "POST",
    headers: {
      "Content-Type": "application/json",
    },
    body: JSON.stringify(configuracion),
  });

  return leerRespuesta(response);
}

export async function consultarGeneracion(trabajoId) {
  const idSeguro = encodeURIComponent(trabajoId);

  const response = await fetch(
    `${API_URL}/generaciones/${idSeguro}`
  );

  return leerRespuesta(response);
}

export async function obtenerArchivosGeneracion(
  trabajoId
) {
  const idSeguro = encodeURIComponent(trabajoId);

  const response = await fetch(
    `${API_URL}/generaciones/${idSeguro}/archivos`
  );

  return leerRespuesta(response);
}

export function construirUrlPdf(
  trabajoId,
  tipo,
  nombre
) {
  const idSeguro = encodeURIComponent(trabajoId);

  const parametros = new URLSearchParams({
    tipo,
    nombre,
  });

  return (
    `${API_URL}/generaciones/${idSeguro}/archivo` +
    `?${parametros.toString()}`
  );
}



export function construirUrlDescarga(trabajoId) {
  const idSeguro = encodeURIComponent(trabajoId);

  return `${API_URL}/generaciones/${idSeguro}/descarga`;
}