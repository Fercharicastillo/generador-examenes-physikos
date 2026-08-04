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

export async function obtenerPlantillas() {
  const response = await fetch(`${API_URL}/plantillas`);

  return leerRespuesta(response);
}

export async function generarExamenes(configuracion) {
  const response = await fetch(`${API_URL}/generar`, {
    method: "POST",
    headers: {
      "Content-Type": "application/json",
    },
    body: JSON.stringify(configuracion),
  });

  return leerRespuesta(response);
}

export function construirUrlDescarga(ruta) {
  return `${API_URL}${ruta}`;
}