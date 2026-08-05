ESTADOS_TRABAJO <- c(
  "pendiente",
  "procesando",
  "generando_examenes",
  "comprimiendo",
  "completado",
  "fallido"
)

escribir_json_atomico <- function(datos, archivo) {
  dir.create(dirname(archivo), recursive = TRUE, showWarnings = FALSE)

  archivo_temporal <- tempfile(
    pattern = paste0(basename(archivo), "_"),
    tmpdir = dirname(archivo)
  )
  on.exit(unlink(archivo_temporal, force = TRUE), add = TRUE)

  jsonlite::write_json(
    datos,
    path = archivo_temporal,
    auto_unbox = TRUE,
    pretty = TRUE,
    null = "null",
    na = "null"
  )

  if (file.exists(archivo)) unlink(archivo, force = TRUE)

  if (!file.rename(archivo_temporal, archivo)) {
    stop("No se pudo guardar el archivo de estado: ", archivo)
  }

  invisible(datos)
}

leer_estado_trabajo <- function(directorio_trabajo) {
  archivo_estado <- file.path(directorio_trabajo, "estado.json")

  if (!file.exists(archivo_estado)) {
    stop("No existe el estado del trabajo: ", archivo_estado)
  }

  jsonlite::read_json(archivo_estado, simplifyVector = TRUE)
}

actualizar_estado_trabajo <- function(
    directorio_trabajo,
    estado = NULL,
    progreso = NULL,
    actual = NULL,
    total = NULL,
    mensaje = NULL,
    error = NULL
) {
  datos <- leer_estado_trabajo(directorio_trabajo)

  if (!is.null(estado)) {
    if (!estado %in% ESTADOS_TRABAJO) {
      stop("Estado de trabajo no permitido: ", estado)
    }
    datos$estado <- estado
  }

  if (!is.null(progreso)) {
    datos$progreso <- max(0, min(100, as.integer(progreso)))
  }
  if (!is.null(actual)) datos$actual <- as.integer(actual)
  if (!is.null(total)) datos$total <- as.integer(total)
  if (!is.null(mensaje)) datos$mensaje <- as.character(mensaje)
  if (!is.null(error)) datos$error <- as.character(error)

  datos$actualizado_en <- format(
    Sys.time(),
    "%Y-%m-%dT%H:%M:%S%z"
  )

  escribir_json_atomico(
    datos,
    file.path(directorio_trabajo, "estado.json")
  )
}
