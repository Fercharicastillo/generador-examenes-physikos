generar_trabajo_id <- function(prefijo = "gen") {
  caracteres <- c(letters, 0:9)
  sufijo <- paste0(sample(caracteres, 12, replace = TRUE), collapse = "")

  paste(
    prefijo,
    format(Sys.time(), "%Y%m%d_%H%M%S"),
    sufijo,
    sep = "_"
  )
}

crear_trabajo <- function(carpeta_trabajos, solicitud) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("El paquete 'jsonlite' es necesario para crear trabajos.")
  }

  dir.create(carpeta_trabajos, recursive = TRUE, showWarnings = FALSE)

  for (intento in seq_len(10)) {
    trabajo_id <- generar_trabajo_id()
    directorio_trabajo <- file.path(carpeta_trabajos, trabajo_id)

    if (!dir.exists(directorio_trabajo)) break
  }

  if (dir.exists(directorio_trabajo)) {
    stop("No se pudo crear un identificador único para el trabajo.")
  }

  directorios <- c(
    directorio_trabajo,
    file.path(directorio_trabajo, "temporales"),
    file.path(directorio_trabajo, "resultados"),
    file.path(directorio_trabajo, "resultados", "examenes"),
    file.path(directorio_trabajo, "resultados", "soluciones")
  )

  vapply(
    directorios,
    dir.create,
    logical(1),
    recursive = TRUE,
    showWarnings = FALSE
  )

  creado_en <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
  estudiantes <- solicitud$estudiantes
  total <- if (is.null(estudiantes)) 0L else length(estudiantes)

  escribir_json_atomico(
    solicitud,
    file.path(directorio_trabajo, "solicitud.json")
  )

  escribir_json_atomico(
    list(
      trabajo_id = trabajo_id,
      estado = "pendiente",
      progreso = 0L,
      actual = 0L,
      total = total,
      mensaje = "Trabajo recibido",
      error = NULL,
      creado_en = creado_en,
      actualizado_en = creado_en
    ),
    file.path(directorio_trabajo, "estado.json")
  )

  list(
    trabajo_id = trabajo_id,
    directorio = normalizePath(
      directorio_trabajo,
      winslash = "/",
      mustWork = TRUE
    )
  )
}
