limpiar_trabajos <- function(
    carpeta_trabajos,
    retencion_minutos = 60
) {
  if (!dir.exists(carpeta_trabajos)) {
    return(invisible(character()))
  }

  carpeta_trabajos <- normalizePath(
    carpeta_trabajos,
    winslash = "/",
    mustWork = TRUE
  )

  directorios <- list.dirs(
    carpeta_trabajos,
    recursive = FALSE,
    full.names = TRUE
  )

  patron_id <- paste0(
    "^gen_",
    "[0-9]{8}_",
    "[0-9]{6}_",
    "[a-z0-9]{12}$"
  )

  directorios <- directorios[
    grepl(
      patron_id,
      basename(directorios)
    )
  ]

  eliminados <- character()

  for (directorio in directorios) {
    archivo_estado <- file.path(
      directorio,
      "estado.json"
    )

    if (!file.exists(archivo_estado)) {
      next
    }

    estado <- tryCatch(
      {
        leer_estado_trabajo(directorio)
      },
      error = function(error) {
        NULL
      }
    )

    if (is.null(estado)) {
      next
    }

    if (
      !estado$estado %in% c(
        "completado",
        "fallido"
      )
    ) {
      next
    }

    fecha_actualizacion <- as.POSIXct(
      estado$actualizado_en,
      format = "%Y-%m-%dT%H:%M:%S%z"
    )

    if (is.na(fecha_actualizacion)) {
      fecha_actualizacion <- file.info(
        archivo_estado
      )$mtime
    }

    antiguedad <- as.numeric(
      difftime(
        Sys.time(),
        fecha_actualizacion,
        units = "mins"
      )
    )

    if (
      is.na(antiguedad) ||
      antiguedad < retencion_minutos
    ) {
      next
    }

    resultado <- unlink(
      directorio,
      recursive = TRUE,
      force = TRUE
    )

    if (
      identical(resultado, 0L) &&
      !dir.exists(directorio)
    ) {
      eliminados <- c(
        eliminados,
        basename(directorio)
      )
    }
  }

  if (length(eliminados) > 0) {
    message(
      "Trabajos eliminados: ",
      paste(eliminados, collapse = ", ")
    )
  }

  invisible(eliminados)
}