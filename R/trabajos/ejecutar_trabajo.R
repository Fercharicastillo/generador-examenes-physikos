ejecutar_trabajo <- function(directorio_trabajo, carpeta_proyecto) {
  directorio_trabajo <- normalizePath(
    directorio_trabajo,
    winslash = "/",
    mustWork = TRUE
  )
  carpeta_proyecto <- normalizePath(
    carpeta_proyecto,
    winslash = "/",
    mustWork = TRUE
  )

  solicitud <- jsonlite::read_json(
    file.path(directorio_trabajo, "solicitud.json"),
    simplifyVector = TRUE
  )

  actualizar <- function(...) {
    actualizar_estado_trabajo(directorio_trabajo, ...)
  }

  resultado <- tryCatch(
    {
      actualizar(
        estado = "procesando",
        progreso = 1L,
        mensaje = "Validando la solicitud"
      )

      registro <- generar_examenes(
        plantilla = as.integer(solicitud$plantilla),
        estudiantes = as.character(solicitud$estudiantes),
        incluir_soluciones = isTRUE(solicitud$incluir_soluciones),
        semilla = as.integer(solicitud$semilla),
        carpeta_proyecto = carpeta_proyecto,
        directorio_trabajo = directorio_trabajo,
        actualizar_progreso = actualizar
      )

      actualizar(
        estado = "comprimiendo",
        progreso = 95L,
        mensaje = "Creando el archivo ZIP"
      )

      carpeta_resultados <- file.path(directorio_trabajo, "resultados")
      estado <- leer_estado_trabajo(directorio_trabajo)
      archivo_zip <- file.path(
        carpeta_resultados,
        paste0(estado$trabajo_id, ".zip")
      )

      elementos <- c("examenes", "registro_generacion.csv")
      if (isTRUE(solicitud$incluir_soluciones)) {
        elementos <- c(elementos, "soluciones")
      }

      zip::zipr(
        zipfile = archivo_zip,
        files = elementos,
        root = carpeta_resultados,
        include_directories = TRUE
      )

      if (!file.exists(archivo_zip)) {
        stop("La generación terminó sin crear el archivo ZIP.")
      }

      actualizar(
        estado = "completado",
        progreso = 100L,
        actual = nrow(registro),
        total = nrow(registro),
        mensaje = "Evaluaciones generadas correctamente"
      )

      list(registro = registro, archivo_zip = archivo_zip)
    },
    error = function(error) {
      try(
        actualizar(
          estado = "fallido",
          mensaje = "No se pudo completar la generación",
          error = conditionMessage(error)
        ),
        silent = TRUE
      )

      stop(error)
    }
  )

  invisible(resultado)
}
