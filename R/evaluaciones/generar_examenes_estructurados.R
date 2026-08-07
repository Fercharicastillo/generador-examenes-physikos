# Este módulo usa funciones cargadas previamente con source().
# nolint start: object_usage_linter.

generar_examenes_estructurados <- function(
  preguntas,
  estudiantes,
  plantilla,
  incluir_soluciones,
  semilla,
  carpeta_proyecto,
  directorio_trabajo,
  actualizar_progreso = NULL
) {
  carpeta_proyecto <- normalizePath(
    carpeta_proyecto,
    winslash = "/",
    mustWork = TRUE
  )

  directorio_trabajo <- normalizePath(
    directorio_trabajo,
    winslash = "/",
    mustWork = TRUE
  )

  estudiantes <- trimws(
    as.character(estudiantes)
  )

  estudiantes <- estudiantes[
    nzchar(estudiantes)
  ]

  if (length(estudiantes) == 0) {
    stop(
      "Debe proporcionar al menos un estudiante.",
      call. = FALSE
    )
  }

  if (!is.list(preguntas) || length(preguntas) == 0) {
    stop(
      "Debe proporcionar al menos una pregunta.",
      call. = FALSE
    )
  }

  carpeta_plantillas <- file.path(
    carpeta_proyecto,
    "plantillas",
    "latex"
  )

  carpeta_temporales <- file.path(
    directorio_trabajo,
    "temporales"
  )

  carpeta_resultados <- file.path(
    directorio_trabajo,
    "resultados"
  )

  carpeta_examenes <- file.path(
    carpeta_resultados,
    "examenes"
  )

  carpeta_soluciones <- file.path(
    carpeta_resultados,
    "soluciones"
  )

  dir.create(
    carpeta_temporales,
    recursive = TRUE,
    showWarnings = FALSE
  )

  dir.create(
    carpeta_examenes,
    recursive = TRUE,
    showWarnings = FALSE
  )

  if (incluir_soluciones) {
    dir.create(
      carpeta_soluciones,
      recursive = TRUE,
      showWarnings = FALSE
    )
  }

  informar_progreso <- function(
    actual,
    mensaje
  ) {
    if (!is.function(actualizar_progreso)) {
      return(
        invisible(NULL)
      )
    }

    progreso <- as.integer(
      round(
        actual /
          length(estudiantes) *
          90
      )
    )

    actualizar_progreso(
      estado = "generando_examenes",
      actual = actual,
      total = length(estudiantes),
      progreso = progreso,
      mensaje = mensaje
    )

    invisible(NULL)
  }

  nombre_archivo_seguro <- function(
    nombre
  ) {
    resultado <- gsub(
      '[<>:"/\\\\|?*]',
      "_",
      nombre
    )

    resultado <- trimws(resultado)

    if (!nzchar(resultado)) {
      stop(
        "El nombre del estudiante no produce un archivo válido.",
        call. = FALSE
      )
    }

    resultado
  }

  copiar_pdf <- function(
    origen,
    destino,
    descripcion
  ) {
    if (!file.exists(origen)) {
      stop(
        paste0(
          "No existe el PDF de ",
          descripcion,
          ": ",
          origen,
          "."
        ),
        call. = FALSE
      )
    }

    copiado <- file.copy(
      from = origen,
      to = destino,
      overwrite = FALSE
    )

    if (!copiado) {
      stop(
        paste0(
          "No se pudo copiar el PDF de ",
          descripcion,
          " a ",
          destino,
          "."
        ),
        call. = FALSE
      )
    }

    invisible(destino)
  }

  registro <- vector(
    mode = "list",
    length = length(estudiantes)
  )

  informar_progreso(
    0L,
    "Preparando la generación estructurada"
  )

  for (indice in seq_along(estudiantes)) {
    nombre_estudiante <- estudiantes[[indice]]

    estudiante_id <- sprintf(
      "estudiante-%03d",
      indice
    )

    semilla_estudiante <- as.integer(
      semilla + indice - 1L
    )

    nombre_seguro <- nombre_archivo_seguro(
      nombre_estudiante
    )

    mensaje <- paste0(
      "Generando evaluación ",
      indice,
      " de ",
      length(estudiantes),
      ": ",
      nombre_estudiante
    )

    message(mensaje)

    if (is.function(actualizar_progreso)) {
      actualizar_progreso(
        estado = "generando_examenes",
        actual = indice - 1L,
        total = length(estudiantes),
        progreso = as.integer(
          round(
            (indice - 1L) /
              length(estudiantes) *
              90
          )
        ),
        mensaje = mensaje
      )
    }

    evaluacion <- construir_evaluacion(
      preguntas = preguntas,
      estudiante = list(
        id = estudiante_id,
        nombre = nombre_estudiante
      ),
      semilla = semilla_estudiante,
      incluir_solucion =
        incluir_soluciones
    )

    directorio_estudiante <- file.path(
      carpeta_temporales,
      sprintf(
        "estudiante_%03d",
        indice
      ),
      "motor_estructurado"
    )

    resultado <- generar_pdf_evaluacion(
      evaluacion = evaluacion,
      plantilla = plantilla,
      directorio_salida =
        directorio_estudiante,
      carpeta_plantillas =
        carpeta_plantillas
    )

    destino_examen <- file.path(
      carpeta_examenes,
      paste0(
        nombre_seguro,
        ".pdf"
      )
    )

    copiar_pdf(
      origen = resultado$examen$ruta_pdf,
      destino = destino_examen,
      descripcion = paste(
        "examen de",
        nombre_estudiante
      )
    )

    destino_solucion <- NA_character_

    if (incluir_soluciones) {
      destino_solucion <- file.path(
        carpeta_soluciones,
        paste0(
          nombre_seguro,
          "_solucion.pdf"
        )
      )

      copiar_pdf(
        origen =
          resultado$solucion$ruta_pdf,
        destino = destino_solucion,
        descripcion = paste(
          "solución de",
          nombre_estudiante
        )
      )
    }

    registro[[indice]] <- data.frame(
      estudiante = nombre_estudiante,
      estudiante_id = estudiante_id,
      semilla = semilla_estudiante,
      plantilla = plantilla,
      preguntas = length(preguntas),
      examen = destino_examen,
      solucion = destino_solucion,
      duracion_segundos =
        resultado$duracion_total_segundos,
      stringsAsFactors = FALSE
    )

    informar_progreso(
      indice,
      paste0(
        "Evaluación ",
        indice,
        " de ",
        length(estudiantes),
        " generada"
      )
    )
  }

  registro <- do.call(
    rbind,
    registro
  )

  archivo_registro <- file.path(
    carpeta_resultados,
    "registro_generacion.csv"
  )

  write.csv(
    registro,
    archivo_registro,
    row.names = FALSE,
    fileEncoding = "UTF-8"
  )

  invisible(registro)
}

# nolint end