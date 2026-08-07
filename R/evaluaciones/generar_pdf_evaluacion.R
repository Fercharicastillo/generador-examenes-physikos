# Este módulo usa funciones cargadas previamente con source().
# nolint start: object_usage_linter.

preparar_salida_evaluacion <- function(
  directorio_salida
) {
  if (
    !is.character(directorio_salida) ||
      length(directorio_salida) != 1 ||
      is.na(directorio_salida) ||
      !nzchar(trimws(directorio_salida))
  ) {
    stop(
      "'directorio_salida' no es válido.",
      call. = FALSE
    )
  }

  if (!dir.exists(directorio_salida)) {
    creado <- dir.create(
      directorio_salida,
      recursive = TRUE,
      showWarnings = FALSE
    )

    if (!creado && !dir.exists(directorio_salida)) {
      stop(
        paste0(
          "No se pudo crear el directorio: ",
          directorio_salida,
          "."
        ),
        call. = FALSE
      )
    }
  }

  contenido_existente <- list.files(
    directorio_salida,
    all.files = TRUE,
    no.. = TRUE
  )

  if (length(contenido_existente) > 0) {
    stop(
      paste0(
        "El directorio de salida no está vacío: ",
        directorio_salida,
        "."
      ),
      call. = FALSE
    )
  }

  normalizePath(
    directorio_salida,
    winslash = "/",
    mustWork = TRUE
  )
}

generar_tipo_pdf <- function(
  fragmentos,
  plantilla,
  tipo,
  directorio_salida,
  carpeta_plantillas
) {
  directorio_tipo <- file.path(
    directorio_salida,
    tipo
  )

  creado <- dir.create(
    directorio_tipo,
    recursive = TRUE,
    showWarnings = FALSE
  )

  if (!creado && !dir.exists(directorio_tipo)) {
    stop(
      paste0(
        "No se pudo crear el directorio para ",
        tipo,
        ": ",
        directorio_tipo,
        "."
      ),
      call. = FALSE
    )
  }

  archivo_tex <- construir_archivo_tex(
    fragmentos = fragmentos,
    plantilla = plantilla,
    tipo = tipo,
    carpeta_plantillas =
      carpeta_plantillas
  )

  guardado_tex <- guardar_archivo_tex(
    archivo_tex = archivo_tex,
    directorio_salida = directorio_tipo
  )

  compilacion <- compilar_archivo_tex(
    ruta_tex = guardado_tex$ruta
  )

  list(
    tipo = tipo,
    plantilla = plantilla,
    nombre_tex = guardado_tex$nombre_archivo,
    ruta_tex = guardado_tex$ruta,
    ruta_pdf = compilacion$ruta_pdf,
    ruta_registro =
      compilacion$ruta_registro,
    ruta_log_latex =
      compilacion$ruta_log_latex,
    bytes = compilacion$bytes,
    duracion_segundos =
      compilacion$duracion_segundos,
    codigo_salida =
      compilacion$codigo_salida
  )
}

generar_pdf_evaluacion <- function(
  evaluacion,
  plantilla,
  directorio_salida,
  carpeta_plantillas
) {
  inicio <- proc.time()[["elapsed"]]

  directorio_salida <-
    preparar_salida_evaluacion(
      directorio_salida
    )

  fragmentos <- renderizar_evaluacion_latex(
    evaluacion
  )

  resultado_examen <- generar_tipo_pdf(
    fragmentos = fragmentos,
    plantilla = plantilla,
    tipo = "examen",
    directorio_salida = directorio_salida,
    carpeta_plantillas =
      carpeta_plantillas
  )

  resultado_solucion <- NULL

  if (evaluacion$incluir_solucion) {
    resultado_solucion <- generar_tipo_pdf(
      fragmentos = fragmentos,
      plantilla = plantilla,
      tipo = "solucion",
      directorio_salida =
        directorio_salida,
      carpeta_plantillas =
        carpeta_plantillas
    )
  }

  duracion_total <- unname(
    proc.time()[["elapsed"]] - inicio
  )

  list(
    formato =
      "physikos-generated-assessment",
    version_formato = 1L,
    id = fragmentos$id,
    plantilla = plantilla,
    estudiante = evaluacion$estudiante,
    semilla = evaluacion$semilla,
    total_preguntas =
      evaluacion$total_preguntas,
    incluir_solucion =
      evaluacion$incluir_solucion,
    directorio = directorio_salida,
    examen = resultado_examen,
    solucion = resultado_solucion,
    duracion_total_segundos =
      duracion_total
  )
}

# nolint end