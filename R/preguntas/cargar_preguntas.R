# Este módulo usa funciones cargadas previamente con source().
# nolint start: object_usage_linter.

cargar_preguntas_ids <- function(
  ids,
  carpeta_preguntas
) {
  ids <- as.character(
    unlist(
      ids,
      use.names = FALSE
    )
  )

  ids <- trimws(ids)
  ids <- ids[nzchar(ids)]

  if (length(ids) == 0) {
    stop(
      "Debe seleccionar al menos una pregunta.",
      call. = FALSE
    )
  }

  if (anyDuplicated(ids)) {
    stop(
      "La solicitud contiene preguntas repetidas.",
      call. = FALSE
    )
  }

  if (!dir.exists(carpeta_preguntas)) {
    stop(
      paste0(
        "La carpeta de preguntas no existe: ",
        carpeta_preguntas,
        "."
      ),
      call. = FALSE
    )
  }

  archivos <- list.files(
    carpeta_preguntas,
    pattern = "\\.json$",
    recursive = TRUE,
    full.names = TRUE,
    ignore.case = TRUE
  )

  if (length(archivos) == 0) {
    stop(
      "No existen preguntas JSON disponibles.",
      call. = FALSE
    )
  }

  preguntas_disponibles <- lapply(
    archivos,
    function(archivo) {
      pregunta <- jsonlite::read_json(
        archivo,
        simplifyVector = FALSE
      )

      validacion <- validar_pregunta(
        pregunta
      )

      if (!validacion$valida) {
        stop(
          paste(
            c(
              paste0(
                "La pregunta no es válida: ",
                archivo
              ),
              validacion$errores
            ),
            collapse = "\n"
          ),
          call. = FALSE
        )
      }

      pregunta
    }
  )

  ids_disponibles <- vapply(
    preguntas_disponibles,
    function(pregunta) {
      pregunta$id
    },
    character(1)
  )

  if (anyDuplicated(ids_disponibles)) {
    duplicados <- unique(
      ids_disponibles[
        duplicated(ids_disponibles)
      ]
    )

    stop(
      paste0(
        "El banco contiene identificadores repetidos: ",
        paste(
          duplicados,
          collapse = ", "
        ),
        "."
      ),
      call. = FALSE
    )
  }

  ids_inexistentes <- setdiff(
    ids,
    ids_disponibles
  )

  if (length(ids_inexistentes) > 0) {
    stop(
      paste0(
        "No existen las preguntas: ",
        paste(
          ids_inexistentes,
          collapse = ", "
        ),
        "."
      ),
      call. = FALSE
    )
  }

  posiciones <- match(
    ids,
    ids_disponibles
  )

  preguntas_disponibles[
    posiciones
  ]
}

# nolint end