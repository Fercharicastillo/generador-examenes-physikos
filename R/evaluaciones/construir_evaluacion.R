# Este módulo usa funciones cargadas previamente con source().
# nolint start: object_usage_linter.

validar_semilla <- function(
  semilla
) {
  if (
    !is.numeric(semilla) ||
      length(semilla) != 1 ||
      is.na(semilla) ||
      !is.finite(semilla) ||
      semilla < 0 ||
      semilla != floor(semilla) ||
      semilla > .Machine$integer.max
  ) {
    stop(
      paste(
        "'semilla' debe ser un entero",
        "entre 0 y",
        .Machine$integer.max,
        "."
      ),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

validar_estudiante <- function(
  estudiante
) {
  if (!is.list(estudiante)) {
    stop(
      "'estudiante' debe ser una lista.",
      call. = FALSE
    )
  }

  campos <- c(
    "id",
    "nombre"
  )

  faltantes <- setdiff(
    campos,
    names(estudiante)
  )

  if (length(faltantes) > 0) {
    stop(
      paste0(
        "Faltan datos del estudiante: ",
        paste(
          faltantes,
          collapse = ", "
        ),
        "."
      ),
      call. = FALSE
    )
  }

  if (!es_texto_escalar(estudiante$id)) {
    stop(
      "El identificador del estudiante no es válido.",
      call. = FALSE
    )
  }

  if (!es_texto_escalar(estudiante$nombre)) {
    stop(
      "El nombre del estudiante no es válido.",
      call. = FALSE
    )
  }

  invisible(TRUE)
}

validar_lista_preguntas <- function(
  preguntas
) {
  if (
    !is.list(preguntas) ||
      length(preguntas) == 0
  ) {
    stop(
      paste(
        "'preguntas' debe ser una lista",
        "con al menos una pregunta."
      ),
      call. = FALSE
    )
  }

  ids <- character(
    length(preguntas)
  )

  for (indice in seq_along(preguntas)) {
    pregunta <- preguntas[[indice]]

    resultado <- validar_pregunta(
      pregunta
    )

    if (!resultado$valida) {
      stop(
        paste(
          c(
            paste0(
              "La pregunta ",
              indice,
              " no es válida:"
            ),
            resultado$errores
          ),
          collapse = "\n"
        ),
        call. = FALSE
      )
    }

    ids[[indice]] <- pregunta$id
  }

  if (anyDuplicated(ids)) {
    duplicados <- unique(
      ids[
        duplicated(ids)
      ]
    )

    stop(
      paste0(
        "La evaluación contiene preguntas repetidas: ",
        paste(
          duplicados,
          collapse = ", "
        ),
        "."
      ),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

usar_semilla_local <- function(
  semilla,
  codigo
) {
  existia_semilla <- exists(
    ".Random.seed",
    envir = .GlobalEnv,
    inherits = FALSE
  )

  if (existia_semilla) {
    semilla_anterior <- get(
      ".Random.seed",
      envir = .GlobalEnv,
      inherits = FALSE
    )
  }

  on.exit(
    {
      if (existia_semilla) {
        assign(
          ".Random.seed",
          semilla_anterior,
          envir = .GlobalEnv
        )
      } else if (
        exists(
          ".Random.seed",
          envir = .GlobalEnv,
          inherits = FALSE
        )
      ) {
        rm(
          ".Random.seed",
          envir = .GlobalEnv
        )
      }
    },
    add = TRUE
  )

  set.seed(
    as.integer(semilla)
  )

  force(codigo)
}

construir_item_evaluacion <- function(
  pregunta,
  orden,
  incluir_solucion
) {
  generacion <- generar_variables(
    pregunta
  )

  documento <- construir_documento_pregunta(
    pregunta = pregunta,
    valores = generacion$valores,
    incluir_solucion = incluir_solucion
  )

  list(
    orden = as.integer(orden),
    pregunta_id = pregunta$id,
    intentos_generacion = as.integer(
      generacion$intentos
    ),
    valores = generacion$valores,
    documento = documento
  )
}

construir_evaluacion <- function(
  preguntas,
  estudiante,
  semilla,
  incluir_solucion = TRUE
) {
  validar_semilla(
    semilla
  )

  validar_estudiante(
    estudiante
  )

  validar_incluir_solucion(
    incluir_solucion
  )

  validar_lista_preguntas(
    preguntas
  )

  items <- usar_semilla_local(
    semilla,
    {
      lapply(
        seq_along(preguntas),
        function(indice) {
          construir_item_evaluacion(
            pregunta = preguntas[[indice]],
            orden = indice,
            incluir_solucion =
              incluir_solucion
          )
        }
      )
    }
  )

  list(
    formato = "physikos-assessment",
    version_formato = 1L,
    estudiante = list(
      id = estudiante$id,
      nombre = estudiante$nombre
    ),
    semilla = as.integer(semilla),
    incluir_solucion = incluir_solucion,
    total_preguntas = as.integer(
      length(items)
    ),
    preguntas = items
  )
}

# nolint end