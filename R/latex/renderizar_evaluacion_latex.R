# Este módulo usa funciones cargadas previamente con source().
# nolint start: object_usage_linter.

validar_evaluacion_latex <- function(
  evaluacion
) {
  if (!is.list(evaluacion)) {
    stop(
      "'evaluacion' debe ser una lista.",
      call. = FALSE
    )
  }

  campos_obligatorios <- c(
    "formato",
    "version_formato",
    "estudiante",
    "semilla",
    "incluir_solucion",
    "total_preguntas",
    "preguntas"
  )

  campos_faltantes <- setdiff(
    campos_obligatorios,
    names(evaluacion)
  )

  if (length(campos_faltantes) > 0) {
    stop(
      paste0(
        "La evaluación no contiene los campos: ",
        paste(
          campos_faltantes,
          collapse = ", "
        ),
        "."
      ),
      call. = FALSE
    )
  }

  if (
    !identical(
      evaluacion$formato,
      "physikos-assessment"
    )
  ) {
    stop(
      paste0(
        "El formato de evaluación no es compatible: ",
        evaluacion$formato,
        "."
      ),
      call. = FALSE
    )
  }

  if (
    !is.numeric(evaluacion$version_formato) ||
      length(evaluacion$version_formato) != 1 ||
      is.na(evaluacion$version_formato) ||
      evaluacion$version_formato != 1
  ) {
    stop(
      paste(
        "La versión de la evaluación",
        "no es compatible."
      ),
      call. = FALSE
    )
  }

  if (!is.list(evaluacion$estudiante)) {
    stop(
      "'evaluacion$estudiante' debe ser una lista.",
      call. = FALSE
    )
  }

  if (
    !es_texto_escalar(
      evaluacion$estudiante$id
    )
  ) {
    stop(
      "El identificador del estudiante no es válido.",
      call. = FALSE
    )
  }

  if (
    !es_texto_escalar(
      evaluacion$estudiante$nombre
    )
  ) {
    stop(
      "El nombre del estudiante no es válido.",
      call. = FALSE
    )
  }

  if (
    !is.logical(evaluacion$incluir_solucion) ||
      length(evaluacion$incluir_solucion) != 1 ||
      is.na(evaluacion$incluir_solucion)
  ) {
    stop(
      paste(
        "'evaluacion$incluir_solucion'",
        "debe ser TRUE o FALSE."
      ),
      call. = FALSE
    )
  }

  if (
    !is.numeric(evaluacion$total_preguntas) ||
      length(evaluacion$total_preguntas) != 1 ||
      is.na(evaluacion$total_preguntas) ||
      evaluacion$total_preguntas < 1 ||
      evaluacion$total_preguntas !=
        floor(evaluacion$total_preguntas)
  ) {
    stop(
      "El total de preguntas no es válido.",
      call. = FALSE
    )
  }

  if (
    !is.list(evaluacion$preguntas) ||
      length(evaluacion$preguntas) == 0
  ) {
    stop(
      paste(
        "La evaluación debe contener",
        "al menos una pregunta."
      ),
      call. = FALSE
    )
  }

  if (
    length(evaluacion$preguntas) !=
      evaluacion$total_preguntas
  ) {
    stop(
      paste0(
        "La evaluación declara ",
        evaluacion$total_preguntas,
        " preguntas, pero contiene ",
        length(evaluacion$preguntas),
        "."
      ),
      call. = FALSE
    )
  }

  for (
    indice in seq_along(
      evaluacion$preguntas
    )
  ) {
    item <- evaluacion$preguntas[[indice]]

    if (!is.list(item)) {
      stop(
        paste0(
          "La pregunta ",
          indice,
          " debe ser una lista."
        ),
        call. = FALSE
      )
    }

    campos_item <- c(
      "orden",
      "pregunta_id",
      "valores",
      "documento"
    )

    faltantes_item <- setdiff(
      campos_item,
      names(item)
    )

    if (length(faltantes_item) > 0) {
      stop(
        paste0(
          "La pregunta ",
          indice,
          " no contiene: ",
          paste(
            faltantes_item,
            collapse = ", "
          ),
          "."
        ),
        call. = FALSE
      )
    }

    if (!identical(
      as.integer(item$orden),
      as.integer(indice)
    )) {
      stop(
        paste0(
          "La pregunta ",
          indice,
          " no conserva su orden."
        ),
        call. = FALSE
      )
    }

    if (!is.list(item$documento)) {
      stop(
        paste0(
          "El documento de la pregunta ",
          indice,
          " no es válido."
        ),
        call. = FALSE
      )
    }

    tiene_solucion <- !is.null(
      item$documento$solucion
    )

    if (
      evaluacion$incluir_solucion &&
        !tiene_solucion
    ) {
      stop(
        paste0(
          "La pregunta ",
          indice,
          " no contiene solución."
        ),
        call. = FALSE
      )
    }

    if (
      !evaluacion$incluir_solucion &&
        tiene_solucion
    ) {
      stop(
        paste0(
          "La pregunta ",
          indice,
          " contiene una solución inesperada."
        ),
        call. = FALSE
      )
    }
  }

  invisible(TRUE)
}

construir_id_evaluacion <- function(
  evaluacion
) {
  estudiante_id <- evaluacion$
    estudiante$id

  patron_id <- paste0(
    "^[A-Za-z0-9]",
    "[A-Za-z0-9_-]*$"
  )

  if (!grepl(
    patron_id,
    estudiante_id
  )) {
    stop(
      paste(
        "El identificador del estudiante debe contener",
        "solamente letras, números, guiones",
        "o guiones bajos."
      ),
      call. = FALSE
    )
  }

  paste0(
    "evaluacion-",
    estudiante_id,
    "-",
    as.integer(
      evaluacion$semilla
    )
  )
}

renderizar_estudiante_latex <- function(
  estudiante
) {
  nombre_latex <- escapar_latex(
    estudiante$nombre
  )

  id_latex <- escapar_latex(
    estudiante$id
  )

  paste(
    c(
      "\\noindent",
      paste0(
        "\\textbf{Estudiante:} ",
        nombre_latex,
        "\\par"
      ),
      paste0(
        "\\textbf{Código:} ",
        id_latex,
        "\\par"
      )
    ),
    collapse = "\n"
  )
}

envolver_pregunta_latex <- function(
  contenido,
  orden
) {
  if (
    !is.character(contenido) ||
      length(contenido) != 1 ||
      is.na(contenido) ||
      !nzchar(contenido)
  ) {
    stop(
      paste0(
        "El contenido LaTeX de la pregunta ",
        orden,
        " no es válido."
      ),
      call. = FALSE
    )
  }

  paste(
    c(
      paste0(
        "\\section*{Pregunta ",
        as.integer(orden),
        "}"
      ),
      "",
      contenido
    ),
    collapse = "\n"
  )
}

unir_preguntas_latex <- function(
  preguntas_latex
) {
  if (
    !is.list(preguntas_latex) ||
      length(preguntas_latex) == 0
  ) {
    stop(
      paste(
        "Se necesita al menos un fragmento",
        "de pregunta."
      ),
      call. = FALSE
    )
  }

  valores_validos <- vapply(
    preguntas_latex,
    function(contenido) {
      is.character(contenido) &&
        length(contenido) == 1 &&
        !is.na(contenido) &&
        nzchar(contenido)
    },
    logical(1)
  )

  if (!all(valores_validos)) {
    stop(
      paste(
        "Uno o más fragmentos de pregunta",
        "no son válidos."
      ),
      call. = FALSE
    )
  }

  paste(
    unlist(
      preguntas_latex,
      use.names = FALSE
    ),
    collapse = paste(
      "",
      "\\bigskip",
      "\\hrule",
      "\\bigskip",
      "",
      sep = "\n"
    )
  )
}

renderizar_evaluacion_latex <- function(
  evaluacion
) {
  validar_evaluacion_latex(
    evaluacion
  )

  id_evaluacion <- construir_id_evaluacion(
    evaluacion
  )

  estudiante_latex <-
    renderizar_estudiante_latex(
      evaluacion$estudiante
    )

  fragmentos_preguntas <- lapply(
    evaluacion$preguntas,
    function(item) {
      renderizar_documento_latex(
        item$documento
      )
    }
  )

  preguntas_examen <- lapply(
    seq_along(fragmentos_preguntas),
    function(indice) {
      envolver_pregunta_latex(
        contenido = fragmentos_preguntas[[
          indice
        ]]$examen$contenido,
        orden = indice
      )
    }
  )

  bloque_examen <- unir_preguntas_latex(
    preguntas_examen
  )

  contenido_examen <- paste(
    c(
      estudiante_latex,
      "",
      "\\medskip",
      "",
      bloque_examen
    ),
    collapse = "\n"
  )

  examen <- list(
    estudiante = estudiante_latex,
    preguntas = preguntas_examen,
    contenido = contenido_examen
  )

  solucion <- NULL

  if (evaluacion$incluir_solucion) {
    preguntas_solucion <- lapply(
      seq_along(fragmentos_preguntas),
      function(indice) {
        fragmento_solucion <-
          fragmentos_preguntas[[
            indice
          ]]$solucion

        if (is.null(fragmento_solucion)) {
          stop(
            paste0(
              "No existe el fragmento de solución ",
              "para la pregunta ",
              indice,
              "."
            ),
            call. = FALSE
          )
        }

        envolver_pregunta_latex(
          contenido =
            fragmento_solucion$contenido,
          orden = indice
        )
      }
    )

    bloque_solucion <- unir_preguntas_latex(
      preguntas_solucion
    )

    contenido_solucion <- paste(
      c(
        estudiante_latex,
        "",
        "\\medskip",
        "",
        bloque_solucion
      ),
      collapse = "\n"
    )

    solucion <- list(
      estudiante = estudiante_latex,
      preguntas = preguntas_solucion,
      contenido = contenido_solucion
    )
  }

  list(
    formato = "physikos-latex-fragments",
    version_formato = 1L,
    id = id_evaluacion,
    examen = examen,
    solucion = solucion
  )
}

# nolint end