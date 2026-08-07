# Este módulo usa funciones cargadas previamente con source().
# nolint start: object_usage_linter.

validar_documento_intermedio <- function(
  documento
) {
  if (!is.list(documento)) {
    stop(
      "'documento' debe ser una lista.",
      call. = FALSE
    )
  }

  campos_obligatorios <- c(
    "formato",
    "version_formato",
    "id",
    "titulo",
    "incluir_solucion",
    "valores",
    "pregunta",
    "solucion"
  )

  campos_faltantes <- setdiff(
    campos_obligatorios,
    names(documento)
  )

  if (length(campos_faltantes) > 0) {
    stop(
      paste0(
        "El documento no contiene los campos: ",
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
      documento$formato,
      "physikos-document-question"
    )
  ) {
    stop(
      paste0(
        "El formato del documento no es compatible: ",
        documento$formato,
        "."
      ),
      call. = FALSE
    )
  }

  if (
    !is.numeric(documento$version_formato) ||
      length(documento$version_formato) != 1 ||
      is.na(documento$version_formato) ||
      documento$version_formato != 1
  ) {
    stop(
      paste(
        "La versión del documento",
        "no es compatible."
      ),
      call. = FALSE
    )
  }

  if (!es_texto_escalar(documento$id)) {
    stop(
      "El documento no contiene un identificador válido.",
      call. = FALSE
    )
  }

  if (!es_texto_escalar(documento$titulo)) {
    stop(
      "El documento no contiene un título válido.",
      call. = FALSE
    )
  }

  if (
    !is.logical(documento$incluir_solucion) ||
      length(documento$incluir_solucion) != 1 ||
      is.na(documento$incluir_solucion)
  ) {
    stop(
      paste(
        "'documento$incluir_solucion'",
        "debe ser TRUE o FALSE."
      ),
      call. = FALSE
    )
  }

  if (!is.list(documento$pregunta)) {
    stop(
      "'documento$pregunta' debe ser una lista.",
      call. = FALSE
    )
  }

  campos_pregunta <- c(
    "id",
    "titulo",
    "texto",
    "latex",
    "incisos",
    "valores_formateados"
  )

  faltantes_pregunta <- setdiff(
    campos_pregunta,
    names(documento$pregunta)
  )

  if (length(faltantes_pregunta) > 0) {
    stop(
      paste0(
        "La pregunta renderizada no contiene: ",
        paste(
          faltantes_pregunta,
          collapse = ", "
        ),
        "."
      ),
      call. = FALSE
    )
  }

  if (!is.list(documento$pregunta$incisos)) {
    stop(
      paste(
        "'documento$pregunta$incisos'",
        "debe ser una lista."
      ),
      call. = FALSE
    )
  }

  if (documento$incluir_solucion) {
    if (!is.list(documento$solucion)) {
      stop(
        paste(
          "El documento indica que debe incluir",
          "una solución, pero no la contiene."
        ),
        call. = FALSE
      )
    }

    if (!is.list(documento$solucion$pasos)) {
      stop(
        paste(
          "'documento$solucion$pasos'",
          "debe ser una lista."
        ),
        call. = FALSE
      )
    }
  } else if (!is.null(documento$solucion)) {
    stop(
      paste(
        "El documento no debe incluir una solución",
        "cuando 'incluir_solucion' es FALSE."
      ),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

renderizar_incisos_latex <- function(
  incisos
) {
  if (!is.list(incisos) || length(incisos) == 0) {
    stop(
      "Se necesita al menos un inciso.",
      call. = FALSE
    )
  }

  elementos <- vapply(
    seq_along(incisos),
    function(indice) {
      inciso <- incisos[[indice]]

      if (
        !is.list(inciso) ||
          !es_texto_escalar(inciso$latex)
      ) {
        stop(
          paste0(
            "El inciso ",
            indice,
            " no contiene texto LaTeX válido."
          ),
          call. = FALSE
        )
      }

      paste0(
        "\\item ",
        inciso$latex
      )
    },
    character(1)
  )

  paste(
    c(
      "\\begin{enumerate}",
      "\\renewcommand{\\labelenumi}{(\\alph{enumi})}",
      elementos,
      "\\end{enumerate}"
    ),
    collapse = "\n"
  )
}

renderizar_paso_latex <- function(
  paso,
  indice
) {
  if (!is.list(paso)) {
    stop(
      paste0(
        "El paso ",
        indice,
        " debe ser una lista."
      ),
      call. = FALSE
    )
  }

  campos_obligatorios <- c(
    "inciso",
    "explicacion_latex",
    "formula_latex",
    "resultado_latex"
  )

  campos_faltantes <- setdiff(
    campos_obligatorios,
    names(paso)
  )

  if (length(campos_faltantes) > 0) {
    stop(
      paste0(
        "El paso ",
        indice,
        " no contiene: ",
        paste(
          campos_faltantes,
          collapse = ", "
        ),
        "."
      ),
      call. = FALSE
    )
  }

  if (!es_texto_escalar(paso$inciso)) {
    stop(
      paste0(
        "El inciso del paso ",
        indice,
        " no es válido."
      ),
      call. = FALSE
    )
  }

  if (!es_texto_escalar(paso$explicacion_latex)) {
    stop(
      paste0(
        "La explicación del paso ",
        indice,
        " no es válida."
      ),
      call. = FALSE
    )
  }

  if (!es_texto_escalar(paso$formula_latex)) {
    stop(
      paste0(
        "La fórmula del paso ",
        indice,
        " no es válida."
      ),
      call. = FALSE
    )
  }

  if (!es_texto_escalar(paso$resultado_latex)) {
    stop(
      paste0(
        "El resultado del paso ",
        indice,
        " no es válido."
      ),
      call. = FALSE
    )
  }

  inciso_latex <- escapar_latex(
    paso$inciso
  )

  paste(
    c(
      paste0(
        "\\noindent\\textbf{Inciso (",
        inciso_latex,
        ")}\\par"
      ),
      "",
      paso$explicacion_latex,
      "",
      "\\[",
      paso$formula_latex,
      "\\]",
      "",
      paste0(
        "\\noindent\\textbf{Resultado:} ",
        paso$resultado_latex,
        "\\par"
      )
    ),
    collapse = "\n"
  )
}

renderizar_pasos_latex <- function(
  pasos
) {
  if (!is.list(pasos) || length(pasos) == 0) {
    stop(
      "La solución debe contener al menos un paso.",
      call. = FALSE
    )
  }

  pasos_latex <- vapply(
    seq_along(pasos),
    function(indice) {
      renderizar_paso_latex(
        paso = pasos[[indice]],
        indice = indice
      )
    },
    character(1)
  )

  pasos_latex
}

renderizar_examen_latex <- function(
  documento
) {
  titulo_latex <- escapar_latex(
    documento$titulo
  )

  enunciado_latex <- documento$
    pregunta$latex

  if (!es_texto_escalar(enunciado_latex)) {
    stop(
      "El enunciado LaTeX no es válido.",
      call. = FALSE
    )
  }

  incisos_latex <- renderizar_incisos_latex(
    documento$pregunta$incisos
  )

  fragmento_titulo <- paste0(
    "\\noindent\\textbf{",
    titulo_latex,
    "}\\par"
  )

  contenido <- paste(
    c(
      fragmento_titulo,
      "",
      enunciado_latex,
      "",
      incisos_latex
    ),
    collapse = "\n"
  )

  list(
    titulo = fragmento_titulo,
    enunciado = enunciado_latex,
    incisos = incisos_latex,
    contenido = contenido
  )
}

renderizar_solucion_latex <- function(
  documento,
  examen_latex
) {
  if (!documento$incluir_solucion) {
    return(NULL)
  }

  pasos_latex <- renderizar_pasos_latex(
    documento$solucion$pasos
  )

  encabezado <- paste0(
    "\\noindent\\textbf{Solución}\\par"
  )

  bloque_pasos <- paste(
    pasos_latex,
    collapse = "\n\n\\medskip\n\n"
  )

  contenido <- paste(
    c(
      examen_latex$contenido,
      "",
      "\\medskip",
      "",
      encabezado,
      "",
      bloque_pasos
    ),
    collapse = "\n"
  )

  list(
    encabezado = encabezado,
    pasos = as.list(pasos_latex),
    contenido = contenido
  )
}

renderizar_documento_latex <- function(
  documento
) {
  validar_documento_intermedio(
    documento
  )

  examen_latex <- renderizar_examen_latex(
    documento
  )

  solucion_latex <- renderizar_solucion_latex(
    documento = documento,
    examen_latex = examen_latex
  )

  list(
    formato = "physikos-latex-fragments",
    version_formato = 1L,
    id = documento$id,
    examen = examen_latex,
    solucion = solucion_latex
  )
}

# nolint end