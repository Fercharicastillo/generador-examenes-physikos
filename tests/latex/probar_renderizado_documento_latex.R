carpeta_proyecto <- normalizePath(
  ".",
  winslash = "/",
  mustWork = TRUE
)

source(
  file.path(
    carpeta_proyecto,
    "R",
    "preguntas",
    "validar_expresion.R"
  ),
  encoding = "UTF-8"
)

source(
  file.path(
    carpeta_proyecto,
    "R",
    "preguntas",
    "validar_pregunta.R"
  ),
  encoding = "UTF-8"
)

source(
  file.path(
    carpeta_proyecto,
    "R",
    "preguntas",
    "evaluar_expresion.R"
  ),
  encoding = "UTF-8"
)

source(
  file.path(
    carpeta_proyecto,
    "R",
    "preguntas",
    "resolver_pregunta.R"
  ),
  encoding = "UTF-8"
)

source(
  file.path(
    carpeta_proyecto,
    "R",
    "preguntas",
    "renderizar_enunciado.R"
  ),
  encoding = "UTF-8"
)

source(
  file.path(
    carpeta_proyecto,
    "R",
    "preguntas",
    "renderizar_solucion.R"
  ),
  encoding = "UTF-8"
)

source(
  file.path(
    carpeta_proyecto,
    "R",
    "preguntas",
    "construir_documento_pregunta.R"
  ),
  encoding = "UTF-8"
)

source(
  file.path(
    carpeta_proyecto,
    "R",
    "latex",
    "renderizar_documento_latex.R"
  ),
  encoding = "UTF-8"
)

ruta_pregunta <- file.path(
  carpeta_proyecto,
  "preguntas",
  "cinematica",
  "mrua-001.json"
)

pregunta <- jsonlite::read_json(
  ruta_pregunta,
  simplifyVector = FALSE
)

valores <- list(
  velocidad_inicial = 3,
  aceleracion = 2.5,
  tiempo = 4
)

documento <- construir_documento_pregunta(
  pregunta = pregunta,
  valores = valores,
  incluir_solucion = TRUE
)

fragmentos <- renderizar_documento_latex(
  documento
)

cat(
  "\nFRAGMENTO DE EXAMEN\n",
  fragmentos$examen$contenido,
  "\n"
)

cat(
  "\nFRAGMENTO DE SOLUCIÓN\n",
  fragmentos$solucion$contenido,
  "\n"
)

stopifnot(
  fragmentos$formato ==
    "physikos-latex-fragments"
)

stopifnot(
  fragmentos$version_formato == 1L
)

stopifnot(
  fragmentos$id ==
    "cinematica-mrua-001"
)

stopifnot(
  is.list(fragmentos$examen)
)

stopifnot(
  is.list(fragmentos$solucion)
)

stopifnot(
  grepl(
    "\\textbf{Movimiento uniformemente acelerado}",
    fragmentos$examen$titulo,
    fixed = TRUE
  )
)

stopifnot(
  grepl(
    "velocidad inicial de 3 m/s",
    fragmentos$examen$enunciado,
    fixed = TRUE
  )
)

stopifnot(
  grepl(
    "\\begin{enumerate}",
    fragmentos$examen$incisos,
    fixed = TRUE
  )
)

stopifnot(
  grepl(
    "\\item Determine la velocidad final.",
    fragmentos$examen$incisos,
    fixed = TRUE
  )
)

stopifnot(
  grepl(
    "\\item Calcule la distancia recorrida.",
    fragmentos$examen$incisos,
    fixed = TRUE
  )
)

stopifnot(
  grepl(
    "\\textbf{Solución}",
    fragmentos$solucion$encabezado,
    fixed = TRUE
  )
)

stopifnot(
  length(fragmentos$solucion$pasos) == 2
)

stopifnot(
  grepl(
    "Inciso (a)",
    fragmentos$solucion$pasos[[1]],
    fixed = TRUE
  )
)

stopifnot(
  grepl(
    "13.00",
    fragmentos$solucion$pasos[[1]],
    fixed = TRUE
  )
)

stopifnot(
  grepl(
    "32.00",
    fragmentos$solucion$pasos[[2]],
    fixed = TRUE
  )
)

stopifnot(
  grepl(
    "\\frac{1}{2}",
    fragmentos$solucion$pasos[[2]],
    fixed = TRUE
  )
)

stopifnot(
  !grepl(
    "{{",
    fragmentos$examen$contenido,
    fixed = TRUE
  )
)

stopifnot(
  !grepl(
    "}}",
    fragmentos$examen$contenido,
    fixed = TRUE
  )
)

stopifnot(
  !grepl(
    "\\documentclass",
    fragmentos$examen$contenido,
    fixed = TRUE
  )
)

stopifnot(
  !grepl(
    "\\begin{document}",
    fragmentos$examen$contenido,
    fixed = TRUE
  )
)

stopifnot(
  !grepl(
    "\\end{document}",
    fragmentos$examen$contenido,
    fixed = TRUE
  )
)

documento_sin_solucion <-
  construir_documento_pregunta(
    pregunta = pregunta,
    valores = valores,
    incluir_solucion = FALSE
  )

fragmentos_sin_solucion <-
  renderizar_documento_latex(
    documento_sin_solucion
  )

stopifnot(
  is.list(
    fragmentos_sin_solucion$examen
  )
)

stopifnot(
  is.null(
    fragmentos_sin_solucion$solucion
  )
)

documento_invalido <- documento
documento_invalido$formato <- "otro-formato"

error_formato <- tryCatch(
  {
    renderizar_documento_latex(
      documento_invalido
    )

    NULL
  },
  error = function(error) {
    error
  }
)

stopifnot(
  inherits(error_formato, "error")
)

stopifnot(
  grepl(
    "no es compatible",
    conditionMessage(error_formato),
    fixed = TRUE
  )
)

documento_inconsistente <- documento
documento_inconsistente["solucion"] <- list(NULL)

error_solucion <- tryCatch(
  {
    renderizar_documento_latex(
      documento_inconsistente
    )

    NULL
  },
  error = function(error) {
    error
  }
)

stopifnot(
  inherits(error_solucion, "error")
)

stopifnot(
  grepl(
    "no la contiene",
    conditionMessage(error_solucion),
    fixed = TRUE
  )
)

cat("\nPRUEBAS SUPERADAS\n")
cat("- Se generó el fragmento LaTeX del examen.\n")
cat("- Se generó el fragmento LaTeX del solucionario.\n")
cat("- Los incisos conservaron su orden.\n")
cat("- Los resultados conservaron sus valores.\n")
cat("- Las fórmulas LaTeX conservaron sus comandos.\n")
cat("- No quedaron marcadores sin sustituir.\n")
cat("- Se produjo correctamente un examen sin solución.\n")
cat("- Se rechazaron documentos intermedios inválidos.\n")