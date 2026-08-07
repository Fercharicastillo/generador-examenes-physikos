carpeta_proyecto <- normalizePath(
  ".",
  winslash = "/",
  mustWork = TRUE
)

carpeta_plantillas <- file.path(
  carpeta_proyecto,
  "plantillas",
  "latex"
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
    "generar_variables.R"
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
    "evaluaciones",
    "construir_evaluacion.R"
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

source(
  file.path(
    carpeta_proyecto,
    "R",
    "latex",
    "renderizar_evaluacion_latex.R"
  ),
  encoding = "UTF-8"
)

source(
  file.path(
    carpeta_proyecto,
    "R",
    "latex",
    "construir_archivo_tex.R"
  ),
  encoding = "UTF-8"
)

pregunta_1 <- jsonlite::read_json(
  file.path(
    carpeta_proyecto,
    "preguntas",
    "cinematica",
    "mrua-001.json"
  ),
  simplifyVector = FALSE
)

pregunta_2 <- pregunta_1
pregunta_2$id <- "cinematica-mrua-002"
pregunta_2$titulo <-
  "Segunda aplicación de movimiento acelerado"

estudiante <- list(
  id = "estudiante-001",
  nombre = "Ana Pérez & Compañía"
)

evaluacion <- construir_evaluacion(
  preguntas = list(
    pregunta_1,
    pregunta_2
  ),
  estudiante = estudiante,
  semilla = 20260806,
  incluir_solucion = TRUE
)

fragmentos <- renderizar_evaluacion_latex(
  evaluacion
)

cat(
  "\nEXAMEN COMPLETO\n",
  fragmentos$examen$contenido,
  "\n"
)

cat(
  "\nSOLUCIONARIO COMPLETO\n",
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
    "evaluacion-estudiante-001-20260806"
)

stopifnot(
  length(
    fragmentos$examen$preguntas
  ) == 2
)

stopifnot(
  length(
    fragmentos$solucion$preguntas
  ) == 2
)

stopifnot(
  grepl(
    "Ana Pérez \\& Compañía",
    fragmentos$examen$estudiante,
    fixed = TRUE
  )
)

stopifnot(
  grepl(
    "estudiante-001",
    fragmentos$examen$estudiante,
    fixed = TRUE
  )
)

stopifnot(
  grepl(
    "\\section*{Pregunta 1}",
    fragmentos$examen$preguntas[[1]],
    fixed = TRUE
  )
)

stopifnot(
  grepl(
    "\\section*{Pregunta 2}",
    fragmentos$examen$preguntas[[2]],
    fixed = TRUE
  )
)

posicion_1 <- regexpr(
  "\\section*{Pregunta 1}",
  fragmentos$examen$contenido,
  fixed = TRUE
)[[1]]

posicion_2 <- regexpr(
  "\\section*{Pregunta 2}",
  fragmentos$examen$contenido,
  fixed = TRUE
)[[1]]

stopifnot(
  posicion_1 > 0,
  posicion_2 > posicion_1
)

stopifnot(
  grepl(
    "\\textbf{Solución}",
    fragmentos$solucion$preguntas[[1]],
    fixed = TRUE
  )
)

stopifnot(
  grepl(
    "\\textbf{Solución}",
    fragmentos$solucion$preguntas[[2]],
    fixed = TRUE
  )
)

stopifnot(
  grepl(
    "\\frac{1}{2}",
    fragmentos$solucion$contenido,
    fixed = TRUE
  )
)

archivo_examen <- construir_archivo_tex(
  fragmentos = fragmentos,
  plantilla = "clasica",
  tipo = "examen",
  carpeta_plantillas = carpeta_plantillas
)

archivo_solucion <- construir_archivo_tex(
  fragmentos = fragmentos,
  plantilla = "minimalista",
  tipo = "solucion",
  carpeta_plantillas = carpeta_plantillas
)

stopifnot(
  grepl(
    "\\documentclass",
    archivo_examen$contenido,
    fixed = TRUE
  )
)

stopifnot(
  grepl(
    "\\section*{Pregunta 2}",
    archivo_examen$contenido,
    fixed = TRUE
  )
)

stopifnot(
  grepl(
    "\\documentclass",
    archivo_solucion$contenido,
    fixed = TRUE
  )
)

evaluacion_sin_solucion <- construir_evaluacion(
  preguntas = list(
    pregunta_1,
    pregunta_2
  ),
  estudiante = estudiante,
  semilla = 20260806,
  incluir_solucion = FALSE
)

fragmentos_sin_solucion <-
  renderizar_evaluacion_latex(
    evaluacion_sin_solucion
  )

stopifnot(
  is.list(
    fragmentos_sin_solucion$examen
  )
)

stopifnot(
  length(
    fragmentos_sin_solucion$
      examen$preguntas
  ) == 2
)

stopifnot(
  is.null(
    fragmentos_sin_solucion$solucion
  )
)

evaluacion_invalida <- evaluacion
evaluacion_invalida$total_preguntas <- 3L

error_evaluacion <- tryCatch(
  {
    renderizar_evaluacion_latex(
      evaluacion_invalida
    )

    NULL
  },
  error = function(error) {
    error
  }
)

stopifnot(
  inherits(
    error_evaluacion,
    "error"
  )
)

stopifnot(
  grepl(
    "declara 3 preguntas",
    conditionMessage(error_evaluacion),
    fixed = TRUE
  )
)

cat("\nPRUEBAS SUPERADAS\n")
cat("- Se renderizó una evaluación con dos preguntas.\n")
cat("- Los datos del estudiante se escaparon para LaTeX.\n")
cat("- Las preguntas conservaron su orden.\n")
cat("- Se construyó un único fragmento de examen.\n")
cat("- Se construyó un único fragmento de solucionario.\n")
cat("- Los fragmentos son compatibles con las plantillas.\n")
cat("- Se renderizó una evaluación sin soluciones.\n")
cat("- Se rechazó una evaluación inconsistente.\n")
