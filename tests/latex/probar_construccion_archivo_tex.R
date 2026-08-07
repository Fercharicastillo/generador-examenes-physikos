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

source(
  file.path(
    carpeta_proyecto,
    "R",
    "latex",
    "construir_archivo_tex.R"
  ),
  encoding = "UTF-8"
)

pregunta <- jsonlite::read_json(
  file.path(
    carpeta_proyecto,
    "preguntas",
    "cinematica",
    "mrua-001.json"
  ),
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

examen_clasico <- construir_archivo_tex(
  fragmentos = fragmentos,
  plantilla = "clasica",
  tipo = "examen",
  carpeta_plantillas = carpeta_plantillas
)

cat(
  "\nEXAMEN CLÁSICO\n",
  examen_clasico$contenido,
  "\n"
)

stopifnot(
  examen_clasico$plantilla == "clasica"
)

stopifnot(
  examen_clasico$tipo == "examen"
)

stopifnot(
  grepl(
    "\\documentclass",
    examen_clasico$contenido,
    fixed = TRUE
  )
)

stopifnot(
  grepl(
    "\\begin{document}",
    examen_clasico$contenido,
    fixed = TRUE
  )
)

stopifnot(
  grepl(
    "\\end{document}",
    examen_clasico$contenido,
    fixed = TRUE
  )
)

stopifnot(
  grepl(
    "physikosblue",
    examen_clasico$contenido,
    fixed = TRUE
  )
)

examen_minimalista <- construir_archivo_tex(
  fragmentos = fragmentos,
  plantilla = "minimalista",
  tipo = "examen",
  carpeta_plantillas = carpeta_plantillas
)

cat(
  "\nEXAMEN MINIMALISTA\n",
  examen_minimalista$contenido,
  "\n"
)

stopifnot(
  examen_minimalista$plantilla ==
    "minimalista"
)

stopifnot(
  !grepl(
    "physikosblue",
    examen_minimalista$contenido,
    fixed = TRUE
  )
)

stopifnot(
  grepl(
    fragmentos$examen$contenido,
    examen_clasico$contenido,
    fixed = TRUE
  )
)

stopifnot(
  grepl(
    fragmentos$examen$contenido,
    examen_minimalista$contenido,
    fixed = TRUE
  )
)

stopifnot(
  !identical(
    examen_clasico$contenido,
    examen_minimalista$contenido
  )
)

stopifnot(
  !grepl(
    "@@",
    examen_clasico$contenido,
    fixed = TRUE
  )
)

stopifnot(
  !grepl(
    "@@",
    examen_minimalista$contenido,
    fixed = TRUE
  )
)

solucion_clasica <- construir_archivo_tex(
  fragmentos = fragmentos,
  plantilla = "clasica",
  tipo = "solucion",
  carpeta_plantillas = carpeta_plantillas
)

solucion_minimalista <- construir_archivo_tex(
  fragmentos = fragmentos,
  plantilla = "minimalista",
  tipo = "solucion",
  carpeta_plantillas = carpeta_plantillas
)

stopifnot(
  grepl(
    "13.00",
    solucion_clasica$contenido,
    fixed = TRUE
  )
)

stopifnot(
  grepl(
    "32.00",
    solucion_clasica$contenido,
    fixed = TRUE
  )
)

stopifnot(
  grepl(
    "\\frac{1}{2}",
    solucion_clasica$contenido,
    fixed = TRUE
  )
)

stopifnot(
  grepl(
    fragmentos$solucion$contenido,
    solucion_clasica$contenido,
    fixed = TRUE
  )
)

stopifnot(
  grepl(
    fragmentos$solucion$contenido,
    solucion_minimalista$contenido,
    fixed = TRUE
  )
)

stopifnot(
  !identical(
    solucion_clasica$contenido,
    solucion_minimalista$contenido
  )
)

stopifnot(
  examen_clasico$nombre_archivo ==
    "cinematica-mrua-001_examen.tex"
)

stopifnot(
  solucion_clasica$nombre_archivo ==
    "cinematica-mrua-001_solucion.tex"
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

examen_sin_solucion <- construir_archivo_tex(
  fragmentos = fragmentos_sin_solucion,
  plantilla = "minimalista",
  tipo = "examen",
  carpeta_plantillas = carpeta_plantillas
)

stopifnot(
  grepl(
    "\\begin{document}",
    examen_sin_solucion$contenido,
    fixed = TRUE
  )
)

error_sin_solucion <- tryCatch(
  {
    construir_archivo_tex(
      fragmentos = fragmentos_sin_solucion,
      plantilla = "minimalista",
      tipo = "solucion",
      carpeta_plantillas = carpeta_plantillas
    )

    NULL
  },
  error = function(error) {
    error
  }
)

stopifnot(
  inherits(error_sin_solucion, "error")
)

stopifnot(
  grepl(
    "no contienen solución",
    conditionMessage(error_sin_solucion),
    fixed = TRUE
  )
)

error_plantilla <- tryCatch(
  {
    construir_archivo_tex(
      fragmentos = fragmentos,
      plantilla = "inexistente",
      tipo = "examen",
      carpeta_plantillas = carpeta_plantillas
    )

    NULL
  },
  error = function(error) {
    error
  }
)

stopifnot(
  inherits(error_plantilla, "error")
)

stopifnot(
  grepl(
    "No existe la plantilla",
    conditionMessage(error_plantilla),
    fixed = TRUE
  )
)

cat("\nPRUEBAS SUPERADAS\n")
cat("- Se construyó el examen con la plantilla clásica.\n")
cat("- Se construyó el examen con la plantilla minimalista.\n")
cat("- Ambas plantillas utilizaron el mismo contenido.\n")
cat("- Los documentos resultantes fueron visualmente diferentes.\n")
cat("- Se construyeron ambos solucionarios.\n")
cat("- No quedaron marcadores sin sustituir.\n")
cat("- Se rechazó un solucionario sin solución.\n")
cat("- Se rechazó una plantilla inexistente.\n")