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

renderizado <- renderizar_enunciado(
  pregunta = pregunta,
  valores = valores
)

print(renderizado)

stopifnot(
  grepl(
    "velocidad inicial de 3 m/s",
    renderizado$texto,
    fixed = TRUE
  )
)

stopifnot(
  grepl(
    "2.5 m/s^2",
    renderizado$texto,
    fixed = TRUE
  )
)

stopifnot(
  grepl(
    "durante 4 s",
    renderizado$texto,
    fixed = TRUE
  )
)

stopifnot(
  !grepl(
    "{{",
    renderizado$texto,
    fixed = TRUE
  )
)

stopifnot(
  !grepl(
    "}}",
    renderizado$texto,
    fixed = TRUE
  )
)

stopifnot(
  length(renderizado$incisos) == 2
)

stopifnot(
  renderizado$incisos[[1]]$id == "a",
  renderizado$incisos[[1]]$texto ==
    "Determine la velocidad final."
)

stopifnot(
  renderizado$incisos[[2]]$id == "b",
  renderizado$incisos[[2]]$texto ==
    "Calcule la distancia recorrida."
)

valores_decimales <- list(
  velocidad_inicial = 2,
  aceleracion = 2,
  tiempo = 5
)

renderizado_decimal <- renderizar_enunciado(
  pregunta = pregunta,
  valores = valores_decimales
)

stopifnot(
  grepl(
    "2.0 m/s^2",
    renderizado_decimal$texto,
    fixed = TRUE
  )
)

pregunta_especial <- pregunta

pregunta_especial$enunciado <- paste(
  pregunta_especial$enunciado,
  "La eficiencia estimada es 80% y se representa con valor_total."
)

renderizado_especial <- renderizar_enunciado(
  pregunta = pregunta_especial,
  valores = valores
)

stopifnot(
  grepl(
    "80\\%",
    renderizado_especial$latex,
    fixed = TRUE
  )
)

stopifnot(
  grepl(
    "valor\\_total",
    renderizado_especial$latex,
    fixed = TRUE
  )
)

error_marcador <- tryCatch(
  {
    sustituir_marcadores(
      texto = "Valor: {{variable_inexistente}}",
      reemplazos = c(
        velocidad = "10"
      )
    )

    NULL
  },
  error = function(error) {
    error
  }
)

stopifnot(
  inherits(error_marcador, "error")
)

stopifnot(
  grepl(
    "variable_inexistente",
    conditionMessage(error_marcador),
    fixed = TRUE
  )
)

valores_incompletos <- list(
  velocidad_inicial = 3,
  aceleracion = 2.5
)

error_valores <- tryCatch(
  {
    renderizar_enunciado(
      pregunta,
      valores_incompletos
    )

    NULL
  },
  error = function(error) {
    error
  }
)

stopifnot(
  inherits(error_valores, "error")
)

stopifnot(
  grepl(
    "tiempo",
    conditionMessage(error_valores),
    fixed = TRUE
  )
)

cat("\nPRUEBAS SUPERADAS\n")
cat("- Se sustituyeron todos los marcadores.\n")
cat("- Los enteros se formatearon correctamente.\n")
cat("- Los decimales conservaron su precisión visual.\n")
cat("- Los incisos fueron renderizados.\n")
cat("- Los caracteres especiales se escaparon para LaTeX.\n")
cat("- Se rechazaron marcadores y valores inexistentes.\n")