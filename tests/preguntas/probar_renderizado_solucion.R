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

solucion_resuelta <- resolver_pregunta(
  pregunta = pregunta,
  valores = valores
)

solucion_renderizada <- renderizar_solucion(
  pregunta = pregunta,
  solucion_resuelta = solucion_resuelta
)

print(solucion_renderizada)

stopifnot(
  solucion_renderizada$id ==
    "cinematica-mrua-001"
)

stopifnot(
  solucion_renderizada$titulo ==
    "Movimiento uniformemente acelerado"
)

stopifnot(
  length(solucion_renderizada$pasos) == 2
)

paso_velocidad <- solucion_renderizada$
  pasos[[1]]

stopifnot(
  paso_velocidad$inciso == "a"
)

stopifnot(
  paso_velocidad$resultado ==
    "velocidad_final"
)

stopifnot(
  paso_velocidad$valor == 13
)

stopifnot(
  paso_velocidad$valor_exacto == 13
)

stopifnot(
  paso_velocidad$valor_formateado ==
    "13.00"
)

stopifnot(
  paso_velocidad$unidad == "m/s"
)

stopifnot(
  grepl(
    "13.00",
    paso_velocidad$resultado_texto,
    fixed = TRUE
  )
)

stopifnot(
  grepl(
    "13.00",
    paso_velocidad$resultado_latex,
    fixed = TRUE
  )
)

paso_distancia <- solucion_renderizada$
  pasos[[2]]

stopifnot(
  paso_distancia$inciso == "b"
)

stopifnot(
  paso_distancia$resultado == "distancia"
)

stopifnot(
  paso_distancia$valor == 32
)

stopifnot(
  paso_distancia$valor_formateado ==
    "32.00"
)

stopifnot(
  paso_distancia$unidad == "m"
)

stopifnot(
  grepl(
    "\\frac",
    paso_distancia$formula_latex,
    fixed = TRUE
  )
)

pregunta_especial <- pregunta

pregunta_especial$
  solucion$pasos[[1]]$explicacion <- paste(
  "La eficiencia es 80%",
  "y usamos la variable v_0."
)

solucion_especial <- resolver_pregunta(
  pregunta = pregunta_especial,
  valores = valores
)

renderizado_especial <- renderizar_solucion(
  pregunta = pregunta_especial,
  solucion_resuelta = solucion_especial
)

stopifnot(
  grepl(
    "80\\%",
    renderizado_especial$
      pasos[[1]]$explicacion_latex,
    fixed = TRUE
  )
)

stopifnot(
  grepl(
    "v\\_0",
    renderizado_especial$
      pasos[[1]]$explicacion_latex,
    fixed = TRUE
  )
)

solucion_inconsistente <- solucion_resuelta

solucion_inconsistente$
  pasos[[1]]$guardar_como <- "otro_resultado"

error_inconsistente <- tryCatch(
  {
    renderizar_solucion(
      pregunta = pregunta,
      solucion_resuelta =
        solucion_inconsistente
    )

    NULL
  },
  error = function(error) {
    error
  }
)

stopifnot(
  inherits(error_inconsistente, "error")
)

stopifnot(
  grepl(
    "otro_resultado",
    conditionMessage(error_inconsistente),
    fixed = TRUE
  )
)

solucion_incompleta <- solucion_resuelta

solucion_incompleta$pasos <-
  solucion_incompleta$pasos[1]

error_incompleto <- tryCatch(
  {
    renderizar_solucion(
      pregunta,
      solucion_incompleta
    )

    NULL
  },
  error = function(error) {
    error
  }
)

stopifnot(
  inherits(error_incompleto, "error")
)

stopifnot(
  grepl(
    "2 pasos",
    conditionMessage(error_incompleto),
    fixed = TRUE
  )
)

cat("\nPRUEBAS SUPERADAS\n")
cat("- Se renderizaron todos los pasos.\n")
cat("- Los resultados conservaron sus decimales.\n")
cat("- Los valores exactos permanecieron disponibles.\n")
cat("- Las explicaciones se escaparon para LaTeX.\n")
cat("- Las fórmulas LaTeX conservaron sus comandos.\n")
cat("- Se rechazaron soluciones inconsistentes.\n")