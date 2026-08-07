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
pregunta_2$version_pregunta <- 1

estudiante <- list(
  id = "estudiante-001",
  nombre = "Estudiante de prueba"
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

print(evaluacion)

stopifnot(
  evaluacion$formato ==
    "physikos-assessment"
)

stopifnot(
  evaluacion$version_formato == 1L
)

stopifnot(
  evaluacion$semilla == 20260806L
)

stopifnot(
  evaluacion$total_preguntas == 2L
)

stopifnot(
  length(evaluacion$preguntas) == 2
)

stopifnot(
  identical(
    evaluacion$estudiante,
    estudiante
  )
)

ordenes <- vapply(
  evaluacion$preguntas,
  function(item) {
    item$orden
  },
  integer(1)
)

stopifnot(
  identical(
    ordenes,
    c(1L, 2L)
  )
)

ids <- vapply(
  evaluacion$preguntas,
  function(item) {
    item$pregunta_id
  },
  character(1)
)

stopifnot(
  identical(
    ids,
    c(
      "cinematica-mrua-001",
      "cinematica-mrua-002"
    )
  )
)

for (item in evaluacion$preguntas) {
  stopifnot(
    is.list(item$valores)
  )

  stopifnot(
    item$intentos_generacion >= 1L
  )

  stopifnot(
    is.list(item$documento)
  )

  stopifnot(
    !is.null(
      item$documento$solucion
    )
  )
}

evaluacion_repetida <- construir_evaluacion(
  preguntas = list(
    pregunta_1,
    pregunta_2
  ),
  estudiante = estudiante,
  semilla = 20260806,
  incluir_solucion = TRUE
)

stopifnot(
  identical(
    evaluacion,
    evaluacion_repetida
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

stopifnot(
  identical(
    evaluacion_sin_solucion$incluir_solucion,
    FALSE
  )
)

for (
  item in evaluacion_sin_solucion$preguntas
) {
  stopifnot(
    is.null(
      item$documento$solucion
    )
  )
}

valores_con_solucion <- lapply(
  evaluacion$preguntas,
  function(item) {
    item$valores
  }
)

valores_sin_solucion <- lapply(
  evaluacion_sin_solucion$preguntas,
  function(item) {
    item$valores
  }
)

stopifnot(
  identical(
    valores_con_solucion,
    valores_sin_solucion
  )
)

set.seed(12345)
valor_esperado <- runif(1)

set.seed(12345)

invisible(
  construir_evaluacion(
    preguntas = list(
      pregunta_1,
      pregunta_2
    ),
    estudiante = estudiante,
    semilla = 20260806,
    incluir_solucion = TRUE
  )
)

valor_obtenido <- runif(1)

stopifnot(
  identical(
    valor_esperado,
    valor_obtenido
  )
)

error_duplicadas <- tryCatch(
  {
    construir_evaluacion(
      preguntas = list(
        pregunta_1,
        pregunta_1
      ),
      estudiante = estudiante,
      semilla = 20260806
    )

    NULL
  },
  error = function(error) {
    error
  }
)

stopifnot(
  inherits(
    error_duplicadas,
    "error"
  )
)

stopifnot(
  grepl(
    "preguntas repetidas",
    conditionMessage(error_duplicadas),
    fixed = TRUE
  )
)

error_estudiante <- tryCatch(
  {
    construir_evaluacion(
      preguntas = list(
        pregunta_1
      ),
      estudiante = list(
        id = "estudiante-001"
      ),
      semilla = 20260806
    )

    NULL
  },
  error = function(error) {
    error
  }
)

stopifnot(
  inherits(
    error_estudiante,
    "error"
  )
)

stopifnot(
  grepl(
    "nombre",
    conditionMessage(error_estudiante),
    fixed = TRUE
  )
)

cat("\nPRUEBAS SUPERADAS\n")
cat("- Se construyó una evaluación con dos preguntas.\n")
cat("- Las preguntas conservaron su orden.\n")
cat("- La semilla produjo resultados reproducibles.\n")
cat("- Se construyó una evaluación sin soluciones.\n")
cat("- Ambas versiones conservaron los mismos valores.\n")
cat("- La secuencia aleatoria global fue restaurada.\n")
cat("- Se rechazaron preguntas repetidas.\n")
cat("- Se rechazó un estudiante incompleto.\n")