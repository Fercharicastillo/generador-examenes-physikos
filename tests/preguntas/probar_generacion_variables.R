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

set.seed(20260806)

resultado <- generar_variables(
  pregunta = pregunta,
  max_intentos = 1000
)

valores <- resultado$valores

print(resultado)

stopifnot(
  valores$velocidad_inicial >= 0,
  valores$velocidad_inicial <= 10,
  valores$velocidad_inicial ==
    floor(valores$velocidad_inicial)
)

stopifnot(
  valores$aceleracion >= 1.5,
  valores$aceleracion <= 4.0,
  valores$aceleracion ==
    round(valores$aceleracion, 1)
)

stopifnot(
  valores$tiempo >= 3,
  valores$tiempo <= 12,
  valores$tiempo ==
    floor(valores$tiempo)
)

stopifnot(
  identical(
    names(valores),
    names(pregunta$variables)
  )
)

restricciones_validas <- vapply(
  seq_along(pregunta$restricciones),
  function(indice) {
    evaluar_expresion(
      expresion =
        pregunta$restricciones[[indice]]$expresion,
      contexto = valores,
      ruta = paste0(
        "restricciones[",
        indice,
        "].expresion"
      )
    )
  },
  logical(1)
)

stopifnot(all(restricciones_validas))

set.seed(20260806)
resultado_1 <- generar_variables(pregunta)

set.seed(20260806)
resultado_2 <- generar_variables(pregunta)

stopifnot(
  identical(
    resultado_1,
    resultado_2
  )
)

set.seed(20260806)

estudiante_1 <- generar_variables(pregunta)
estudiante_2 <- generar_variables(pregunta)
estudiante_3 <- generar_variables(pregunta)

pregunta_imposible <- pregunta

pregunta_imposible$restricciones <- list(
  list(
    descripcion = paste(
      "La velocidad inicial debe ser",
      "mayor que 100."
    ),
    expresion = list(
      operacion = "mayor",
      argumentos = list(
        "velocidad_inicial",
        100
      )
    )
  )
)

error_esperado <- tryCatch(
  {
    generar_variables(
      pregunta = pregunta_imposible,
      max_intentos = 20
    )

    NULL
  },
  error = function(error) {
    error
  }
)

stopifnot(
  inherits(error_esperado, "error")
)

stopifnot(
  grepl(
    "20 intentos",
    conditionMessage(error_esperado),
    fixed = TRUE
  )
)

cat("\nPRUEBAS SUPERADAS\n")
cat("- Se generaron variables enteras y decimales.\n")
cat("- Los valores respetan sus rangos.\n")
cat("- Las restricciones fueron satisfechas.\n")
cat("- La semilla produjo resultados reproducibles.\n")
cat("- Se rechazó una restricción imposible.\n")