# Las funciones probadas se cargan mÃ¡s abajo con source().
# nolint start: object_usage_linter.

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

ruta_pregunta <- file.path(
  carpeta_proyecto,
  "preguntas",
  "cinematica",
  "mrua-001.json"
)

resultado_validacion <- validar_archivo_pregunta(
  ruta_pregunta
)

stopifnot(
  resultado_validacion$valida
)

pregunta <- resultado_validacion$pregunta

contexto <- list(
  velocidad_inicial = 3,
  aceleracion = 2.5,
  tiempo = 4
)

# Evaluar la velocidad final del inciso a.
expresion_velocidad <- pregunta$
  solucion$pasos[[1]]$calculo

velocidad_final <- evaluar_expresion(
  expresion = expresion_velocidad,
  contexto = contexto,
  ruta = "solucion.velocidad_final"
)

cat(
  "Velocidad final:",
  velocidad_final,
  "m/s\n"
)

stopifnot(
  isTRUE(
    all.equal(
      velocidad_final,
      13,
      tolerance = 1e-9
    )
  )
)

# Los resultados calculados pueden agregarse
# posteriormente al contexto.
contexto$velocidad_final <- velocidad_final

# Evaluar la distancia del inciso b.
expresion_distancia <- pregunta$
  solucion$pasos[[2]]$calculo

distancia <- evaluar_expresion(
  expresion = expresion_distancia,
  contexto = contexto,
  ruta = "solucion.distancia"
)

cat(
  "Distancia:",
  distancia,
  "m\n"
)

stopifnot(
  isTRUE(
    all.equal(
      distancia,
      32,
      tolerance = 1e-9
    )
  )
)

# Evaluar la restricción:
# velocidad final <= 60
expresion_restriccion <- pregunta$
  restricciones[[1]]$expresion

restriccion_cumplida <- evaluar_expresion(
  expresion = expresion_restriccion,
  contexto = contexto,
  ruta = "restricciones[1]"
)

cat(
  "Restricción cumplida:",
  restriccion_cumplida,
  "\n"
)

stopifnot(
  isTRUE(restriccion_cumplida)
)

capturar_error <- function(expresion, contexto) {
  tryCatch(
    {
      evaluar_expresion(
        expresion = expresion,
        contexto = contexto
      )

      NULL
    },
    error = function(error) {
      error
    }
  )
}

# Prueba negativa 1:
# no debe permitir divisiones por cero.
expresion_division_cero <- list(
  operacion = "dividir",
  argumentos = list(
    10,
    0
  )
)

error_division <- capturar_error(
  expresion_division_cero,
  contexto
)

stopifnot(
  inherits(error_division, "error")
)

stopifnot(
  grepl(
    "división por cero",
    conditionMessage(error_division),
    fixed = TRUE
  )
)

# Prueba negativa 2:
# no debe aceptar referencias inexistentes.
expresion_referencia_invalida <- list(
  operacion = "sumar",
  argumentos = list(
    "variable_inexistente",
    1
  )
)

error_referencia <- capturar_error(
  expresion_referencia_invalida,
  contexto
)

stopifnot(
  inherits(error_referencia, "error")
)

stopifnot(
  grepl(
    "no existe",
    conditionMessage(error_referencia),
    fixed = TRUE
  )
)

# Prueba negativa 3:
# no debe ejecutar operaciones arbitrarias.
expresion_peligrosa <- list(
  operacion = "system",
  argumentos = list(
    1,
    2
  )
)

error_operacion <- capturar_error(
  expresion_peligrosa,
  contexto
)

stopifnot(
  inherits(error_operacion, "error")
)

stopifnot(
  grepl(
    "no está permitida",
    conditionMessage(error_operacion),
    fixed = TRUE
  )
)

# Prueba de igualdad con tolerancia.
expresion_igualdad <- list(
  operacion = "igual",
  argumentos = list(
    0.1 + 0.2,
    0.3
  )
)

igualdad <- evaluar_expresion(
  expresion = expresion_igualdad,
  contexto = contexto
)

stopifnot(
  isTRUE(igualdad)
)

cat("\nPRUEBAS SUPERADAS\n")
cat("- Se evaluó la velocidad final.\n")
cat("- Se evaluó la distancia.\n")
cat("- Se evaluó una restricción.\n")
cat("- Se rechazó la división por cero.\n")
cat("- Se rechazó una referencia inexistente.\n")
cat("- Se rechazó una operación no permitida.\n")
cat("- Se verificó la igualdad con tolerancia.\n")

# nolint end
