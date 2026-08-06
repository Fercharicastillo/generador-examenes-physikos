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

ruta_pregunta <- file.path(
  carpeta_proyecto,
  "preguntas",
  "cinematica",
  "mrua-001.json"
)

resultado <- validar_archivo_pregunta(
  ruta_pregunta
)

cat("\nResultado de validación\n")
cat("-----------------------\n")
cat("Válida:", resultado$valida, "\n")

if (length(resultado$errores) > 0) {
  cat("\nErrores:\n")

  for (error in resultado$errores) {
    cat("-", error, "\n")
  }
}

if (length(resultado$advertencias) > 0) {
  cat("\nAdvertencias:\n")

  for (
    advertencia in resultado$advertencias
  ) {
    cat("-", advertencia, "\n")
  }
}

stopifnot(resultado$valida)

pregunta_original <- resultado$pregunta

# Prueba negativa 1:
# utilizar una operación que no está permitida.
pregunta_operacion_invalida <-
  pregunta_original

pregunta_operacion_invalida$
  solucion$pasos[[1]]$calculo$operacion <-
  "system"

resultado_operacion_invalida <-
  validar_pregunta(
    pregunta_operacion_invalida
  )

stopifnot(
  !resultado_operacion_invalida$valida
)

stopifnot(
  any(
    grepl(
      "no está permitida",
      resultado_operacion_invalida$errores,
      fixed = TRUE
    )
  )
)

# Prueba negativa 2:
# solicitar un resultado inexistente.
pregunta_resultado_invalido <-
  pregunta_original

pregunta_resultado_invalido$
  incisos[[1]]$resultado <-
  "resultado_inexistente"

resultado_resultado_invalido <-
  validar_pregunta(
    pregunta_resultado_invalido
  )

stopifnot(
  !resultado_resultado_invalido$valida
)

stopifnot(
  any(
    grepl(
      "no lo genera",
      resultado_resultado_invalido$errores,
      fixed = TRUE
    )
  )
)

cat(
  "\nPRUEBAS SUPERADAS:",
  "el validador acepta la pregunta correcta",
  "y rechaza las modificaciones inválidas.\n"
)