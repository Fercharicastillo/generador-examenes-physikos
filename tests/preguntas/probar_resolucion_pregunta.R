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

solucion <- resolver_pregunta(
  pregunta = pregunta,
  valores = valores
)

print(solucion)

stopifnot(
  isTRUE(
    all.equal(
      solucion$resultados$velocidad_final,
      13,
      tolerance = 1e-9
    )
  )
)

stopifnot(
  isTRUE(
    all.equal(
      solucion$resultados$distancia,
      32,
      tolerance = 1e-9
    )
  )
)

stopifnot(
  identical(
    names(solucion$contexto),
    c(
      "velocidad_inicial",
      "aceleracion",
      "tiempo",
      "velocidad_final",
      "distancia"
    )
  )
)

stopifnot(
  length(solucion$pasos) == 2
)

stopifnot(
  solucion$pasos[[1]]$inciso == "a",
  solucion$pasos[[1]]$guardar_como ==
    "velocidad_final",
  solucion$pasos[[1]]$unidad == "m/s"
)

stopifnot(
  solucion$pasos[[2]]$inciso == "b",
  solucion$pasos[[2]]$guardar_como ==
    "distancia",
  solucion$pasos[[2]]$unidad == "m"
)

set.seed(20260806)

generacion <- generar_variables(
  pregunta = pregunta
)

solucion_aleatoria <- resolver_pregunta(
  pregunta = pregunta,
  valores = generacion$valores
)

print(generacion)
print(solucion_aleatoria$resultados)

resultados_validos <- vapply(
  solucion_aleatoria$resultados,
  es_numero_escalar,
  logical(1)
)

stopifnot(all(resultados_validos))

pregunta_secuencial <- pregunta

pregunta_secuencial$solucion$pasos[[2]]$calculo <- list(
  operacion = "multiplicar",
  argumentos = list(
    list(
      operacion = "dividir",
      argumentos = list(
        list(
          operacion = "sumar",
          argumentos = list(
            "velocidad_inicial",
            "velocidad_final"
          )
        ),
        2
      )
    ),
    "tiempo"
  )
)

solucion_secuencial <- resolver_pregunta(
  pregunta = pregunta_secuencial,
  valores = valores
)

stopifnot(
  isTRUE(
    all.equal(
      solucion_secuencial$resultados$distancia,
      32,
      tolerance = 1e-9
    )
  )
)

valores_incompletos <- list(
  velocidad_inicial = 3,
  aceleracion = 2.5
)

error_faltante <- tryCatch(
  {
    resolver_pregunta(
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
  inherits(error_faltante, "error")
)

stopifnot(
  grepl(
    "tiempo",
    conditionMessage(error_faltante),
    fixed = TRUE
  )
)

valores_adicionales <- c(
  valores,
  list(variable_inventada = 10)
)

error_adicional <- tryCatch(
  {
    resolver_pregunta(
      pregunta,
      valores_adicionales
    )

    NULL
  },
  error = function(error) {
    error
  }
)

stopifnot(
  inherits(error_adicional, "error")
)

stopifnot(
  grepl(
    "variable_inventada",
    conditionMessage(error_adicional),
    fixed = TRUE
  )
)

valores_invalidos <- valores
valores_invalidos$tiempo <- "cuatro"

error_no_numerico <- tryCatch(
  {
    resolver_pregunta(
      pregunta,
      valores_invalidos
    )

    NULL
  },
  error = function(error) {
    error
  }
)

stopifnot(
  inherits(error_no_numerico, "error")
)

pregunta_redondeo <- pregunta
pregunta_redondeo$solucion$pasos[[2]]$decimales <- 1

valores_redondeo <- list(
  velocidad_inicial = 1,
  aceleracion = 1.7,
  tiempo = 3
)

solucion_redondeo <- resolver_pregunta(
  pregunta = pregunta_redondeo,
  valores = valores_redondeo
)

stopifnot(
  isTRUE(
    all.equal(
      solucion_redondeo$resultados_exactos$distancia,
      10.65,
      tolerance = 1e-9
    )
  )
)

stopifnot(
  isTRUE(
    all.equal(
      solucion_redondeo$resultados$distancia,
      round(
        solucion_redondeo$resultados_exactos$distancia,
        digits = 1
      ),
      tolerance = 1e-9
    )
  )
)

cat("\nPRUEBAS SUPERADAS\n")
cat("- Se resolvió la velocidad final.\n")
cat("- Se resolvió la distancia.\n")
cat("- Los pasos se procesaron en orden.\n")
cat("- Los resultados se añadieron al contexto.\n")
cat("- Se conservaron valores exactos y redondeados.\n")
cat("- Se rechazaron valores incompletos o inválidos.\n")
cat("- Se diferenciaron valores exactos y redondeados.\n")
