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

documento_completo <- construir_documento_pregunta(
  pregunta = pregunta,
  valores = valores,
  incluir_solucion = TRUE
)

print(documento_completo)

stopifnot(
  documento_completo$formato ==
    "physikos-document-question"
)

stopifnot(
  documento_completo$version_formato == 1L
)

stopifnot(
  documento_completo$id ==
    "cinematica-mrua-001"
)

stopifnot(
  documento_completo$version_pregunta == 1
)

stopifnot(
  documento_completo$titulo ==
    "Movimiento uniformemente acelerado"
)

stopifnot(
  documento_completo$area == "fisica",
  documento_completo$tema == "cinematica",
  documento_completo$dificultad == "basica"
)

stopifnot(
  isTRUE(
    documento_completo$incluir_solucion
  )
)

stopifnot(
  identical(
    documento_completo$valores,
    valores
  )
)

stopifnot(
  grepl(
    "velocidad inicial de 3 m/s",
    documento_completo$pregunta$texto,
    fixed = TRUE
  )
)

stopifnot(
  grepl(
    "2.5 m/s^2",
    documento_completo$pregunta$texto,
    fixed = TRUE
  )
)

stopifnot(
  grepl(
    "durante 4 s",
    documento_completo$pregunta$texto,
    fixed = TRUE
  )
)

stopifnot(
  length(
    documento_completo$pregunta$incisos
  ) == 2
)

stopifnot(
  !grepl(
    "{{",
    documento_completo$pregunta$texto,
    fixed = TRUE
  )
)

stopifnot(
  !is.null(documento_completo$solucion)
)

stopifnot(
  length(
    documento_completo$solucion$pasos
  ) == 2
)

stopifnot(
  documento_completo$
    solucion$resultados$velocidad_final ==
    "13.00"
)

stopifnot(
  documento_completo$
    solucion$resultados$distancia ==
    "32.00"
)

stopifnot(
  documento_completo$pregunta$id ==
    documento_completo$solucion$id
)

stopifnot(
  documento_completo$id ==
    documento_completo$pregunta$id
)

ids_incisos <- vapply(
  documento_completo$pregunta$incisos,
  function(inciso) {
    inciso$id
  },
  character(1)
)

ids_pasos <- vapply(
  documento_completo$solucion$pasos,
  function(paso) {
    paso$inciso
  },
  character(1)
)

stopifnot(
  identical(
    ids_incisos,
    ids_pasos
  )
)

documento_examen <- construir_documento_pregunta(
  pregunta = pregunta,
  valores = valores,
  incluir_solucion = FALSE
)

stopifnot(
  identical(
    documento_examen$incluir_solucion,
    FALSE
  )
)

stopifnot(
  is.null(documento_examen$solucion)
)

stopifnot(
  grepl(
    "velocidad inicial de 3 m/s",
    documento_examen$pregunta$texto,
    fixed = TRUE
  )
)

stopifnot(
  length(
    documento_examen$pregunta$incisos
  ) == 2
)

stopifnot(
  identical(
    documento_completo$pregunta,
    documento_examen$pregunta
  )
)

stopifnot(
  identical(
    documento_completo$valores,
    documento_examen$valores
  )
)

error_opcion <- tryCatch(
  {
    construir_documento_pregunta(
      pregunta = pregunta,
      valores = valores,
      incluir_solucion = "sí"
    )

    NULL
  },
  error = function(error) {
    error
  }
)

stopifnot(
  inherits(error_opcion, "error")
)

stopifnot(
  grepl(
    "TRUE o FALSE",
    conditionMessage(error_opcion),
    fixed = TRUE
  )
)

error_na <- tryCatch(
  {
    construir_documento_pregunta(
      pregunta,
      valores,
      incluir_solucion = NA
    )

    NULL
  },
  error = function(error) {
    error
  }
)

stopifnot(
  inherits(error_na, "error")
)

error_valores <- tryCatch(
  {
    construir_documento_pregunta(
      pregunta = pregunta,
      valores = list(
        velocidad_inicial = 3,
        aceleracion = 2.5
      )
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

documento_json <- jsonlite::toJSON(
  documento_completo,
  auto_unbox = TRUE,
  pretty = TRUE,
  null = "null"
)

stopifnot(
  jsonlite::validate(
    documento_json
  )
)

cat(
  "\nDocumento JSON generado:\n",
  documento_json,
  "\n"
)

cat("\nPRUEBAS SUPERADAS\n")
cat("- Se construyó un documento con solución.\n")
cat("- Se construyó un examen sin solución.\n")
cat("- Ambos documentos conservaron los mismos valores.\n")
cat("- El enunciado y la solución mantuvieron sus identificadores.\n")
cat("- Los incisos coincidieron con los pasos de solución.\n")
cat("- Se rechazaron opciones y valores inválidos.\n")
cat("- El documento pudo serializarse como JSON.\n")