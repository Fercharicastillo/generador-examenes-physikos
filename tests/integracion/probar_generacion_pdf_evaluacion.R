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

source(
  file.path(
    carpeta_proyecto,
    "R",
    "latex",
    "guardar_archivo_tex.R"
  ),
  encoding = "UTF-8"
)

source(
  file.path(
    carpeta_proyecto,
    "R",
    "latex",
    "compilar_archivo_tex.R"
  ),
  encoding = "UTF-8"
)

source(
  file.path(
    carpeta_proyecto,
    "R",
    "evaluaciones",
    "generar_pdf_evaluacion.R"
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

raiz_temporal <- tempfile(
  pattern = "physikos_evaluacion_"
)

creado <- dir.create(
  raiz_temporal,
  recursive = TRUE,
  showWarnings = FALSE
)

stopifnot(
  creado,
  dir.exists(raiz_temporal)
)

on.exit(
  {
    if (dir.exists(raiz_temporal)) {
      unlink(
        raiz_temporal,
        recursive = TRUE,
        force = TRUE
      )
    }
  },
  add = TRUE
)

directorio_clasico <- file.path(
  raiz_temporal,
  "clasica"
)

directorio_minimalista <- file.path(
  raiz_temporal,
  "minimalista"
)

resultado_clasico <- generar_pdf_evaluacion(
  evaluacion = evaluacion,
  plantilla = "clasica",
  directorio_salida =
    directorio_clasico,
  carpeta_plantillas =
    carpeta_plantillas
)

print(resultado_clasico)

stopifnot(
  resultado_clasico$formato ==
    "physikos-generated-assessment"
)

stopifnot(
  resultado_clasico$plantilla ==
    "clasica"
)

stopifnot(
  resultado_clasico$total_preguntas ==
    2L
)

stopifnot(
  file.exists(
    resultado_clasico$examen$ruta_pdf
  )
)

stopifnot(
  file.exists(
    resultado_clasico$solucion$ruta_pdf
  )
)

stopifnot(
  resultado_clasico$examen$bytes > 0
)

stopifnot(
  resultado_clasico$solucion$bytes > 0
)

stopifnot(
  resultado_clasico$examen$
    codigo_salida == 0L
)

stopifnot(
  resultado_clasico$solucion$
    codigo_salida == 0L
)

stopifnot(
  file.exists(
    resultado_clasico$examen$
      ruta_registro
  )
)

stopifnot(
  file.exists(
    resultado_clasico$solucion$
      ruta_registro
  )
)

stopifnot(
  basename(
    dirname(
      resultado_clasico$examen$
        ruta_pdf
    )
  ) == "examen"
)

stopifnot(
  basename(
    dirname(
      resultado_clasico$solucion$
        ruta_pdf
    )
  ) == "solucion"
)

resultado_minimalista <-
  generar_pdf_evaluacion(
    evaluacion = evaluacion,
    plantilla = "minimalista",
    directorio_salida =
    directorio_minimalista,
    carpeta_plantillas =
    carpeta_plantillas
  )

stopifnot(
  file.exists(
    resultado_minimalista$examen$
      ruta_pdf
  )
)

stopifnot(
  file.exists(
    resultado_minimalista$solucion$
      ruta_pdf
  )
)

md5_examenes <- tools::md5sum(
  c(
    resultado_clasico$examen$ruta_pdf,
    resultado_minimalista$examen$ruta_pdf
  )
)

stopifnot(
  md5_examenes[[1]] !=
    md5_examenes[[2]]
)

md5_soluciones <- tools::md5sum(
  c(
    resultado_clasico$solucion$ruta_pdf,
    resultado_minimalista$solucion$ruta_pdf
  )
)

stopifnot(
  md5_soluciones[[1]] !=
    md5_soluciones[[2]]
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

directorio_sin_solucion <- file.path(
  raiz_temporal,
  "sin_solucion"
)

resultado_sin_solucion <-
  generar_pdf_evaluacion(
    evaluacion = evaluacion_sin_solucion,
    plantilla = "minimalista",
    directorio_salida =
    directorio_sin_solucion,
    carpeta_plantillas =
    carpeta_plantillas
  )

stopifnot(
  file.exists(
    resultado_sin_solucion$examen$
      ruta_pdf
  )
)

stopifnot(
  is.null(
    resultado_sin_solucion$solucion
  )
)

stopifnot(
  !dir.exists(
    file.path(
      directorio_sin_solucion,
      "solucion"
    )
  )
)

error_directorio <- tryCatch(
  {
    generar_pdf_evaluacion(
      evaluacion = evaluacion,
      plantilla = "clasica",
      directorio_salida =
        directorio_clasico,
      carpeta_plantillas =
        carpeta_plantillas
    )

    NULL
  },
  error = function(error) {
    error
  }
)

stopifnot(
  inherits(
    error_directorio,
    "error"
  )
)

stopifnot(
  grepl(
    "no está vacío",
    conditionMessage(error_directorio),
    fixed = TRUE
  )
)

stopifnot(
  is.numeric(
    resultado_clasico$
      duracion_total_segundos
  )
)

stopifnot(
  is.finite(
    resultado_clasico$
      duracion_total_segundos
  )
)

stopifnot(
  resultado_clasico$
    duracion_total_segundos > 0
)

cat(
  "\nDuración clásica:",
  round(
    resultado_clasico$
      duracion_total_segundos,
    3
  ),
  "segundos\n"
)

cat(
  "Duración minimalista:",
  round(
    resultado_minimalista$
      duracion_total_segundos,
    3
  ),
  "segundos\n"
)

cat("\nPRUEBAS SUPERADAS\n")
cat("- Se generó una evaluación clásica con dos preguntas.\n")
cat("- Se generó una evaluación minimalista con dos preguntas.\n")
cat("- Cada evaluación produjo un solo examen PDF.\n")
cat("- Cada evaluación produjo un solo solucionario PDF.\n")
cat("- Los PDF y registros existen y tienen contenido.\n")
cat("- Las plantillas produjeron resultados diferentes.\n")
cat("- Se generó correctamente una evaluación sin solución.\n")
cat("- Se rechazó la reutilización de un directorio.\n")
cat("- Se registró la duración total del proceso.\n")