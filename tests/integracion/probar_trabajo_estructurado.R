carpeta_proyecto <- normalizePath(
  ".",
  winslash = "/",
  mustWork = TRUE
)

source(
  file.path(
    carpeta_proyecto,
    "R",
    "trabajos",
    "estado_trabajo.R"
  ),
  encoding = "UTF-8"
)

source(
  file.path(
    carpeta_proyecto,
    "R",
    "trabajos",
    "crear_trabajo.R"
  ),
  encoding = "UTF-8"
)

source(
  file.path(
    carpeta_proyecto,
    "R",
    "generar_examenes.R"
  ),
  encoding = "UTF-8"
)

source(
  file.path(
    carpeta_proyecto,
    "R",
    "cargar_motor_estructurado.R"
  ),
  encoding = "UTF-8"
)

cargar_motor_estructurado(
  carpeta_proyecto
)

source(
  file.path(
    carpeta_proyecto,
    "R",
    "trabajos",
    "ejecutar_trabajo.R"
  ),
  encoding = "UTF-8"
)

carpeta_trabajos <- tempfile(
  pattern = "physikos_trabajos_"
)

dir.create(
  carpeta_trabajos,
  recursive = TRUE,
  showWarnings = FALSE
)

on.exit(
  {
    if (dir.exists(carpeta_trabajos)) {
      unlink(
        carpeta_trabajos,
        recursive = TRUE,
        force = TRUE
      )
    }
  },
  add = TRUE
)

solicitud <- list(
  motor = "estructurado",
  plantilla_latex = "clasica",
  preguntas = c(
    "cinematica-mrua-001"
  ),
  estudiantes = c(
    "Estudiante Uno",
    "Estudiante Dos"
  ),
  incluir_soluciones = TRUE,
  semilla = 20260806
)

trabajo <- crear_trabajo(
  carpeta_trabajos =
    carpeta_trabajos,
  solicitud = solicitud
)

ejecutar_trabajo(
  directorio_trabajo =
    trabajo$directorio,
  carpeta_proyecto =
    carpeta_proyecto
)

estado <- leer_estado_trabajo(
  trabajo$directorio
)

print(estado)

stopifnot(
  estado$estado == "completado"
)

stopifnot(
  estado$progreso == 100L
)

stopifnot(
  estado$actual == 2L
)

stopifnot(
  estado$total == 2L
)

carpeta_resultados <- file.path(
  trabajo$directorio,
  "resultados"
)

examenes <- list.files(
  file.path(
    carpeta_resultados,
    "examenes"
  ),
  pattern = "\\.pdf$",
  full.names = TRUE
)

soluciones <- list.files(
  file.path(
    carpeta_resultados,
    "soluciones"
  ),
  pattern = "\\.pdf$",
  full.names = TRUE
)

stopifnot(
  length(examenes) == 2
)

stopifnot(
  length(soluciones) == 2
)

stopifnot(
  all(file.info(examenes)$size > 0)
)

stopifnot(
  all(file.info(soluciones)$size > 0)
)

archivo_zip <- file.path(
  carpeta_resultados,
  paste0(
    trabajo$trabajo_id,
    ".zip"
  )
)

stopifnot(
  file.exists(archivo_zip)
)

stopifnot(
  file.info(archivo_zip)$size > 0
)

cat("\nPRUEBAS SUPERADAS\n")
cat("- Se creó un trabajo estructurado.\n")
cat("- Se generaron dos evaluaciones.\n")
cat("- Se generaron dos solucionarios.\n")
cat("- El progreso llegó al 100%.\n")
cat("- Se creó el archivo ZIP.\n")