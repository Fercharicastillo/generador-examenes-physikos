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
    "trabajos",
    "ejecutar_trabajo.R"
  ),
  encoding = "UTF-8"
)
crear_y_ejecutar_prueba <- function(
    estudiante,
    semilla
) {
  solicitud <- list(
    plantilla = 1,
    estudiantes = estudiante,
    incluir_soluciones = TRUE,
    semilla = semilla
  )

  trabajo <- crear_trabajo(
    carpeta_trabajos = file.path(
      carpeta_proyecto,
      "trabajos"
    ),
    solicitud = solicitud
  )

  cat(
    "\nTrabajo creado:",
    trabajo$trabajo_id,
    "\n"
  )

  ejecutar_trabajo(
    directorio_trabajo = trabajo$directorio,
    carpeta_proyecto = carpeta_proyecto
  )

  estado <- leer_estado_trabajo(
    trabajo$directorio
  )

  print(estado)

  list(
    trabajo = trabajo,
    estado = estado
  )
}

resultado_1 <- crear_y_ejecutar_prueba(
  estudiante = "Estudiante uno",
  semilla = 20260805
)

resultado_2 <- crear_y_ejecutar_prueba(
  estudiante = "Estudiante dos",
  semilla = 20260806
)

stopifnot(
  resultado_1$trabajo$trabajo_id !=
    resultado_2$trabajo$trabajo_id
)

stopifnot(
  resultado_1$estado$estado == "completado",
  resultado_2$estado$estado == "completado"
)

cat(
  "\nPRUEBA SUPERADA:",
  "los trabajos son independientes.\n"
)