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

pregunta <- jsonlite::read_json(
  file.path(
    carpeta_proyecto,
    "preguntas",
    "cinematica",
    "mrua-001.json"
  ),
  simplifyVector = FALSE
)

valores <- list(
  velocidad_inicial = 3,
  aceleracion = 2.5,
  tiempo = 4
)

documento <- construir_documento_pregunta(
  pregunta = pregunta,
  valores = valores,
  incluir_solucion = TRUE
)

fragmentos <- renderizar_documento_latex(
  documento
)

examen_clasico <- construir_archivo_tex(
  fragmentos = fragmentos,
  plantilla = "clasica",
  tipo = "examen",
  carpeta_plantillas = carpeta_plantillas
)

solucion_clasica <- construir_archivo_tex(
  fragmentos = fragmentos,
  plantilla = "clasica",
  tipo = "solucion",
  carpeta_plantillas = carpeta_plantillas
)

examen_minimalista <- construir_archivo_tex(
  fragmentos = fragmentos,
  plantilla = "minimalista",
  tipo = "examen",
  carpeta_plantillas = carpeta_plantillas
)

solucion_minimalista <- construir_archivo_tex(
  fragmentos = fragmentos,
  plantilla = "minimalista",
  tipo = "solucion",
  carpeta_plantillas = carpeta_plantillas
)

raiz_temporal <- tempfile(
  pattern = "physikos_tex_"
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

guardado_examen_clasico <- guardar_archivo_tex(
  archivo_tex = examen_clasico,
  directorio_salida = directorio_clasico
)

guardado_solucion_clasica <- guardar_archivo_tex(
  archivo_tex = solucion_clasica,
  directorio_salida = directorio_clasico
)

guardado_examen_minimalista <- guardar_archivo_tex(
  archivo_tex = examen_minimalista,
  directorio_salida = directorio_minimalista
)

guardado_solucion_minimalista <- guardar_archivo_tex(
  archivo_tex = solucion_minimalista,
  directorio_salida = directorio_minimalista
)

cat(
  "\nDIRECTORIO TEMPORAL\n",
  normalizePath(
    raiz_temporal,
    winslash = "/"
  ),
  "\n"
)

print(
  list.files(
    raiz_temporal,
    recursive = TRUE,
    full.names = TRUE
  )
)

rutas_guardadas <- c(
  guardado_examen_clasico$ruta,
  guardado_solucion_clasica$ruta,
  guardado_examen_minimalista$ruta,
  guardado_solucion_minimalista$ruta
)

stopifnot(
  all(
    file.exists(rutas_guardadas)
  )
)

tamanos <- file.info(
  rutas_guardadas
)$size

stopifnot(
  all(tamanos > 0)
)

leer_archivo_binario <- function(
  ruta
) {
  cantidad <- file.info(ruta)$size

  conexion <- file(
    ruta,
    open = "rb"
  )

  on.exit(
    close(conexion),
    add = TRUE
  )

  contenido_raw <- readBin(
    conexion,
    what = "raw",
    n = cantidad
  )

  rawToChar(
    contenido_raw
  )
}

contenido_leido <- leer_archivo_binario(
  guardado_examen_clasico$ruta
)

stopifnot(
  identical(
    contenido_leido,
    examen_clasico$contenido
  )
)

stopifnot(
  dirname(
    guardado_examen_clasico$ruta
  ) == normalizePath(
    directorio_clasico,
    winslash = "/"
  )
)

stopifnot(
  dirname(
    guardado_examen_minimalista$ruta
  ) == normalizePath(
    directorio_minimalista,
    winslash = "/"
  )
)

stopifnot(
  !identical(
    guardado_examen_clasico$ruta,
    guardado_examen_minimalista$ruta
  )
)

contenido_clasico <- leer_archivo_binario(
  guardado_examen_clasico$ruta
)

contenido_minimalista <- leer_archivo_binario(
  guardado_examen_minimalista$ruta
)

stopifnot(
  !identical(
    contenido_clasico,
    contenido_minimalista
  )
)

stopifnot(
  grepl(
    "physikosblue",
    contenido_clasico,
    fixed = TRUE
  )
)

stopifnot(
  !grepl(
    "physikosblue",
    contenido_minimalista,
    fixed = TRUE
  )
)

error_colision <- tryCatch(
  {
    guardar_archivo_tex(
      archivo_tex = examen_clasico,
      directorio_salida = directorio_clasico
    )

    NULL
  },
  error = function(error) {
    error
  }
)

stopifnot(
  inherits(error_colision, "error")
)

stopifnot(
  grepl(
    "ya existe",
    conditionMessage(error_colision),
    fixed = TRUE
  )
)

archivo_peligroso <- examen_clasico

archivo_peligroso$nombre_archivo <-
  "../fuera_del_directorio.tex"

error_nombre <- tryCatch(
  {
    guardar_archivo_tex(
      archivo_tex = archivo_peligroso,
      directorio_salida = directorio_clasico
    )

    NULL
  },
  error = function(error) {
    error
  }
)

stopifnot(
  inherits(error_nombre, "error")
)

stopifnot(
  grepl(
    "no debe contener rutas",
    conditionMessage(error_nombre),
    fixed = TRUE
  )
)

stopifnot(
  !file.exists(
    file.path(
      raiz_temporal,
      "fuera_del_directorio.tex"
    )
  )
)

cat("\nPRUEBAS SUPERADAS\n")
cat("- Se guardaron cuatro archivos TEX.\n")
cat("- Cada plantilla quedó aislada en su directorio.\n")
cat("- El contenido guardado coincide con el construido.\n")
cat("- Los documentos clásicos y minimalistas son diferentes.\n")
cat("- No se permitió sobrescribir un archivo existente.\n")
cat("- Se rechazaron nombres que intentan salir del directorio.\n")
cat("- Los archivos temporales se eliminarán al finalizar.\n")