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

source(
  file.path(
    carpeta_proyecto,
    "R",
    "latex",
    "compilar_archivo_tex.R"
  ),
  encoding = "UTF-8"
)

compilado_examen_clasico <- compilar_archivo_tex(
  guardado_examen_clasico$ruta
)

compilado_solucion_clasica <- compilar_archivo_tex(
  guardado_solucion_clasica$ruta
)

compilado_examen_minimalista <- compilar_archivo_tex(
  guardado_examen_minimalista$ruta
)

compilado_solucion_minimalista <- compilar_archivo_tex(
  guardado_solucion_minimalista$ruta
)

compilaciones <- list(
  examen_clasico =
    compilado_examen_clasico,
  solucion_clasica =
    compilado_solucion_clasica,
  examen_minimalista =
    compilado_examen_minimalista,
  solucion_minimalista =
    compilado_solucion_minimalista
)

print(compilaciones)

cat("\nPDF GENERADOS\n")

for (nombre in names(compilaciones)) {
  cat(
    "-",
    nombre,
    ":",
    compilaciones[[nombre]]$ruta_pdf,
    "\n"
  )
}

rutas_pdf <- vapply(
  compilaciones,
  function(compilacion) {
    compilacion$ruta_pdf
  },
  character(1)
)

stopifnot(
  all(
    file.exists(rutas_pdf)
  )
)

tamanos_pdf <- file.info(
  rutas_pdf
)$size

stopifnot(
  all(tamanos_pdf > 0)
)

codigos <- vapply(
  compilaciones,
  function(compilacion) {
    compilacion$codigo_salida
  },
  integer(1)
)

stopifnot(
  identical(
    unname(codigos),
    rep(0L, 4)
  )
)

rutas_registro <- vapply(
  compilaciones,
  function(compilacion) {
    compilacion$ruta_registro
  },
  character(1)
)

stopifnot(
  all(
    file.exists(rutas_registro)
  )
)

stopifnot(
  all(
    file.info(rutas_registro)$size > 0
  )
)

es_archivo_pdf <- function(
  ruta
) {
  conexion <- file(
    ruta,
    open = "rb"
  )

  on.exit(
    close(conexion),
    add = TRUE
  )

  firma <- readBin(
    conexion,
    what = "raw",
    n = 5
  )

  identical(
    rawToChar(firma),
    "%PDF-"
  )
}

pdf_validos <- vapply(
  rutas_pdf,
  es_archivo_pdf,
  logical(1)
)

stopifnot(
  all(pdf_validos)
)

md5_examenes <- tools::md5sum(
  c(
    compilado_examen_clasico$ruta_pdf,
    compilado_examen_minimalista$ruta_pdf
  )
)

stopifnot(
  md5_examenes[[1]] !=
    md5_examenes[[2]]
)

md5_soluciones <- tools::md5sum(
  c(
    compilado_solucion_clasica$ruta_pdf,
    compilado_solucion_minimalista$ruta_pdf
  )
)

stopifnot(
  md5_soluciones[[1]] !=
    md5_soluciones[[2]]
)

duraciones <- vapply(
  compilaciones,
  function(compilacion) {
    compilacion$duracion_segundos
  },
  numeric(1)
)

stopifnot(
  all(
    is.finite(duraciones)
  )
)

stopifnot(
  all(
    duraciones >= 0
  )
)

cat("\nTIEMPOS DE COMPILACIÓN\n")

for (nombre in names(duraciones)) {
  cat(
    "-",
    nombre,
    ":",
    round(
      duraciones[[nombre]],
      3
    ),
    "segundos\n"
  )
}

tex_invalido <- examen_clasico

tex_invalido$nombre_archivo <-
  "documento_invalido.tex"

tex_invalido$contenido <- paste(
  c(
    "\\documentclass{article}",
    "\\begin{document}",
    "\\comandoPhysikosInexistente",
    "\\end{document}"
  ),
  collapse = "\n"
)

directorio_invalido <- file.path(
  raiz_temporal,
  "invalido"
)

guardado_invalido <- guardar_archivo_tex(
  archivo_tex = tex_invalido,
  directorio_salida = directorio_invalido
)

error_compilacion <- tryCatch(
  {
    compilar_archivo_tex(
      guardado_invalido$ruta
    )

    NULL
  },
  error = function(error) {
    error
  }
)

stopifnot(
  inherits(
    error_compilacion,
    "error"
  )
)

stopifnot(
  grepl(
    "compilación LaTeX falló",
    conditionMessage(error_compilacion),
    fixed = TRUE
  )
)

ruta_registro_invalido <- file.path(
  directorio_invalido,
  "documento_invalido_compilacion.log"
)

stopifnot(
  file.exists(
    ruta_registro_invalido
  )
)

stopifnot(
  file.info(
    ruta_registro_invalido
  )$size > 0
)

stopifnot(
  !file.exists(
    file.path(
      directorio_invalido,
      "documento_invalido.pdf"
    )
  )
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

cat("\nPRUEBAS SUPERADAS\n")
cat("- Se compilaron cuatro documentos TEX.\n")
cat("- Todos los motores devolvieron código cero.\n")
cat("- Todos los PDF tienen una firma válida.\n")
cat("- Se capturaron los registros de compilación.\n")
cat("- Se registraron los tiempos de compilación.\n")
cat("- Las plantillas generaron PDF diferentes.\n")
cat("- Una compilación inválida fue rechazada.\n")
cat("- Los archivos temporales serán eliminados.\n")