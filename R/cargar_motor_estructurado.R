cargar_motor_estructurado <- function(
  carpeta_proyecto
) {
  archivos <- c(
    "R/preguntas/validar_expresion.R",
    "R/preguntas/validar_pregunta.R",
    "R/preguntas/evaluar_expresion.R",
    "R/preguntas/generar_variables.R",
    "R/preguntas/resolver_pregunta.R",
    "R/preguntas/renderizar_enunciado.R",
    "R/preguntas/renderizar_solucion.R",
    "R/preguntas/construir_documento_pregunta.R",
    "R/preguntas/cargar_preguntas.R",
    "R/evaluaciones/construir_evaluacion.R",
    "R/latex/renderizar_documento_latex.R",
    "R/latex/renderizar_evaluacion_latex.R",
    "R/latex/construir_archivo_tex.R",
    "R/latex/guardar_archivo_tex.R",
    "R/latex/compilar_archivo_tex.R",
    "R/evaluaciones/generar_pdf_evaluacion.R",
    "R/evaluaciones/generar_examenes_estructurados.R"
  )

  for (archivo in archivos) {
    ruta <- file.path(
      carpeta_proyecto,
      archivo
    )

    if (!file.exists(ruta)) {
      stop(
        paste0(
          "No existe el módulo requerido: ",
          ruta,
          "."
        ),
        call. = FALSE
      )
    }

    source(
      ruta,
      encoding = "UTF-8",
      local = .GlobalEnv
    )
  }

  invisible(TRUE)
}