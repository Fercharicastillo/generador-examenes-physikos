# Este módulo usa funciones cargadas previamente con source().
# nolint start: object_usage_linter.

validar_incluir_solucion <- function(
  incluir_solucion
) {
  if (
    !is.logical(incluir_solucion) ||
      length(incluir_solucion) != 1 ||
      is.na(incluir_solucion)
  ) {
    stop(
      paste(
        "'incluir_solucion' debe ser",
        "TRUE o FALSE."
      ),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

construir_documento_pregunta <- function(
  pregunta,
  valores,
  incluir_solucion = TRUE
) {
  validar_incluir_solucion(
    incluir_solucion
  )

  pregunta_renderizada <- renderizar_enunciado(
    pregunta = pregunta,
    valores = valores
  )

  solucion_renderizada <- NULL

  if (incluir_solucion) {
    solucion_resuelta <- resolver_pregunta(
      pregunta = pregunta,
      valores = valores
    )

    solucion_renderizada <- renderizar_solucion(
      pregunta = pregunta,
      solucion_resuelta = solucion_resuelta
    )
  }

  list(
    formato = "physikos-document-question",
    version_formato = 1L,
    id = pregunta$id,
    version_pregunta = pregunta$version_pregunta,
    titulo = pregunta$titulo,
    area = pregunta$area,
    tema = pregunta$tema,
    dificultad = pregunta$dificultad,
    incluir_solucion = incluir_solucion,
    valores = valores,
    pregunta = pregunta_renderizada,
    solucion = solucion_renderizada
  )
}

# nolint end