# Este módulo usa auxiliares cargados previamente con source().
# nolint start: object_usage_linter.

validar_valores_generados <- function(
  pregunta,
  valores
) {
  if (!is.list(valores)) {
    stop(
      "'valores' debe ser una lista.",
      call. = FALSE
    )
  }

  nombres_esperados <- names(
    pregunta$variables
  )

  nombres_recibidos <- names(valores)

  if (
    is.null(nombres_recibidos) ||
      any(!nzchar(nombres_recibidos)) ||
      anyDuplicated(nombres_recibidos)
  ) {
    stop(
      paste(
        "'valores' debe contener nombres",
        "únicos y no vacíos."
      ),
      call. = FALSE
    )
  }

  variables_faltantes <- setdiff(
    nombres_esperados,
    nombres_recibidos
  )

  if (length(variables_faltantes) > 0) {
    stop(
      paste0(
        "Faltan valores para las variables: ",
        paste(
          variables_faltantes,
          collapse = ", "
        ),
        "."
      ),
      call. = FALSE
    )
  }

  variables_adicionales <- setdiff(
    nombres_recibidos,
    nombres_esperados
  )

  if (length(variables_adicionales) > 0) {
    stop(
      paste0(
        "Se recibieron variables desconocidas: ",
        paste(
          variables_adicionales,
          collapse = ", "
        ),
        "."
      ),
      call. = FALSE
    )
  }

  valores_validos <- vapply(
    valores,
    es_numero_escalar,
    logical(1)
  )

  if (!all(valores_validos)) {
    nombres_invalidos <- nombres_recibidos[
      !valores_validos
    ]

    stop(
      paste0(
        "Estas variables no contienen números finitos: ",
        paste(
          nombres_invalidos,
          collapse = ", "
        ),
        "."
      ),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

resolver_pregunta <- function(
  pregunta,
  valores
) {
  resultado_validacion <- validar_pregunta(
    pregunta
  )

  if (!resultado_validacion$valida) {
    stop(
      paste(
        c(
          "No se puede resolver la pregunta:",
          resultado_validacion$errores
        ),
        collapse = "\n"
      ),
      call. = FALSE
    )
  }

  validar_valores_generados(
    pregunta = pregunta,
    valores = valores
  )

  contexto <- valores
  resultados <- list()
  resultados_exactos <- list()
  pasos_resueltos <- vector(
    mode = "list",
    length = length(pregunta$solucion$pasos)
  )

  for (
    indice in seq_along(
      pregunta$solucion$pasos
    )
  ) {
    paso <- pregunta$solucion$pasos[[indice]]
    nombre_resultado <- paso$guardar_como

    valor_exacto <- evaluar_expresion(
      expresion = paso$calculo,
      contexto = contexto,
      ruta = paste0(
        "solucion.pasos[",
        indice,
        "].calculo"
      )
    )

    if (!es_numero_escalar(valor_exacto)) {
      stop(
        paste0(
          "El cálculo del paso ",
          indice,
          " no produjo un número finito."
        ),
        call. = FALSE
      )
    }

    decimales <- paso$decimales
    valor_mostrado <- round(
      valor_exacto,
      digits = decimales
    )

    resultados_exactos[[nombre_resultado]] <-
      valor_exacto

    resultados[[nombre_resultado]] <-
      valor_mostrado

    contexto[[nombre_resultado]] <-
      valor_exacto

    pasos_resueltos[[indice]] <- list(
      indice = indice,
      inciso = paso$inciso,
      explicacion = paso$explicacion,
      formula_latex = paso$formula_latex,
      guardar_como = nombre_resultado,
      valor = valor_mostrado,
      valor_exacto = valor_exacto,
      unidad = paso$unidad,
      decimales = decimales
    )
  }

  list(
    variables = valores,
    resultados = resultados,
    resultados_exactos = resultados_exactos,
    pasos = pasos_resueltos,
    contexto = contexto
  )
}

# nolint end
