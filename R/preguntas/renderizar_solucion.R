# Este módulo usa auxiliares cargados previamente con source().
# nolint start: object_usage_linter.

formatear_resultado <- function(
  valor,
  decimales,
  nombre
) {
  if (!es_numero_escalar(valor)) {
    stop(
      paste0(
        "El resultado '",
        nombre,
        "' no contiene un número finito."
      ),
      call. = FALSE
    )
  }

  if (
    !is.numeric(decimales) ||
      length(decimales) != 1 ||
      is.na(decimales) ||
      !is.finite(decimales) ||
      decimales < 0 ||
      decimales != floor(decimales)
  ) {
    stop(
      paste0(
        "La cantidad de decimales del resultado '",
        nombre,
        "' no es válida."
      ),
      call. = FALSE
    )
  }

  formatC(
    valor,
    format = "f",
    digits = decimales,
    decimal.mark = "."
  )
}

construir_resultado_texto <- function(
  nombre,
  valor_formateado,
  unidad
) {
  partes <- c(
    paste0(nombre, " = ", valor_formateado)
  )

  if (
    es_texto_escalar(unidad) &&
      nzchar(trimws(unidad))
  ) {
    partes <- c(
      partes,
      trimws(unidad)
    )
  }

  paste(
    partes,
    collapse = " "
  )
}

construir_resultado_latex <- function(
  nombre,
  valor_formateado,
  unidad
) {
  nombre_latex <- escapar_latex(nombre)
  valor_latex <- escapar_latex(valor_formateado)

  if (
    es_texto_escalar(unidad) &&
      nzchar(trimws(unidad))
  ) {
    unidad_latex <- escapar_latex(
      trimws(unidad)
    )

    return(
      paste0(
        "\\(",
        nombre_latex,
        " = ",
        valor_latex,
        "\\;\\text{",
        unidad_latex,
        "}",
        "\\)"
      )
    )
  }

  paste0(
    "\\(",
    nombre_latex,
    " = ",
    valor_latex,
    "\\)"
  )
}

renderizar_paso_solucion <- function(
  paso_definido,
  paso_resuelto,
  indice
) {
  nombre_resultado <- paso_definido$guardar_como

  if (
    !identical(
      nombre_resultado,
      paso_resuelto$guardar_como
    )
  ) {
    stop(
      paste0(
        "El paso ",
        indice,
        " no coincide con la solución resuelta. ",
        "Se esperaba '",
        nombre_resultado,
        "' y se recibió '",
        paso_resuelto$guardar_como,
        "'."
      ),
      call. = FALSE
    )
  }

  valor <- paso_resuelto$valor
  valor_exacto <- paso_resuelto$valor_exacto
  decimales <- paso_definido$decimales
  unidad <- paso_definido$unidad

  valor_formateado <- formatear_resultado(
    valor = valor,
    decimales = decimales,
    nombre = nombre_resultado
  )

  resultado_texto <- construir_resultado_texto(
    nombre = nombre_resultado,
    valor_formateado = valor_formateado,
    unidad = unidad
  )

  resultado_latex <- construir_resultado_latex(
    nombre = nombre_resultado,
    valor_formateado = valor_formateado,
    unidad = unidad
  )

  list(
    indice = indice,
    inciso = paso_definido$inciso,
    explicacion_texto =
      paso_definido$explicacion,
    explicacion_latex = escapar_latex(
      paso_definido$explicacion
    ),
    formula_latex =
      paso_definido$formula_latex,
    resultado = nombre_resultado,
    valor = valor,
    valor_exacto = valor_exacto,
    valor_formateado = valor_formateado,
    unidad = unidad,
    resultado_texto = resultado_texto,
    resultado_latex = resultado_latex
  )
}

renderizar_solucion <- function(
  pregunta,
  solucion_resuelta
) {
  resultado_validacion <- validar_pregunta(
    pregunta
  )

  if (!resultado_validacion$valida) {
    stop(
      paste(
        c(
          "No se puede renderizar la solución:",
          resultado_validacion$errores
        ),
        collapse = "\n"
      ),
      call. = FALSE
    )
  }

  if (!is.list(solucion_resuelta)) {
    stop(
      "'solucion_resuelta' debe ser una lista.",
      call. = FALSE
    )
  }

  if (!is.list(solucion_resuelta$pasos)) {
    stop(
      paste(
        "'solucion_resuelta$pasos'",
        "debe ser una lista."
      ),
      call. = FALSE
    )
  }

  pasos_definidos <- pregunta$solucion$pasos
  pasos_resueltos <- solucion_resuelta$pasos

  if (
    length(pasos_definidos) !=
      length(pasos_resueltos)
  ) {
    stop(
      paste0(
        "La pregunta define ",
        length(pasos_definidos),
        " pasos, pero la solución contiene ",
        length(pasos_resueltos),
        "."
      ),
      call. = FALSE
    )
  }

  pasos_renderizados <- lapply(
    seq_along(pasos_definidos),
    function(indice) {
      renderizar_paso_solucion(
        paso_definido =
          pasos_definidos[[indice]],
        paso_resuelto =
          pasos_resueltos[[indice]],
        indice = indice
      )
    }
  )

  resultados_formateados <- lapply(
    pasos_renderizados,
    function(paso) {
      paso$valor_formateado
    }
  )

  names(resultados_formateados) <- vapply(
    pasos_renderizados,
    function(paso) {
      paso$resultado
    },
    character(1)
  )

  list(
    id = pregunta$id,
    titulo = pregunta$titulo,
    pasos = pasos_renderizados,
    resultados = resultados_formateados
  )
}

# nolint end