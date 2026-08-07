generar_valor_variable <- function(variable, nombre) {
  tipo <- variable$tipo

  if (identical(tipo, "entero")) {
    posibilidades <- seq.int(
      from = variable$minimo,
      to = variable$maximo
    )

    return(sample(posibilidades, size = 1))
  }

  if (identical(tipo, "decimal")) {
    decimales <- variable$decimales
    factor <- 10^decimales

    minimo_entero <- as.integer(
      round(variable$minimo * factor)
    )

    maximo_entero <- as.integer(
      round(variable$maximo * factor)
    )

    valor_entero <- sample(
      seq.int(minimo_entero, maximo_entero),
      size = 1
    )

    return(valor_entero / factor)
  }

  stop(
    paste0(
      "La variable '",
      nombre,
      "' tiene un tipo no soportado: ",
      tipo,
      "."
    ),
    call. = FALSE
  )
}

generar_combinacion <- function(variables) {
  valores <- lapply(
    names(variables),
    function(nombre) {
      generar_valor_variable(
        variable = variables[[nombre]],
        nombre = nombre
      )
    }
  )

  names(valores) <- names(variables)

  valores
}

cumple_restricciones <- function(
  restricciones,
  contexto
) {
  if (length(restricciones) == 0) {
    return(TRUE)
  }

  resultados <- vapply(
    seq_along(restricciones),
    function(indice) {
      restriccion <- restricciones[[indice]]

      resultado <- evaluar_expresion(
        expresion = restriccion$expresion,
        contexto = contexto,
        ruta = paste0(
          "restricciones[",
          indice,
          "].expresion"
        )
      )

      if (
        !is.logical(resultado) ||
          length(resultado) != 1 ||
          is.na(resultado)
      ) {
        stop(
          paste0(
            "La restricción ",
            indice,
            " no produjo un valor lógico."
          ),
          call. = FALSE
        )
      }

      resultado
    },
    logical(1)
  )

  all(resultados)
}

generar_variables <- function(
  pregunta,
  max_intentos = 1000
) {
  resultado_validacion <- validar_pregunta(pregunta)

  if (!resultado_validacion$valida) {
    stop(
      paste(
        c(
          "No se pueden generar variables:",
          resultado_validacion$errores
        ),
        collapse = "\n"
      ),
      call. = FALSE
    )
  }

  if (
    !is.numeric(max_intentos) ||
      length(max_intentos) != 1 ||
      is.na(max_intentos) ||
      !is.finite(max_intentos) ||
      max_intentos < 1 ||
      max_intentos != floor(max_intentos)
  ) {
    stop(
      "'max_intentos' debe ser un entero positivo.",
      call. = FALSE
    )
  }

  for (intento in seq_len(max_intentos)) {
    valores <- generar_combinacion(
      pregunta$variables
    )

    restricciones_cumplidas <- cumple_restricciones(
      restricciones = pregunta$restricciones,
      contexto = valores
    )

    if (restricciones_cumplidas) {
      return(list(
        valores = valores,
        intentos = intento
      ))
    }
  }

  stop(
    paste0(
      "No fue posible generar una combinación válida ",
      "después de ",
      max_intentos,
      " intentos. Revisa las restricciones de la pregunta."
    ),
    call. = FALSE
  )
}