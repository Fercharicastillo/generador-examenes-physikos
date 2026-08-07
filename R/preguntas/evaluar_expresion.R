# Este mÃ³dulo usa auxiliares cargados previamente con source().
# nolint start: object_usage_linter.

evaluar_expresion <- function(
  expresion,
  contexto = list(),
  ruta = "expresion",
  tolerancia = 1e-9
) {
  if (!is.list(contexto)) {
    stop(
      "'contexto' debe ser una lista.",
      call. = FALSE
    )
  }

  if (length(contexto) > 0) {
    nombres_contexto <- names(contexto)

    if (
      is.null(nombres_contexto) ||
        any(!nzchar(nombres_contexto)) ||
        anyDuplicated(nombres_contexto)
    ) {
      stop(
        paste(
          "'contexto' debe tener nombres",
          "únicos y no vacíos."
        ),
        call. = FALSE
      )
    }

    valores_validos <- vapply(
      contexto,
      es_numero_escalar,
      logical(1)
    )

    if (!all(valores_validos)) {
      nombres_invalidos <- nombres_contexto[
        !valores_validos
      ]

      stop(
        paste0(
          "El contexto contiene valores no numéricos ",
          "o no finitos: ",
          paste(
            nombres_invalidos,
            collapse = ", "
          ),
          "."
        ),
        call. = FALSE
      )
    }
  }

  errores <- validar_expresion(
    expresion = expresion,
    referencias_permitidas = names(contexto),
    ruta = ruta
  )

  if (length(errores) > 0) {
    stop(
      paste(
        errores,
        collapse = "\n"
      ),
      call. = FALSE
    )
  }

  evaluar_nodo <- function(nodo,
                           ruta_nodo) {
    if (es_numero_escalar(nodo)) {
      return(as.numeric(nodo))
    }

    if (es_texto_escalar(nodo)) {
      if (!nodo %in% names(contexto)) {
        stop(
          paste0(
            ruta_nodo,
            ": la referencia '",
            nodo,
            "' no existe."
          ),
          call. = FALSE
        )
      }

      valor <- contexto[[nodo]]

      if (!es_numero_escalar(valor)) {
        stop(
          paste0(
            ruta_nodo,
            ": la referencia '",
            nodo,
            "' no contiene un número finito."
          ),
          call. = FALSE
        )
      }

      return(as.numeric(valor))
    }

    operacion <- nodo$operacion
    argumentos <- nodo$argumentos

    valores <- lapply(
      seq_along(argumentos),
      function(indice) {
        evaluar_nodo(
          argumentos[[indice]],
          paste0(
            ruta_nodo,
            ".argumentos[",
            indice,
            "]"
          )
        )
      }
    )

    valores_numericos <- vapply(
      valores,
      es_numero_escalar,
      logical(1)
    )

    if (!all(valores_numericos)) {
      stop(
        paste0(
          ruta_nodo,
          ": la operación '",
          operacion,
          "' requiere argumentos numéricos."
        ),
        call. = FALSE
      )
    }

    resultado <- switch(operacion,
      sumar = {
        sum(
          unlist(
            valores,
            use.names = FALSE
          )
        )
      },
      restar = {
        valores[[1]] - valores[[2]]
      },
      multiplicar = {
        prod(
          unlist(
            valores,
            use.names = FALSE
          )
        )
      },
      dividir = {
        denominador <- valores[[2]]

        if (abs(denominador) <= tolerancia) {
          stop(
            paste0(
              ruta_nodo,
              ": división por cero o por un valor ",
              "demasiado cercano a cero."
            ),
            call. = FALSE
          )
        }

        valores[[1]] / denominador
      },
      potencia = {
        suppressWarnings(
          valores[[1]]^valores[[2]]
        )
      },
      menor = {
        valores[[1]] < valores[[2]]
      },
      menor_o_igual = {
        valores[[1]] <= valores[[2]]
      },
      mayor = {
        valores[[1]] > valores[[2]]
      },
      mayor_o_igual = {
        valores[[1]] >= valores[[2]]
      },
      igual = {
        abs(
          valores[[1]] - valores[[2]]
        ) <= tolerancia
      },
      stop(
        paste0(
          ruta_nodo,
          ": operación no implementada: ",
          operacion,
          "."
        ),
        call. = FALSE
      )
    )

    es_comparacion <- operacion %in% c(
      "menor",
      "menor_o_igual",
      "mayor",
      "mayor_o_igual",
      "igual"
    )

    if (es_comparacion) {
      if (
        !is.logical(resultado) ||
          length(resultado) != 1 ||
          is.na(resultado)
      ) {
        stop(
          paste0(
            ruta_nodo,
            ": la comparación no produjo ",
            "un resultado lógico válido."
          ),
          call. = FALSE
        )
      }

      return(resultado)
    }

    if (!es_numero_escalar(resultado)) {
      stop(
        paste0(
          ruta_nodo,
          ": la operación '",
          operacion,
          "' produjo un resultado no finito."
        ),
        call. = FALSE
      )
    }

    as.numeric(resultado)
  }

  evaluar_nodo(
    expresion,
    ruta
  )
}

# nolint end
