operaciones_permitidas <- list(
  sumar = c(minimo = 2, maximo = Inf),
  restar = c(minimo = 2, maximo = 2),
  multiplicar = c(minimo = 2, maximo = Inf),
  dividir = c(minimo = 2, maximo = 2),
  potencia = c(minimo = 2, maximo = 2),
  menor = c(minimo = 2, maximo = 2),
  menor_o_igual = c(minimo = 2, maximo = 2),
  mayor = c(minimo = 2, maximo = 2),
  mayor_o_igual = c(minimo = 2, maximo = 2),
  igual = c(minimo = 2, maximo = 2)
)

es_texto_escalar <- function(valor) {
  is.character(valor) &&
    length(valor) == 1 &&
    !is.na(valor) &&
    nzchar(trimws(valor))
}

es_numero_escalar <- function(valor) {
  is.numeric(valor) &&
    length(valor) == 1 &&
    !is.na(valor) &&
    is.finite(valor)
}

es_entero <- function(valor) {
  es_numero_escalar(valor) &&
    valor == floor(valor)
}

validar_expresion <- function(
  expresion,
  referencias_permitidas,
  ruta = "expresion"
) {
  errores <- character()

  # Un número fijo es un operando válido.
  if (es_numero_escalar(expresion)) {
    return(errores)
  }

  # Un texto representa el nombre de una variable
  # o de un resultado calculado anteriormente.
  if (es_texto_escalar(expresion)) {
    if (!expresion %in% referencias_permitidas) {
      errores <- c(
        errores,
        paste0(
          ruta,
          ": la referencia '",
          expresion,
          "' no existe."
        )
      )
    }

    return(errores)
  }

  if (!is.list(expresion)) {
    return(c(
      errores,
      paste0(
        ruta,
        ": debe ser un número, una referencia ",
        "o una operación estructurada."
      )
    ))
  }

  operacion <- expresion$operacion
  argumentos <- expresion$argumentos

  if (!es_texto_escalar(operacion)) {
    errores <- c(
      errores,
      paste0(
        ruta,
        ": falta una operación válida."
      )
    )

    return(errores)
  }

  if (!operacion %in% names(operaciones_permitidas)) {
    errores <- c(
      errores,
      paste0(
        ruta,
        ": la operación '",
        operacion,
        "' no está permitida."
      )
    )

    return(errores)
  }

  if (!is.list(argumentos)) {
    errores <- c(
      errores,
      paste0(
        ruta,
        ": 'argumentos' debe ser una lista."
      )
    )

    return(errores)
  }

  aridad <- operaciones_permitidas[[operacion]]
  cantidad <- length(argumentos)

  if (cantidad < aridad[["minimo"]]) {
    errores <- c(
      errores,
      paste0(
        ruta,
        ": la operación '",
        operacion,
        "' necesita al menos ",
        aridad[["minimo"]],
        " argumentos."
      )
    )
  }

  if (
    is.finite(aridad[["maximo"]]) &&
      cantidad > aridad[["maximo"]]
  ) {
    errores <- c(
      errores,
      paste0(
        ruta,
        ": la operación '",
        operacion,
        "' acepta como máximo ",
        aridad[["maximo"]],
        " argumentos."
      )
    )
  }

  for (indice in seq_along(argumentos)) {
    errores_argumento <- validar_expresion(
      expresion = argumentos[[indice]],
      referencias_permitidas = referencias_permitidas,
      ruta = paste0(
        ruta,
        ".argumentos[",
        indice,
        "]"
      )
    )

    errores <- c(
      errores,
      errores_argumento
    )
  }

  unique(errores)
}
