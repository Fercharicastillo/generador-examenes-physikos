extraer_marcadores <- function(texto) {
  if (!es_texto_escalar(texto)) {
    return(character())
  }

  coincidencias <- gregexpr(
    "\\{\\{[A-Za-z][A-Za-z0-9_]*\\}\\}",
    texto,
    perl = TRUE
  )

  marcadores <- regmatches(
    texto,
    coincidencias
  )[[1]]

  if (length(marcadores) == 0) {
    return(character())
  }

  unique(
    gsub(
      "^\\{\\{|\\}\\}$",
      "",
      marcadores
    )
  )
}

contiene_comando_latex_peligroso <- function(texto) {
  if (!es_texto_escalar(texto)) {
    return(FALSE)
  }

  comandos <- c(
    "\\input",
    "\\include",
    "\\write",
    "\\openout",
    "\\read",
    "\\usepackage",
    "\\documentclass",
    "\\immediate"
  )

  any(
    vapply(
      comandos,
      function(comando) {
        grepl(
          comando,
          texto,
          fixed = TRUE
        )
      },
      logical(1)
    )
  )
}

validar_pregunta <- function(pregunta) {
  errores <- character()
  advertencias <- character()

  agregar_error <- function(mensaje) {
    errores <<- c(errores, mensaje)
  }

  agregar_advertencia <- function(mensaje) {
    advertencias <<- c(
      advertencias,
      mensaje
    )
  }

  if (!is.list(pregunta)) {
    return(list(
      valida = FALSE,
      errores = "La pregunta debe ser un objeto JSON.",
      advertencias = character()
    ))
  }

  campos_obligatorios <- c(
    "formato",
    "version_formato",
    "id",
    "version_pregunta",
    "titulo",
    "area",
    "tema",
    "dificultad",
    "enunciado",
    "incisos",
    "variables",
    "restricciones",
    "solucion"
  )

  campos_faltantes <- setdiff(
    campos_obligatorios,
    names(pregunta)
  )

  if (length(campos_faltantes) > 0) {
    agregar_error(
      paste0(
        "Faltan campos obligatorios: ",
        paste(
          campos_faltantes,
          collapse = ", "
        ),
        "."
      )
    )
  }

  if (
    !es_texto_escalar(pregunta$formato) ||
    pregunta$formato != "physikos-question"
  ) {
    agregar_error(
      "El campo 'formato' debe ser 'physikos-question'."
    )
  }

  if (
    !es_entero(pregunta$version_formato) ||
    pregunta$version_formato != 1
  ) {
    agregar_error(
      "La versión del formato debe ser 1."
    )
  }

  if (
    !es_texto_escalar(pregunta$id) ||
    !grepl(
      "^[a-z][a-z0-9]*(?:-[a-z0-9]+)+$",
      pregunta$id
    )
  ) {
    agregar_error(
      paste(
        "El identificador debe utilizar minúsculas,",
        "números y guiones."
      )
    )
  }

  if (
    !es_entero(pregunta$version_pregunta) ||
    pregunta$version_pregunta < 1
  ) {
    agregar_error(
      "'version_pregunta' debe ser un entero mayor o igual a 1."
    )
  }

  campos_texto <- c(
    "titulo",
    "area",
    "tema",
    "dificultad",
    "enunciado"
  )

  for (campo in campos_texto) {
    if (!es_texto_escalar(pregunta[[campo]])) {
      agregar_error(
        paste0(
          "El campo '",
          campo,
          "' debe contener texto."
        )
      )
    }
  }

  # Validar variables
  variables <- pregunta$variables
  nombres_variables <- character()

  if (
    !is.list(variables) ||
    length(variables) == 0 ||
    is.null(names(variables)) ||
    any(!nzchar(names(variables)))
  ) {
    agregar_error(
      "'variables' debe ser un objeto con al menos una variable."
    )
  } else {
    nombres_variables <- names(variables)

    for (nombre in nombres_variables) {
      variable <- variables[[nombre]]
      ruta <- paste0("variables.", nombre)

      if (
        !grepl(
          "^[A-Za-z][A-Za-z0-9_]*$",
          nombre
        )
      ) {
        agregar_error(
          paste0(
            ruta,
            ": el nombre no es válido."
          )
        )
      }

      if (!is.list(variable)) {
        agregar_error(
          paste0(
            ruta,
            ": debe ser un objeto."
          )
        )
        next
      }

      tipo <- variable$tipo

      if (
        !es_texto_escalar(tipo) ||
        !tipo %in% c(
          "entero",
          "decimal",
          "constante"
        )
      ) {
        agregar_error(
          paste0(
            ruta,
            ": tipo de variable no permitido."
          )
        )
        next
      }

      if (!es_texto_escalar(variable$unidad)) {
        agregar_error(
          paste0(
            ruta,
            ": debe declarar una unidad."
          )
        )
      }

      if (tipo %in% c("entero", "decimal")) {
        if (!es_numero_escalar(variable$minimo)) {
          agregar_error(
            paste0(
              ruta,
              ".minimo debe ser numérico."
            )
          )
        }

        if (!es_numero_escalar(variable$maximo)) {
          agregar_error(
            paste0(
              ruta,
              ".maximo debe ser numérico."
            )
          )
        }

        if (
          es_numero_escalar(variable$minimo) &&
          es_numero_escalar(variable$maximo) &&
          variable$minimo > variable$maximo
        ) {
          agregar_error(
            paste0(
              ruta,
              ": el mínimo no puede superar el máximo."
            )
          )
        }
      }

      if (tipo == "entero") {
        if (
          es_numero_escalar(variable$minimo) &&
          !es_entero(variable$minimo)
        ) {
          agregar_error(
            paste0(
              ruta,
              ".minimo debe ser entero."
            )
          )
        }

        if (
          es_numero_escalar(variable$maximo) &&
          !es_entero(variable$maximo)
        ) {
          agregar_error(
            paste0(
              ruta,
              ".maximo debe ser entero."
            )
          )
        }
      }

      if (tipo == "decimal") {
        if (
          !es_entero(variable$decimales) ||
          variable$decimales < 0 ||
          variable$decimales > 10
        ) {
          agregar_error(
            paste0(
              ruta,
              ".decimales debe ser un entero entre 0 y 10."
            )
          )
        }
      }

      if (
        tipo == "constante" &&
        !es_numero_escalar(variable$valor)
      ) {
        agregar_error(
          paste0(
            ruta,
            ".valor debe ser numérico."
          )
        )
      }
    }
  }

  # Relacionar marcadores y variables
  marcadores <- extraer_marcadores(
    pregunta$enunciado
  )

  marcadores_sin_variable <- setdiff(
    marcadores,
    nombres_variables
  )

  for (marcador in marcadores_sin_variable) {
    agregar_error(
      paste0(
        "El marcador {{",
        marcador,
        "}} no tiene una variable definida."
      )
    )
  }

  variables_no_usadas <- setdiff(
    nombres_variables,
    marcadores
  )

  for (variable in variables_no_usadas) {
    agregar_advertencia(
      paste0(
        "La variable '",
        variable,
        "' no aparece en el enunciado."
      )
    )
  }

  # Validar incisos
  incisos <- pregunta$incisos
  ids_incisos <- character()
  resultados_incisos <- character()

  if (!is.list(incisos) || length(incisos) == 0) {
    agregar_error(
      "La pregunta debe contener al menos un inciso."
    )
  } else {
    for (indice in seq_along(incisos)) {
      inciso <- incisos[[indice]]
      ruta <- paste0(
        "incisos[",
        indice,
        "]"
      )

      if (!is.list(inciso)) {
        agregar_error(
          paste0(ruta, " debe ser un objeto.")
        )
        next
      }

      if (!es_texto_escalar(inciso$id)) {
        agregar_error(
          paste0(ruta, ".id no es válido.")
        )
      } else {
        ids_incisos <- c(
          ids_incisos,
          inciso$id
        )
      }

      if (!es_texto_escalar(inciso$texto)) {
        agregar_error(
          paste0(ruta, ".texto no es válido.")
        )
      }

      if (!es_texto_escalar(inciso$resultado)) {
        agregar_error(
          paste0(ruta, ".resultado no es válido.")
        )
      } else {
        resultados_incisos <- c(
          resultados_incisos,
          inciso$resultado
        )
      }
    }

    ids_duplicados <- unique(
      ids_incisos[
        duplicated(ids_incisos)
      ]
    )

    if (length(ids_duplicados) > 0) {
      agregar_error(
        paste0(
          "Hay incisos duplicados: ",
          paste(ids_duplicados, collapse = ", "),
          "."
        )
      )
    }
  }

  # Validar restricciones
  restricciones <- pregunta$restricciones

  if (!is.list(restricciones)) {
    agregar_error(
      "'restricciones' debe ser una lista."
    )
  } else {
    for (indice in seq_along(restricciones)) {
      restriccion <- restricciones[[indice]]
      ruta <- paste0(
        "restricciones[",
        indice,
        "]"
      )

      if (!is.list(restriccion)) {
        agregar_error(
          paste0(ruta, " debe ser un objeto.")
        )
        next
      }

      if (
        !es_texto_escalar(
          restriccion$descripcion
        )
      ) {
        agregar_error(
          paste0(
            ruta,
            ".descripcion no es válida."
          )
        )
      }

      errores_expresion <- validar_expresion(
        expresion = restriccion$expresion,
        referencias_permitidas = nombres_variables,
        ruta = paste0(ruta, ".expresion")
      )

      errores <- c(
        errores,
        errores_expresion
      )
    }
  }

  # Validar solución
  pasos <- pregunta$solucion$pasos
  resultados_generados <- character()
  referencias_disponibles <- nombres_variables

  if (!is.list(pasos) || length(pasos) == 0) {
    agregar_error(
      "La solución debe contener al menos un paso."
    )
  } else {
    for (indice in seq_along(pasos)) {
      paso <- pasos[[indice]]
      ruta <- paste0(
        "solucion.pasos[",
        indice,
        "]"
      )

      if (!is.list(paso)) {
        agregar_error(
          paste0(ruta, " debe ser un objeto.")
        )
        next
      }

      if (
        !es_texto_escalar(paso$inciso) ||
        !paso$inciso %in% ids_incisos
      ) {
        agregar_error(
          paste0(
            ruta,
            ": referencia un inciso inexistente."
          )
        )
      }

      if (!es_texto_escalar(paso$explicacion)) {
        agregar_error(
          paste0(
            ruta,
            ".explicacion no es válida."
          )
        )
      }

      if (!es_texto_escalar(paso$formula_latex)) {
        agregar_error(
          paste0(
            ruta,
            ".formula_latex no es válida."
          )
        )
      } else if (
        contiene_comando_latex_peligroso(
          paso$formula_latex
        )
      ) {
        agregar_error(
          paste0(
            ruta,
            ": contiene un comando LaTeX no permitido."
          )
        )
      }

      errores_calculo <- validar_expresion(
        expresion = paso$calculo,
        referencias_permitidas =
          referencias_disponibles,
        ruta = paste0(ruta, ".calculo")
      )

      errores <- c(
        errores,
        errores_calculo
      )

      if (
        !es_texto_escalar(paso$guardar_como) ||
        !grepl(
          "^[A-Za-z][A-Za-z0-9_]*$",
          paso$guardar_como
        )
      ) {
        agregar_error(
          paste0(
            ruta,
            ".guardar_como no es válido."
          )
        )
      } else if (
        paso$guardar_como %in%
          resultados_generados
      ) {
        agregar_error(
          paste0(
            ruta,
            ": el resultado '",
            paso$guardar_como,
            "' está duplicado."
          )
        )
      } else {
        resultados_generados <- c(
          resultados_generados,
          paso$guardar_como
        )

        referencias_disponibles <- c(
          referencias_disponibles,
          paso$guardar_como
        )
      }

      if (!es_texto_escalar(paso$unidad)) {
        agregar_error(
          paste0(
            ruta,
            ".unidad no es válida."
          )
        )
      }

      if (
        !es_entero(paso$decimales) ||
        paso$decimales < 0 ||
        paso$decimales > 10
      ) {
        agregar_error(
          paste0(
            ruta,
            ".decimales debe estar entre 0 y 10."
          )
        )
      }
    }
  }

  resultados_inexistentes <- setdiff(
    resultados_incisos,
    resultados_generados
  )

  for (resultado in resultados_inexistentes) {
    agregar_error(
      paste0(
        "El inciso solicita el resultado '",
        resultado,
        "', pero la solución no lo genera."
      )
    )
  }

  resultados_no_utilizados <- setdiff(
    resultados_generados,
    resultados_incisos
  )

  for (resultado in resultados_no_utilizados) {
    agregar_advertencia(
      paste0(
        "La solución genera '",
        resultado,
        "', pero ningún inciso lo solicita."
      )
    )
  }

  list(
    valida = length(errores) == 0,
    errores = unique(errores),
    advertencias = unique(advertencias)
  )
}

validar_archivo_pregunta <- function(ruta) {
  if (!file.exists(ruta)) {
    return(list(
      valida = FALSE,
      errores = paste(
        "El archivo no existe:",
        ruta
      ),
      advertencias = character()
    ))
  }

  pregunta <- tryCatch(
    {
      jsonlite::read_json(
        ruta,
        simplifyVector = FALSE
      )
    },
    error = function(error) {
      error
    }
  )

  if (inherits(pregunta, "error")) {
    return(list(
      valida = FALSE,
      errores = paste(
        "El JSON no se pudo leer:",
        conditionMessage(pregunta)
      ),
      advertencias = character()
    ))
  }

  resultado <- validar_pregunta(pregunta)
  resultado$pregunta <- pregunta
  resultado
}