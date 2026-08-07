formatear_valor_variable <- function(
  valor,
  definicion,
  nombre
) {
  if (!es_numero_escalar(valor)) {
    stop(
      paste0(
        "La variable '",
        nombre,
        "' no contiene un número finito."
      ),
      call. = FALSE
    )
  }

  tipo <- definicion$tipo

  if (identical(tipo, "entero")) {
    return(
      format(
        as.integer(valor),
        scientific = FALSE,
        trim = TRUE
      )
    )
  }

  if (identical(tipo, "decimal")) {
    return(
      formatC(
        valor,
        format = "f",
        digits = definicion$decimales,
        decimal.mark = "."
      )
    )
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

preparar_reemplazos <- function(
  pregunta,
  valores
) {
  nombres <- names(pregunta$variables)

  reemplazos <- vapply(
    nombres,
    function(nombre) {
      formatear_valor_variable(
        valor = valores[[nombre]],
        definicion = pregunta$variables[[nombre]],
        nombre = nombre
      )
    },
    character(1)
  )

  reemplazos
}

sustituir_marcadores <- function(
  texto,
  reemplazos
) {
  if (!es_texto_escalar(texto)) {
    stop(
      "El texto que se desea renderizar no es válido.",
      call. = FALSE
    )
  }

  coincidencias <- gregexpr(
    "\\{\\{[A-Za-z][A-Za-z0-9_]*\\}\\}",
    texto,
    perl = TRUE
  )

  marcadores_completos <- regmatches(
    texto,
    coincidencias
  )[[1]]

  if (length(marcadores_completos) == 0) {
    return(texto)
  }

  nombres <- gsub(
    "^\\{\\{|\\}\\}$",
    "",
    marcadores_completos
  )

  desconocidos <- setdiff(
    nombres,
    names(reemplazos)
  )

  if (length(desconocidos) > 0) {
    stop(
      paste0(
        "No existen valores para los marcadores: ",
        paste(
          unique(desconocidos),
          collapse = ", "
        ),
        "."
      ),
      call. = FALSE
    )
  }

  valores_sustitucion <- unname(
    reemplazos[nombres]
  )

  regmatches(
    texto,
    coincidencias
  ) <- list(valores_sustitucion)

  texto
}

escapar_latex <- function(texto) {
  if (!es_texto_escalar(texto)) {
    stop(
      "El texto para LaTeX no es válido.",
      call. = FALSE
    )
  }

  reemplazos <- c(
    "\\" = "\\textbackslash{}",
    "{" = "\\{",
    "}" = "\\}",
    "$" = "\\$",
    "&" = "\\&",
    "#" = "\\#",
    "%" = "\\%",
    "_" = "\\_",
    "^" = "\\textasciicircum{}",
    "~" = "\\textasciitilde{}"
  )

  caracteres <- strsplit(
    texto,
    split = "",
    fixed = TRUE
  )[[1]]

  caracteres_escapados <- vapply(
    caracteres,
    function(caracter) {
      if (caracter %in% names(reemplazos)) {
        reemplazos[[caracter]]
      } else {
        caracter
      }
    },
    character(1)
  )

  paste0(
    caracteres_escapados,
    collapse = ""
  )
}

renderizar_enunciado <- function(
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
          "No se puede renderizar la pregunta:",
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

  reemplazos <- preparar_reemplazos(
    pregunta = pregunta,
    valores = valores
  )

  enunciado_texto <- sustituir_marcadores(
    texto = pregunta$enunciado,
    reemplazos = reemplazos
  )

  incisos_renderizados <- lapply(
    pregunta$incisos,
    function(inciso) {
      texto_inciso <- sustituir_marcadores(
        texto = inciso$texto,
        reemplazos = reemplazos
      )

      list(
        id = inciso$id,
        resultado = inciso$resultado,
        texto = texto_inciso,
        latex = escapar_latex(texto_inciso)
      )
    }
  )

  list(
    id = pregunta$id,
    titulo = pregunta$titulo,
    texto = enunciado_texto,
    latex = escapar_latex(enunciado_texto),
    incisos = incisos_renderizados,
    valores_formateados = as.list(reemplazos)
  )
}