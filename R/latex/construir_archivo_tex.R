# Este módulo usa funciones cargadas previamente con source().
# nolint start: object_usage_linter.

validar_nombre_plantilla <- function(
  plantilla
) {
  if (
    !es_texto_escalar(plantilla) ||
      !grepl(
        "^[a-z][a-z0-9_-]*$",
        plantilla
      )
  ) {
    stop(
      paste(
        "El nombre de la plantilla debe contener",
        "solamente letras minúsculas, números,",
        "guiones o guiones bajos."
      ),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

obtener_ruta_plantilla <- function(
  carpeta_plantillas,
  plantilla,
  tipo
) {
  validar_nombre_plantilla(
    plantilla
  )

  tipos_permitidos <- c(
    "examen",
    "solucion"
  )

  if (
    !es_texto_escalar(tipo) ||
      !tipo %in% tipos_permitidos
  ) {
    stop(
      paste0(
        "'tipo' debe ser uno de: ",
        paste(
          tipos_permitidos,
          collapse = ", "
        ),
        "."
      ),
      call. = FALSE
    )
  }

  if (!dir.exists(carpeta_plantillas)) {
    stop(
      paste0(
        "La carpeta de plantillas no existe: ",
        carpeta_plantillas,
        "."
      ),
      call. = FALSE
    )
  }

  ruta <- file.path(
    carpeta_plantillas,
    plantilla,
    paste0(
      tipo,
      ".tex.tpl"
    )
  )

  if (!file.exists(ruta)) {
    stop(
      paste0(
        "No existe la plantilla LaTeX: ",
        ruta,
        "."
      ),
      call. = FALSE
    )
  }

  normalizePath(
    ruta,
    winslash = "/",
    mustWork = TRUE
  )
}

leer_plantilla_tex <- function(
  ruta
) {
  lineas <- readLines(
    ruta,
    encoding = "UTF-8",
    warn = FALSE
  )

  if (length(lineas) == 0) {
    stop(
      paste0(
        "La plantilla está vacía: ",
        ruta,
        "."
      ),
      call. = FALSE
    )
  }

  paste(
    lineas,
    collapse = "\n"
  )
}

sustituir_token_tex <- function(
  texto,
  token,
  valor
) {
  if (!is.character(texto) || length(texto) != 1) {
    stop(
      "'texto' debe ser una cadena.",
      call. = FALSE
    )
  }

  if (!es_texto_escalar(token)) {
    stop(
      "'token' debe ser un texto válido.",
      call. = FALSE
    )
  }

  if (!is.character(valor) || length(valor) != 1) {
    stop(
      "'valor' debe ser una cadena.",
      call. = FALSE
    )
  }

  posiciones <- gregexpr(
    token,
    texto,
    fixed = TRUE
  )[[1]]

  if (
    length(posiciones) == 1 &&
      posiciones[[1]] == -1
  ) {
    stop(
      paste0(
        "La plantilla no contiene el marcador ",
        token,
        "."
      ),
      call. = FALSE
    )
  }

  if (length(posiciones) != 1) {
    stop(
      paste0(
        "El marcador ",
        token,
        " debe aparecer exactamente una vez."
      ),
      call. = FALSE
    )
  }

  inicio <- posiciones[[1]]
  fin <- inicio + nchar(token) - 1

  parte_anterior <- if (inicio > 1) {
    substr(
      texto,
      1,
      inicio - 1
    )
  } else {
    ""
  }

  parte_posterior <- if (fin < nchar(texto)) {
    substr(
      texto,
      fin + 1,
      nchar(texto)
    )
  } else {
    ""
  }

  paste0(
    parte_anterior,
    valor,
    parte_posterior
  )
}

validar_fragmentos_latex <- function(
  fragmentos
) {
  if (!is.list(fragmentos)) {
    stop(
      "'fragmentos' debe ser una lista.",
      call. = FALSE
    )
  }

  campos_obligatorios <- c(
    "formato",
    "version_formato",
    "id",
    "examen",
    "solucion"
  )

  faltantes <- setdiff(
    campos_obligatorios,
    names(fragmentos)
  )

  if (length(faltantes) > 0) {
    stop(
      paste0(
        "Faltan campos en los fragmentos: ",
        paste(
          faltantes,
          collapse = ", "
        ),
        "."
      ),
      call. = FALSE
    )
  }

  if (
    !identical(
      fragmentos$formato,
      "physikos-latex-fragments"
    )
  ) {
    stop(
      paste0(
        "El formato de fragmentos no es compatible: ",
        fragmentos$formato,
        "."
      ),
      call. = FALSE
    )
  }

  if (
    !is.numeric(fragmentos$version_formato) ||
      length(fragmentos$version_formato) != 1 ||
      is.na(fragmentos$version_formato) ||
      fragmentos$version_formato != 1
  ) {
    stop(
      paste(
        "La versión de los fragmentos",
        "no es compatible."
      ),
      call. = FALSE
    )
  }

  if (!es_texto_escalar(fragmentos$id)) {
    stop(
      "Los fragmentos no contienen un identificador válido.",
      call. = FALSE
    )
  }

  if (
    !is.list(fragmentos$examen) ||
      !es_texto_escalar(
        fragmentos$examen$contenido
      )
  ) {
    stop(
      "El fragmento de examen no es válido.",
      call. = FALSE
    )
  }

  if (
    !is.null(fragmentos$solucion) &&
      (
        !is.list(fragmentos$solucion) ||
          !es_texto_escalar(
            fragmentos$solucion$contenido
          )
      )
  ) {
    stop(
      "El fragmento de solución no es válido.",
      call. = FALSE
    )
  }

  invisible(TRUE)
}

construir_archivo_tex <- function(
  fragmentos,
  plantilla,
  tipo,
  carpeta_plantillas
) {
  validar_fragmentos_latex(
    fragmentos
  )

  ruta_plantilla <- obtener_ruta_plantilla(
    carpeta_plantillas = carpeta_plantillas,
    plantilla = plantilla,
    tipo = tipo
  )

  contenido_fragmento <- switch(
    tipo,
    examen = fragmentos$examen$contenido,
    solucion = {
      if (is.null(fragmentos$solucion)) {
        stop(
          paste(
            "No se puede construir un solucionario:",
            "los fragmentos no contienen solución."
          ),
          call. = FALSE
        )
      }

      fragmentos$solucion$contenido
    }
  )

  tipo_documento <- switch(
    tipo,
    examen = "Evaluación Physikos",
    solucion = "Solucionario Physikos"
  )

  plantilla_tex <- leer_plantilla_tex(
    ruta_plantilla
  )

  contenido_tex <- sustituir_token_tex(
    texto = plantilla_tex,
    token = "@@TIPO_DOCUMENTO@@",
    valor = escapar_latex(
      tipo_documento
    )
  )

  contenido_tex <- sustituir_token_tex(
    texto = contenido_tex,
    token = "@@IDENTIFICADOR@@",
    valor = escapar_latex(
      fragmentos$id
    )
  )

  contenido_tex <- sustituir_token_tex(
    texto = contenido_tex,
    token = "@@CONTENIDO@@",
    valor = contenido_fragmento
  )

  marcadores_restantes <- grepl(
    "@@[^@]+@@",
    contenido_tex,
    perl = TRUE
  )

  if (marcadores_restantes) {
    stop(
      paste(
        "La plantilla contiene marcadores",
        "que no fueron sustituidos."
      ),
      call. = FALSE
    )
  }

  list(
    formato = "physikos-tex-document",
    version_formato = 1L,
    id = fragmentos$id,
    plantilla = plantilla,
    tipo = tipo,
    nombre_archivo = paste0(
      fragmentos$id,
      "_",
      tipo,
      ".tex"
    ),
    ruta_plantilla = ruta_plantilla,
    contenido = contenido_tex
  )
}

# nolint end