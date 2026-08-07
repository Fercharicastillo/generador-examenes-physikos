validar_ruta_tex <- function(
  ruta_tex
) {
  if (
    !is.character(ruta_tex) ||
      length(ruta_tex) != 1 ||
      is.na(ruta_tex) ||
      !nzchar(trimws(ruta_tex))
  ) {
    stop(
      "'ruta_tex' debe ser una ruta válida.",
      call. = FALSE
    )
  }

  if (!file.exists(ruta_tex)) {
    stop(
      paste0(
        "El archivo TEX no existe: ",
        ruta_tex,
        "."
      ),
      call. = FALSE
    )
  }

  if (dir.exists(ruta_tex)) {
    stop(
      paste0(
        "La ruta TEX corresponde a un directorio: ",
        ruta_tex,
        "."
      ),
      call. = FALSE
    )
  }

  if (
    !grepl(
      "\\.tex$",
      basename(ruta_tex),
      ignore.case = TRUE
    )
  ) {
    stop(
      paste(
        "El archivo de entrada debe tener",
        "la extensión .tex."
      ),
      call. = FALSE
    )
  }

  normalizePath(
    ruta_tex,
    winslash = "/",
    mustWork = TRUE
  )
}

localizar_motor_latex <- function(
  motor
) {
  motores_permitidos <- c(
    "pdflatex",
    "xelatex",
    "lualatex"
  )

  if (
    !is.character(motor) ||
      length(motor) != 1 ||
      is.na(motor) ||
      !motor %in% motores_permitidos
  ) {
    stop(
      paste0(
        "'motor' debe ser uno de: ",
        paste(
          motores_permitidos,
          collapse = ", "
        ),
        "."
      ),
      call. = FALSE
    )
  }

  ruta_motor <- Sys.which(
    motor
  )

  if (!nzchar(ruta_motor)) {
    stop(
      paste0(
        "El motor LaTeX '",
        motor,
        "' no está disponible en el PATH."
      ),
      call. = FALSE
    )
  }

  unname(ruta_motor)
}

construir_rutas_compilacion <- function(
  ruta_tex
) {
  directorio <- dirname(
    ruta_tex
  )

  nombre_tex <- basename(
    ruta_tex
  )

  nombre_base <- sub(
    "\\.tex$",
    "",
    nombre_tex,
    ignore.case = TRUE
  )

  list(
    directorio = directorio,
    nombre_base = nombre_base,
    pdf = file.path(
      directorio,
      paste0(
        nombre_base,
        ".pdf"
      )
    ),
    registro = file.path(
      directorio,
      paste0(
        nombre_base,
        "_compilacion.log"
      )
    ),
    log_latex = file.path(
      directorio,
      paste0(
        nombre_base,
        ".log"
      )
    )
  )
}

guardar_registro_compilacion <- function(
  salida,
  ruta_registro,
  motor,
  codigo_salida,
  duracion_segundos
) {
  encabezado <- c(
    paste0(
      "Motor: ",
      motor
    ),
    paste0(
      "Código de salida: ",
      codigo_salida
    ),
    paste0(
      "Duración: ",
      format(
        duracion_segundos,
        digits = 6
      ),
      " segundos"
    ),
    paste0(
      "Fecha: ",
      format(
        Sys.time(),
        "%Y-%m-%dT%H:%M:%S%z"
      )
    ),
    "",
    "Salida del motor LaTeX:",
    "------------------------"
  )

  contenido <- c(
    encabezado,
    as.character(salida)
  )

  conexion <- file(
    ruta_registro,
    open = "wb"
  )

  on.exit(
    close(conexion),
    add = TRUE
  )

  texto <- paste(
    contenido,
    collapse = "\n"
  )

  writeBin(
    charToRaw(
      enc2utf8(texto)
    ),
    conexion
  )

  invisible(TRUE)
}

resumir_error_compilacion <- function(
  salida,
  cantidad = 20L
) {
  if (length(salida) == 0) {
    return(
      "El motor LaTeX no produjo salida."
    )
  }

  cantidad <- min(
    cantidad,
    length(salida)
  )

  paste(
    tail(
      salida,
      cantidad
    ),
    collapse = "\n"
  )
}

compilar_archivo_tex <- function(
  ruta_tex,
  motor = "pdflatex"
) {
  ruta_tex <- validar_ruta_tex(
    ruta_tex
  )

  ruta_motor <- localizar_motor_latex(
    motor
  )

  rutas <- construir_rutas_compilacion(
    ruta_tex
  )

  if (file.exists(rutas$pdf)) {
    stop(
      paste0(
        "El PDF de salida ya existe: ",
        rutas$pdf,
        "."
      ),
      call. = FALSE
    )
  }

  if (file.exists(rutas$registro)) {
    stop(
      paste0(
        "El registro de compilación ya existe: ",
        rutas$registro,
        "."
      ),
      call. = FALSE
    )
  }

  argumentos <- c(
    "-interaction=nonstopmode",
    "-halt-on-error",
    "-file-line-error",
    "-no-shell-escape",
    paste0(
      "-output-directory=",
      shQuote(
        rutas$directorio
      )
    ),
    shQuote(
      ruta_tex
    )
  )

  inicio <- proc.time()[["elapsed"]]

  salida <- suppressWarnings(
    system2(
      command = ruta_motor,
      args = argumentos,
      stdout = TRUE,
      stderr = TRUE
    )
  )

  duracion <- unname(
    proc.time()[["elapsed"]] - inicio
  )

  codigo_salida <- attr(
    salida,
    "status"
  )

  if (is.null(codigo_salida)) {
    codigo_salida <- 0L
  } else {
    codigo_salida <- as.integer(
      codigo_salida
    )
  }

  guardar_registro_compilacion(
    salida = salida,
    ruta_registro = rutas$registro,
    motor = motor,
    codigo_salida = codigo_salida,
    duracion_segundos = duracion
  )

  compilacion_exitosa <- (
    codigo_salida == 0L &&
      file.exists(rutas$pdf)
  )

  if (!compilacion_exitosa) {
    resumen <- resumir_error_compilacion(
      salida
    )

    stop(
      paste0(
        "La compilación LaTeX falló.\n",
        "Código de salida: ",
        codigo_salida,
        "\n",
        "Registro: ",
        rutas$registro,
        "\n\n",
        resumen
      ),
      call. = FALSE
    )
  }

  informacion_pdf <- file.info(
    rutas$pdf
  )

  if (
    is.na(informacion_pdf$size) ||
      informacion_pdf$size <= 0
  ) {
    stop(
      paste0(
        "La compilación terminó, pero el PDF está vacío: ",
        rutas$pdf,
        "."
      ),
      call. = FALSE
    )
  }

  list(
    formato = "physikos-compiled-pdf",
    version_formato = 1L,
    motor = motor,
    codigo_salida = codigo_salida,
    ruta_tex = ruta_tex,
    ruta_pdf = normalizePath(
      rutas$pdf,
      winslash = "/",
      mustWork = TRUE
    ),
    ruta_registro = normalizePath(
      rutas$registro,
      winslash = "/",
      mustWork = TRUE
    ),
    ruta_log_latex = if (
      file.exists(rutas$log_latex)
    ) {
      normalizePath(
        rutas$log_latex,
        winslash = "/",
        mustWork = TRUE
      )
    } else {
      NULL
    },
    bytes = unname(
      informacion_pdf$size
    ),
    duracion_segundos = duracion
  )
}
