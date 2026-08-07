# Este módulo usa funciones cargadas previamente con source().
# nolint start: object_usage_linter.

validar_objeto_tex <- function(
  archivo_tex
) {
  if (!is.list(archivo_tex)) {
    stop(
      "'archivo_tex' debe ser una lista.",
      call. = FALSE
    )
  }

  campos_obligatorios <- c(
    "formato",
    "version_formato",
    "id",
    "plantilla",
    "tipo",
    "nombre_archivo",
    "contenido"
  )

  campos_faltantes <- setdiff(
    campos_obligatorios,
    names(archivo_tex)
  )

  if (length(campos_faltantes) > 0) {
    stop(
      paste0(
        "El objeto TEX no contiene los campos: ",
        paste(
          campos_faltantes,
          collapse = ", "
        ),
        "."
      ),
      call. = FALSE
    )
  }

  if (
    !identical(
      archivo_tex$formato,
      "physikos-tex-document"
    )
  ) {
    stop(
      paste0(
        "El formato del objeto TEX no es compatible: ",
        archivo_tex$formato,
        "."
      ),
      call. = FALSE
    )
  }

  if (
    !is.numeric(archivo_tex$version_formato) ||
      length(archivo_tex$version_formato) != 1 ||
      is.na(archivo_tex$version_formato) ||
      archivo_tex$version_formato != 1
  ) {
    stop(
      paste(
        "La versión del objeto TEX",
        "no es compatible."
      ),
      call. = FALSE
    )
  }

  if (!es_texto_escalar(archivo_tex$id)) {
    stop(
      "El objeto TEX no contiene un identificador válido.",
      call. = FALSE
    )
  }

  if (!es_texto_escalar(archivo_tex$plantilla)) {
    stop(
      "El objeto TEX no contiene una plantilla válida.",
      call. = FALSE
    )
  }

  if (
    !es_texto_escalar(archivo_tex$tipo) ||
      !archivo_tex$tipo %in% c(
        "examen",
        "solucion"
      )
  ) {
    stop(
      paste(
        "El tipo del objeto TEX debe ser",
        "'examen' o 'solucion'."
      ),
      call. = FALSE
    )
  }

  if (
    !is.character(archivo_tex$contenido) ||
      length(archivo_tex$contenido) != 1 ||
      is.na(archivo_tex$contenido) ||
      !nzchar(archivo_tex$contenido)
  ) {
    stop(
      "El contenido TEX no es válido.",
      call. = FALSE
    )
  }

  invisible(TRUE)
}

validar_nombre_archivo_tex <- function(
  nombre_archivo
) {
  if (!es_texto_escalar(nombre_archivo)) {
    stop(
      "El nombre del archivo TEX no es válido.",
      call. = FALSE
    )
  }

  if (
    !identical(
      basename(nombre_archivo),
      nombre_archivo
    )
  ) {
    stop(
      paste(
        "El nombre del archivo TEX no debe",
        "contener rutas o directorios."
      ),
      call. = FALSE
    )
  }

  patron <- paste0(
    "^[A-Za-z0-9]",
    "[A-Za-z0-9._-]*",
    "\\.tex$"
  )

  if (!grepl(patron, nombre_archivo)) {
    stop(
      paste(
        "El nombre del archivo TEX contiene",
        "caracteres no permitidos."
      ),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

preparar_directorio_tex <- function(
  directorio_salida
) {
  if (
    !is.character(directorio_salida) ||
      length(directorio_salida) != 1 ||
      is.na(directorio_salida) ||
      !nzchar(trimws(directorio_salida))
  ) {
    stop(
      "'directorio_salida' no es válido.",
      call. = FALSE
    )
  }

  if (!dir.exists(directorio_salida)) {
    creado <- dir.create(
      directorio_salida,
      recursive = TRUE,
      showWarnings = FALSE
    )

    if (!creado && !dir.exists(directorio_salida)) {
      stop(
        paste0(
          "No se pudo crear el directorio: ",
          directorio_salida,
          "."
        ),
        call. = FALSE
      )
    }
  }

  normalizePath(
    directorio_salida,
    winslash = "/",
    mustWork = TRUE
  )
}

escribir_contenido_utf8 <- function(
  contenido,
  ruta
) {
  contenido_utf8 <- enc2utf8(
    contenido
  )

  conexion <- file(
    ruta,
    open = "wb"
  )

  on.exit(
    close(conexion),
    add = TRUE
  )

  writeBin(
    charToRaw(contenido_utf8),
    conexion
  )

  invisible(TRUE)
}

guardar_archivo_tex <- function(
  archivo_tex,
  directorio_salida
) {
  validar_objeto_tex(
    archivo_tex
  )

  validar_nombre_archivo_tex(
    archivo_tex$nombre_archivo
  )

  directorio_normalizado <- preparar_directorio_tex(
    directorio_salida
  )

  ruta_destino <- file.path(
    directorio_normalizado,
    archivo_tex$nombre_archivo
  )

  if (file.exists(ruta_destino)) {
    stop(
      paste0(
        "El archivo de salida ya existe: ",
        ruta_destino,
        "."
      ),
      call. = FALSE
    )
  }

  ruta_temporal <- tempfile(
    pattern = ".physikos-",
    tmpdir = directorio_normalizado,
    fileext = ".tmp"
  )

  conservar_temporal <- TRUE

  on.exit(
    {
      if (
        conservar_temporal &&
          file.exists(ruta_temporal)
      ) {
        unlink(
          ruta_temporal,
          force = TRUE
        )
      }
    },
    add = TRUE
  )

  escribir_contenido_utf8(
    contenido = archivo_tex$contenido,
    ruta = ruta_temporal
  )

  if (!file.exists(ruta_temporal)) {
    stop(
      paste(
        "No se pudo crear el archivo",
        "temporal TEX."
      ),
      call. = FALSE
    )
  }

  movido <- file.rename(
    from = ruta_temporal,
    to = ruta_destino
  )

  if (!movido) {
    stop(
      paste0(
        "No se pudo mover el archivo TEX a: ",
        ruta_destino,
        "."
      ),
      call. = FALSE
    )
  }

  conservar_temporal <- FALSE

  if (!file.exists(ruta_destino)) {
    stop(
      paste(
        "La escritura finalizó, pero el archivo",
        "TEX no existe en el destino."
      ),
      call. = FALSE
    )
  }

  informacion <- file.info(
    ruta_destino
  )

  list(
    formato = "physikos-saved-tex",
    version_formato = 1L,
    id = archivo_tex$id,
    plantilla = archivo_tex$plantilla,
    tipo = archivo_tex$tipo,
    nombre_archivo = archivo_tex$nombre_archivo,
    ruta = normalizePath(
      ruta_destino,
      winslash = "/",
      mustWork = TRUE
    ),
    bytes = unname(
      informacion$size
    )
  )
}

# nolint end