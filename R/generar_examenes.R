library(knitr)

generar_examenes <- function(
    plantilla,
    estudiantes,
    incluir_soluciones = TRUE,
    carpeta_salida = "resultados",
    semilla = 20260804,
    carpeta_proyecto = ".",
    directorio_trabajo = NULL,
    actualizar_progreso = NULL
) {
  carpeta_proyecto <- normalizePath(
    carpeta_proyecto,
    winslash = "/",
    mustWork = TRUE
  )

  archivo_prueba <- file.path(
    carpeta_proyecto,
    paste0("prueba", plantilla, ".Rnw")
  )
  archivo_solucion <- file.path(
    carpeta_proyecto,
    paste0("solucion", plantilla, ".Rnw")
  )

  if (!file.exists(archivo_prueba)) {
    stop("No se encontró la plantilla: ", archivo_prueba)
  }
  if (incluir_soluciones && !file.exists(archivo_solucion)) {
    stop("No se encontró el solucionario: ", archivo_solucion)
  }

  if (is.null(directorio_trabajo)) {
    carpeta_resultados <- carpeta_salida
    carpeta_temporales <- tempfile("generacion_")
    eliminar_temporales <- TRUE
  } else {
    directorio_trabajo <- normalizePath(
      directorio_trabajo,
      winslash = "/",
      mustWork = TRUE
    )
    carpeta_resultados <- file.path(directorio_trabajo, "resultados")
    carpeta_temporales <- file.path(directorio_trabajo, "temporales")
    eliminar_temporales <- FALSE
  }

  dir.create(carpeta_temporales, recursive = TRUE, showWarnings = FALSE)
  if (eliminar_temporales) {
    on.exit(
      unlink(carpeta_temporales, recursive = TRUE, force = TRUE),
      add = TRUE
    )
  }

  carpeta_examenes <- file.path(carpeta_resultados, "examenes")
  carpeta_soluciones <- file.path(carpeta_resultados, "soluciones")
  dir.create(carpeta_examenes, recursive = TRUE, showWarnings = FALSE)

  if (incluir_soluciones) {
    dir.create(carpeta_soluciones, recursive = TRUE, showWarnings = FALSE)
  }

  informar_progreso <- function(actual, mensaje) {
    if (!is.function(actualizar_progreso)) return(invisible(NULL))

    progreso <- if (length(estudiantes) == 0) {
      0L
    } else {
      as.integer(round(actual / length(estudiantes) * 90))
    }

    actualizar_progreso(
      estado = "generando_examenes",
      actual = actual,
      total = length(estudiantes),
      progreso = progreso,
      mensaje = mensaje
    )

    invisible(NULL)
  }

  copiar_recursos <- function(destino) {
    origen <- file.path(carpeta_proyecto, "recursos")
    if (!dir.exists(origen)) return(invisible(NULL))

    destino_recursos <- file.path(destino, "recursos")
    dir.create(destino_recursos, recursive = TRUE, showWarnings = FALSE)
    archivos <- list.files(origen, full.names = TRUE)

    if (length(archivos) > 0) {
      copiados <- file.copy(
        archivos,
        destino_recursos,
        overwrite = TRUE,
        recursive = TRUE
      )
      if (!all(copiados)) stop("No se pudieron copiar todos los recursos.")
    }

    invisible(NULL)
  }

  mover_pdf <- function(origen, destino, descripcion) {
    if (!file.exists(origen)) {
      stop("La compilación no creó el PDF de ", descripcion, ".")
    }

    if (file.rename(origen, destino)) return(invisible(destino))

    if (!file.copy(origen, destino, overwrite = TRUE)) {
      stop("No se pudo mover el PDF de ", descripcion, ".")
    }

    unlink(origen, force = TRUE)
    invisible(destino)
  }

  compilar_tex <- function(archivo_tex) {
  carpeta_compilacion <- dirname(archivo_tex)
  nombre_tex <- basename(archivo_tex)

  callr::r(
    func = function(nombre_archivo) {
      tools::texi2pdf(
        file = nombre_archivo,
        clean = TRUE
      )
    },
    args = list(
      nombre_archivo = nombre_tex
    ),
    wd = carpeta_compilacion,
    show = TRUE
  )

  archivo_pdf <- sub(
    "\\.tex$",
    ".pdf",
    archivo_tex
  )

  if (!file.exists(archivo_pdf)) {
    stop(
      "La compilación terminó sin crear el PDF esperado: ",
      archivo_pdf
    )
  }

  invisible(archivo_pdf)
}

  registro <- vector("list", length(estudiantes))
  informar_progreso(0L, "Preparando la generación")

  for (i in seq_along(estudiantes)) {
    nombre_actual <- trimws(estudiantes[i])
    semilla_actual <- semilla + i - 1
    nombre_archivo <- gsub('[<>:"/\\\\|?*]', "_", nombre_actual)

    message(
      "Generando evaluación ", i, " de ", length(estudiantes),
      ": ", nombre_actual
    )

    entorno_estudiante <- new.env(parent = globalenv())
    entorno_estudiante$nombre_actual <- nombre_actual
    entorno_estudiante$semilla_actual <- semilla_actual
    set.seed(semilla_actual)

    carpeta_temporal_estudiante <- file.path(
      carpeta_temporales,
      sprintf("estudiante_%03d", i)
    )
    dir.create(
      carpeta_temporal_estudiante,
      recursive = TRUE,
      showWarnings = FALSE
    )
    copiar_recursos(carpeta_temporal_estudiante)

    archivo_tex_prueba <- file.path(
      carpeta_temporal_estudiante,
      paste0("prueba", plantilla, ".tex")
    )
    archivo_pdf_prueba <- file.path(
      carpeta_temporal_estudiante,
      paste0("prueba", plantilla, ".pdf")
    )

    knit(
      input = archivo_prueba,
      output = archivo_tex_prueba,
      envir = entorno_estudiante,
      quiet = TRUE
    )
    compilar_tex(archivo_tex_prueba)

    destino_prueba <- file.path(
      carpeta_examenes,
      paste0(nombre_archivo, ".pdf")
    )
    mover_pdf(archivo_pdf_prueba, destino_prueba, nombre_actual)

    destino_solucion <- NA_character_

    if (incluir_soluciones) {
      archivo_tex_solucion <- file.path(
        carpeta_temporal_estudiante,
        paste0("solucion", plantilla, ".tex")
      )
      archivo_pdf_solucion <- file.path(
        carpeta_temporal_estudiante,
        paste0("solucion", plantilla, ".pdf")
      )

      knit(
        input = archivo_solucion,
        output = archivo_tex_solucion,
        envir = entorno_estudiante,
        quiet = TRUE
      )
      compilar_tex(archivo_tex_solucion)

      destino_solucion <- file.path(
        carpeta_soluciones,
        paste0(nombre_archivo, "_solucion.pdf")
      )
      mover_pdf(
        archivo_pdf_solucion,
        destino_solucion,
        paste("la solución de", nombre_actual)
      )
    }

    registro[[i]] <- data.frame(
      estudiante = nombre_actual,
      semilla = semilla_actual,
      examen = destino_prueba,
      solucion = destino_solucion,
      stringsAsFactors = FALSE
    )

    informar_progreso(
      i,
      paste0(
        "Evaluación ", i, " de ", length(estudiantes), " generada"
      )
    )
  }

  registro <- do.call(rbind, registro)
  archivo_registro <- file.path(
    carpeta_resultados,
    "registro_generacion.csv"
  )

  write.csv(
    registro,
    archivo_registro,
    row.names = FALSE,
    fileEncoding = "UTF-8"
  )

  message("")
  message("Generación terminada.")
  message("Resultados: ", normalizePath(carpeta_resultados))

  invisible(registro)
}
