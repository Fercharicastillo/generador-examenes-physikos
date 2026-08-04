library(knitr)

generar_examenes <- function(
    plantilla,
    estudiantes,
    incluir_soluciones = TRUE,
    carpeta_salida = "resultados",
    semilla = 20260804,
    carpeta_proyecto = "."
) {
  # Trabajar temporalmente dentro del proyecto
  carpeta_anterior <- getwd()
  on.exit(setwd(carpeta_anterior), add = TRUE)
  
  setwd(carpeta_proyecto)
  
  archivo_prueba <- paste0("prueba", plantilla, ".Rnw")
  archivo_solucion <- paste0("solucion", plantilla, ".Rnw")
  
  if (!file.exists(archivo_prueba)) {
    stop("No se encontró la plantilla: ", archivo_prueba)
  }
  
  if (incluir_soluciones && !file.exists(archivo_solucion)) {
    stop("No se encontró el solucionario: ", archivo_solucion)
  }
  
  # Crear carpetas de salida
  carpeta_examenes <- file.path(carpeta_salida, "examenes")
  carpeta_soluciones <- file.path(carpeta_salida, "soluciones")
  
  dir.create(
    carpeta_examenes,
    recursive = TRUE,
    showWarnings = FALSE
  )
  
  if (incluir_soluciones) {
    dir.create(
      carpeta_soluciones,
      recursive = TRUE,
      showWarnings = FALSE
    )
  }
  
  registro <- vector("list", length(estudiantes))
  
  for (i in seq_along(estudiantes)) {
    nombre_actual <- trimws(estudiantes[i])
    semilla_actual <- semilla + i - 1
    
    # Evitar caracteres problemáticos en el nombre del archivo
    nombre_archivo <- gsub(
      '[<>:"/\\\\|?*]',
      "_",
      nombre_actual
    )
    
    message(
      "Generando evaluación ",
      i,
      " de ",
      length(estudiantes),
      ": ",
      nombre_actual
    )
    
    # Cada estudiante obtiene su propio entorno
    entorno_estudiante <- new.env(parent = globalenv())
    
    entorno_estudiante$nombre_actual <- nombre_actual
    entorno_estudiante$semilla_actual <- semilla_actual
    
    # La prueba siempre puede reproducirse usando esta semilla
    set.seed(semilla_actual)
    
    archivo_tex_prueba <- paste0("prueba", plantilla, ".tex")
    archivo_pdf_prueba <- paste0("prueba", plantilla, ".pdf")
    
    knit(
      input = archivo_prueba,
      output = archivo_tex_prueba,
      envir = entorno_estudiante,
      quiet = TRUE
    )
    
    tools::texi2pdf(
      file = archivo_tex_prueba,
      clean = TRUE
    )
    
    destino_prueba <- file.path(
      carpeta_examenes,
      paste0(nombre_archivo, ".pdf")
    )
    
    if (!file.rename(archivo_pdf_prueba, destino_prueba)) {
      stop(
        "No se pudo mover la prueba de ",
        nombre_actual
      )
    }
    
    destino_solucion <- NA_character_
    
    if (incluir_soluciones) {
      archivo_tex_solucion <- paste0(
        "solucion",
        plantilla,
        ".tex"
      )
      
      archivo_pdf_solucion <- paste0(
        "solucion",
        plantilla,
        ".pdf"
      )
      
      # Se usa el mismo entorno de la prueba.
      # La solución puede acceder a las variables ya generadas.
      knit(
        input = archivo_solucion,
        output = archivo_tex_solucion,
        envir = entorno_estudiante,
        quiet = TRUE
      )
      
      tools::texi2pdf(
        file = archivo_tex_solucion,
        clean = TRUE
      )
      
      destino_solucion <- file.path(
        carpeta_soluciones,
        paste0(nombre_archivo, "_solucion.pdf")
      )
      
      if (!file.rename(archivo_pdf_solucion, destino_solucion)) {
        stop(
          "No se pudo mover la solución de ",
          nombre_actual
        )
      }
    }
    
    registro[[i]] <- data.frame(
      estudiante = nombre_actual,
      semilla = semilla_actual,
      examen = destino_prueba,
      solucion = destino_solucion,
      stringsAsFactors = FALSE
    )
  }
  
  registro <- do.call(rbind, registro)
  
  archivo_registro <- file.path(
    carpeta_salida,
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
  message("Resultados: ", normalizePath(carpeta_salida))
  
  invisible(registro)
}