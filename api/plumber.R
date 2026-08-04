library(plumber)

CARPETA_PROYECTO <- normalizePath(
  "C:/Users/Usuario/Desktop/generador_examenes",
  winslash = "/",
  mustWork = TRUE
)

CARPETA_TRABAJOS <- file.path(
  CARPETA_PROYECTO,
  "trabajos"
)

dir.create(
  CARPETA_TRABAJOS,
  recursive = TRUE,
  showWarnings = FALSE
)

source(
  file.path(
    CARPETA_PROYECTO,
    "R",
    "generar_examenes.R"
  ),
  encoding = "UTF-8"
)

`%||%` <- function(valor, predeterminado) {
  if (is.null(valor) || length(valor) == 0) {
    predeterminado
  } else {
    valor
  }
}

#* @apiTitle API del generador de exámenes Physikos
#* @apiDescription Generación de evaluaciones aleatorias y solucionarios

#* Mostrar detalles de errores durante el desarrollo. BORRRAR AL PUBLICAR LA APP! 
#* @plumber
function(pr) {
  plumber::pr_set_debug(pr, TRUE)
}

# Permitir peticiones desde React durante el desarrollo
#* @filter cors
function(req, res) {
  res$setHeader(
    "Access-Control-Allow-Origin",
    "http://localhost:5173"
  )
  
  res$setHeader(
    "Access-Control-Allow-Methods",
    "GET, POST, OPTIONS"
  )
  
  res$setHeader(
    "Access-Control-Allow-Headers",
    "Content-Type"
  )
  
  if (req$REQUEST_METHOD == "OPTIONS") {
    res$status <- 200
    return(list())
  }
  
  plumber::forward()
}

#* Verificar que la API está funcionando
#* @get /salud
function() {
  list(
    estado = "ok",
    servicio = "Generador de exámenes Physikos",
    fecha = format(
      Sys.time(),
      "%Y-%m-%d %H:%M:%S"
    )
  )
}

#* Mostrar las plantillas disponibles
#* @get /plantillas
function() {
  archivos <- list.files(
    CARPETA_PROYECTO,
    pattern = "^prueba[0-9]+\\.Rnw$"
  )
  
  numeros <- sub(
    "^prueba([0-9]+)\\.Rnw$",
    "\\1",
    archivos
  )
  
  plantillas <- lapply(
    seq_along(archivos),
    function(i) {
      numero <- as.integer(numeros[i])
      
      list(
        id = numero,
        nombre = paste("Plantilla", numero),
        archivo = archivos[i],
        tiene_solucion = file.exists(
          file.path(
            CARPETA_PROYECTO,
            paste0("solucion", numero, ".Rnw")
          )
        )
      )
    }
  )
  
  list(plantillas = plantillas)
}

#* Generar evaluaciones
#* Recibe estudiantes y opciones de generación
#* @param entrada:object* Configuración de la evaluación
#* @parser json
#* @serializer unboxedJSON
#* @response 200 Evaluaciones generadas correctamente
#* @response 400 Datos de entrada incorrectos
#* @response 404 Plantilla no encontrada
#* @response 500 Error durante la generación
#* @post /generar
function(req, res, entrada = NULL) {
  # Dependiendo de la versión de Plumber, el cuerpo puede
  # llegar como `entrada` o estar disponible en req$body.
  if (is.null(entrada)) {
    entrada <- req$body
  }
  
  if (is.null(entrada) || length(entrada) == 0) {
    res$status <- 400
    
    return(list(
      error = "La petición no contiene datos."
    ))
  }
  
  estudiantes <- unlist(
    entrada$estudiantes,
    use.names = FALSE
  )
  
  estudiantes <- trimws(
    as.character(estudiantes)
  )
  
  estudiantes <- estudiantes[nzchar(estudiantes)]
  
  if (length(estudiantes) == 0) {
    res$status <- 400
    
    return(list(
      error = "Debe proporcionar al menos un estudiante."
    ))
  }
  
  if (length(estudiantes) > 100) {
    res$status <- 400
    
    return(list(
      error = "El máximo provisional es de 100 estudiantes."
    ))
  }
  
  plantilla <- as.integer(
    entrada$plantilla %||% 1
  )
  
  incluir_soluciones <- if (
    is.null(entrada$incluir_soluciones)
  ) {
    TRUE
  } else {
    as.logical(entrada$incluir_soluciones[[1]])
  }
  
  semilla <- as.integer(
    entrada$semilla %||% 20260804
  )
  
  if (
    length(plantilla) != 1 ||
    is.na(plantilla) ||
    plantilla < 1
  ) {
    res$status <- 400
    
    return(list(
      error = "La plantilla no es válida."
    ))
  }
  
  if (
    length(semilla) != 1 ||
    is.na(semilla)
  ) {
    res$status <- 400
    
    return(list(
      error = "La semilla no es válida."
    ))
  }
  
  archivo_plantilla <- file.path(
    CARPETA_PROYECTO,
    paste0("prueba", plantilla, ".Rnw")
  )
  
  if (!file.exists(archivo_plantilla)) {
    res$status <- 404
    
    return(list(
      error = paste(
        "No existe la plantilla",
        plantilla
      )
    ))
  }
  
  trabajo_id <- paste0(
    "examen_",
    format(Sys.time(), "%Y%m%d_%H%M%S"),
    "_",
    sprintf("%04d", sample.int(9999, 1))
  )
  
  carpeta_trabajo <- file.path(
    CARPETA_TRABAJOS,
    trabajo_id
  )
  
  dir.create(
    carpeta_trabajo,
    recursive = TRUE,
    showWarnings = FALSE
  )
  
  resultado <- tryCatch(
    {
      generar_examenes(
        plantilla = plantilla,
        estudiantes = estudiantes,
        incluir_soluciones = incluir_soluciones,
        carpeta_salida = carpeta_trabajo,
        semilla = semilla,
        carpeta_proyecto = CARPETA_PROYECTO
      )
    },
    error = function(error) {
      error
    }
  )
  
  if (inherits(resultado, "error")) {
    res$status <- 500
    
    return(list(
      error = "No se pudieron generar los exámenes.",
      detalle = conditionMessage(resultado),
      trabajo_id = trabajo_id
    ))
  }
  
  # Crear ZIP con los exámenes, soluciones y registro
  archivo_zip <- file.path(
    CARPETA_TRABAJOS,
    paste0(trabajo_id, ".zip")
  )
  
  resultado_zip <- tryCatch(
    {
      zip::zipr(
        zipfile = archivo_zip,
        files = ".",
        root = carpeta_trabajo,
        include_directories = TRUE
      )
      
      TRUE
    },
    error = function(error) {
      error
    }
  )
  
  if (inherits(resultado_zip, "error")) {
    res$status <- 500
    
    return(list(
      error = "Los PDF se generaron, pero no se pudo crear el ZIP.",
      detalle = conditionMessage(resultado_zip),
      trabajo_id = trabajo_id
    ))
  }
  
  if (!file.exists(archivo_zip)) {
    res$status <- 500
    
    return(list(
      error = "La operación terminó sin crear el archivo ZIP.",
      trabajo_id = trabajo_id
    ))
  }
  
  
  list(
    estado = "completado",
    trabajo_id = trabajo_id,
    estudiantes = length(estudiantes),
    examenes_generados = nrow(resultado),
    soluciones_incluidas = incluir_soluciones,
    semilla_inicial = semilla,
    descarga = paste0(
      "/descargar/",
      trabajo_id
    ),
    nombre_archivo = paste0(
      trabajo_id,
      ".zip"
    )
  )
}

#* Descargar un trabajo generado
#* @param trabajo_id Identificador completo del trabajo
#* @get /descargar/<trabajo_id>
#* @serializer contentType list(type = "application/zip")
#* @response 200 Archivo ZIP
#* @response 400 Identificador incorrecto
#* @response 404 Archivo no encontrado
function(trabajo_id, res) {
  patron_id <- paste0(
    "^examen_",
    "[0-9]{8}_",
    "[0-9]{6}_",
    "[0-9]{4}$"
  )
  
  if (!grepl(patron_id, trabajo_id)) {
    res$status <- 400
    
    return(charToRaw(
      "Identificador de trabajo incorrecto."
    ))
  }
  
  archivo_zip <- file.path(
    CARPETA_TRABAJOS,
    paste0(trabajo_id, ".zip")
  )
  
  if (!file.exists(archivo_zip)) {
    res$status <- 404
    
    return(charToRaw(
      "El archivo solicitado no existe."
    ))
  }
  
  res$setHeader(
    "Content-Disposition",
    paste0(
      'attachment; filename="',
      trabajo_id,
      '.zip"'
    )
  )
  
  res$setHeader(
    "Content-Length",
    as.character(file.info(archivo_zip)$size)
  )
  
  readBin(
    con = archivo_zip,
    what = "raw",
    n = file.info(archivo_zip)$size
  )
}