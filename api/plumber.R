library(plumber)

ruta_proyecto <- Sys.getenv("PHYSIKOS_PROJECT_DIR", unset = "")

if (!nzchar(ruta_proyecto)) {
  stop(
    "No se definió PHYSIKOS_PROJECT_DIR. Inicie la API con api/iniciar_api.R."
  )
}

CARPETA_PROYECTO <- normalizePath(
  ruta_proyecto,
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

source(
  file.path(
    CARPETA_PROYECTO,
    "R",
    "trabajos",
    "estado_trabajo.R"
  ),
  encoding = "UTF-8"
)

source(
  file.path(
    CARPETA_PROYECTO,
    "R",
    "trabajos",
    "crear_trabajo.R"
  ),
  encoding = "UTF-8"
)

source(
  file.path(
    CARPETA_PROYECTO,
    "R",
    "trabajos",
    "limpiar_trabajos.R"
  ),
  encoding = "UTF-8"
)

PROCESOS_TRABAJOS <- new.env(
  parent = emptyenv()
)

limpiar_procesos_finalizados <- function() {
  identificadores <- ls(
    envir = PROCESOS_TRABAJOS,
    all.names = TRUE
  )

  for (trabajo_id in identificadores) {
    proceso <- get(
      trabajo_id,
      envir = PROCESOS_TRABAJOS,
      inherits = FALSE
    )

    proceso_activo <- tryCatch(
      {
        proceso$is_alive()
      },
      error = function(error) {
        FALSE
      }
    )

    if (!isTRUE(proceso_activo)) {
      rm(
        list = trabajo_id,
        envir = PROCESOS_TRABAJOS
      )
    }
  }

  invisible(NULL)
}

`%||%` <- function(valor, predeterminado) {
  if (is.null(valor) || length(valor) == 0) {
    predeterminado
  } else {
    valor
  }
}

#* @apiTitle API del generador de exámenes Physikos
#* @apiDescription Generación de evaluaciones aleatorias y solucionarios

# Mostrar detalles internos únicamente cuando se habiliten explícitamente.
#* @plumber
function(pr) {
  modo_debug <- tolower(
    Sys.getenv("PLUMBER_DEBUG", unset = "false")
  ) %in% c("1", "true", "yes", "si")

  plumber::pr_set_debug(pr, modo_debug)
}

# Permitir únicamente los orígenes configurados.
#* @filter cors
function(req, res) {
  origenes_permitidos <- strsplit(
    Sys.getenv(
      "CORS_ALLOWED_ORIGINS",
      unset = paste(
        "http://localhost:5173",
        "http://127.0.0.1:5173",
        "https://fercharicastillo.github.io",
        sep = ","
      )
    ),
    split = ",",
    fixed = TRUE
  )

  origenes_permitidos <- trimws(origenes_permitidos[[1]])
  origen_solicitud <- req$HTTP_ORIGIN %||% ""

  if (origen_solicitud %in% origenes_permitidos) {
    res$setHeader("Access-Control-Allow-Origin", origen_solicitud)
    res$setHeader("Vary", "Origin")
  }
  
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

#* Crear una generación en segundo plano
#* @param entrada:object* Configuración de la evaluación
#* @parser json
#* @serializer unboxedJSON
#* @response 202 Trabajo creado correctamente
#* @response 400 Datos de entrada incorrectos
#* @response 404 Plantilla no encontrada
#* @response 500 No se pudo iniciar el trabajo
#* @post /generaciones
function(req, res, entrada = NULL) {
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

  if (length(estudiantes) > 10) {
    res$status <- 400

    return(list(
      error = "El máximo provisional es de 10 estudiantes."
    ))
  }

  plantilla <- suppressWarnings(
    as.integer(entrada$plantilla %||% 1)
  )

  semilla <- suppressWarnings(
    as.integer(entrada$semilla %||% 20260804)
  )

  incluir_soluciones <- if (
    is.null(entrada$incluir_soluciones)
  ) {
    TRUE
  } else {
    isTRUE(
      as.logical(entrada$incluir_soluciones[[1]])
    )
  }

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

  if (incluir_soluciones) {
    archivo_solucion <- file.path(
      CARPETA_PROYECTO,
      paste0("solucion", plantilla, ".Rnw")
    )

    if (!file.exists(archivo_solucion)) {
      res$status <- 404

      return(list(
        error = paste(
          "No existe la solución de la plantilla",
          plantilla
        )
      ))
    }
  }

  solicitud <- list(
    plantilla = plantilla,
    estudiantes = estudiantes,
    incluir_soluciones = incluir_soluciones,
    semilla = semilla
  )

try(
  limpiar_trabajos(
    carpeta_trabajos = CARPETA_TRABAJOS,
    retencion_minutos = 60
  ),
  silent = TRUE
)

limpiar_procesos_finalizados()

procesos_activos <- ls(
  envir = PROCESOS_TRABAJOS,
  all.names = TRUE
)

if (length(procesos_activos) >= 1) {
  res$status <- 429

  res$setHeader(
    "Retry-After",
    "10"
  )

  return(list(
    error = paste(
      "En este momento se está generando otra evaluación.",
      "Espera unos segundos e inténtalo nuevamente."
    )
  ))
}

  trabajo <- tryCatch(
    {
      crear_trabajo(
        carpeta_trabajos = CARPETA_TRABAJOS,
        solicitud = solicitud
      )
    },
    error = function(error) {
      error
    }
  )

  if (inherits(trabajo, "error")) {
    res$status <- 500

    return(list(
      error = "No se pudo crear el trabajo.",
      detalle = conditionMessage(trabajo)
    ))
  }

  archivo_salida <- file.path(
    trabajo$directorio,
    "proceso.log"
  )

  archivo_errores <- file.path(
    trabajo$directorio,
    "errores.log"
  )

  proceso <- tryCatch(
    {
      callr::r_bg(
        func = function(
            directorio_trabajo,
            carpeta_proyecto
        ) {
          source(
            file.path(
              carpeta_proyecto,
              "R",
              "trabajos",
              "estado_trabajo.R"
            ),
            encoding = "UTF-8"
          )

          source(
            file.path(
              carpeta_proyecto,
              "R",
              "generar_examenes.R"
            ),
            encoding = "UTF-8"
          )

          source(
            file.path(
              carpeta_proyecto,
              "R",
              "trabajos",
              "ejecutar_trabajo.R"
            ),
            encoding = "UTF-8"
          )

          ejecutar_trabajo(
            directorio_trabajo = directorio_trabajo,
            carpeta_proyecto = carpeta_proyecto
          )
        },
        args = list(
          directorio_trabajo = trabajo$directorio,
          carpeta_proyecto = CARPETA_PROYECTO
        ),
        wd = CARPETA_PROYECTO,
        stdout = archivo_salida,
        stderr = archivo_errores,
        supervise = FALSE
      )
    },
    error = function(error) {
      error
    }
  )

  if (inherits(proceso, "error")) {
    actualizar_estado_trabajo(
      directorio_trabajo = trabajo$directorio,
      estado = "fallido",
      mensaje = "No se pudo iniciar el proceso de generación",
      error = conditionMessage(proceso)
    )

    res$status <- 500

    return(list(
      error = "No se pudo iniciar el trabajo.",
      detalle = conditionMessage(proceso),
      trabajo_id = trabajo$trabajo_id
    ))
  }

  assign(
    trabajo$trabajo_id,
    proceso,
    envir = PROCESOS_TRABAJOS
  )

  res$status <- 202

  list(
    trabajo_id = trabajo$trabajo_id,
    estado = "pendiente",
    mensaje = "El trabajo fue recibido y se procesará en segundo plano."
  )
}

#* Consultar el estado de una generación
#* @param trabajo_id Identificador del trabajo
#* @serializer unboxedJSON
#* @response 200 Estado del trabajo
#* @response 400 Identificador incorrecto
#* @response 404 Trabajo no encontrado
#* @response 500 No se pudo leer el estado
#* @get /generaciones/<trabajo_id>
function(trabajo_id, res) {
  patron_id <- paste0(
    "^gen_",
    "[0-9]{8}_",
    "[0-9]{6}_",
    "[a-z0-9]{12}$"
  )

  if (!grepl(patron_id, trabajo_id)) {
    res$status <- 400

    return(list(
      error = "El identificador del trabajo no es válido."
    ))
  }

  directorio_trabajo <- file.path(
    CARPETA_TRABAJOS,
    trabajo_id
  )

  archivo_estado <- file.path(
    directorio_trabajo,
    "estado.json"
  )

  if (
    !dir.exists(directorio_trabajo) ||
    !file.exists(archivo_estado)
  ) {
    res$status <- 404

    return(list(
      error = "El trabajo solicitado no existe."
    ))
  }

  estado <- NULL
  error_lectura <- NULL

  # El worker puede estar reemplazando estado.json
  # justo cuando llega esta consulta.
  for (intento in seq_len(5)) {
    resultado_lectura <- tryCatch(
      {
        leer_estado_trabajo(
          directorio_trabajo
        )
      },
      error = function(error) {
        error
      }
    )

    if (!inherits(resultado_lectura, "error")) {
      estado <- resultado_lectura
      break
    }

    error_lectura <- resultado_lectura
    Sys.sleep(0.05)
  }

  if (is.null(estado)) {
    res$status <- 500

    return(list(
      error = "No se pudo leer el estado del trabajo.",
      detalle = conditionMessage(error_lectura)
    ))
  }

  # Liberar del registro los procesos que ya terminaron.
  if (
    exists(
      trabajo_id,
      envir = PROCESOS_TRABAJOS,
      inherits = FALSE
    )
  ) {
    proceso <- get(
      trabajo_id,
      envir = PROCESOS_TRABAJOS,
      inherits = FALSE
    )

    if (!proceso$is_alive()) {
      rm(
        list = trabajo_id,
        envir = PROCESOS_TRABAJOS
      )
    }
  }

  estado$descarga_disponible <- identical(
    estado$estado,
    "completado"
  )

  estado
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

#* Descargar los resultados de una generación
#* @param trabajo_id Identificador del trabajo
#* @serializer contentType list(type = "application/zip")
#* @response 200 Archivo ZIP
#* @response 400 Identificador incorrecto
#* @response 404 Trabajo o archivo no encontrado
#* @response 409 Trabajo todavía no completado
#* @response 500 No se pudo leer el estado
#* @get /generaciones/<trabajo_id>/descarga
function(trabajo_id, res) {
  patron_id <- paste0(
    "^gen_",
    "[0-9]{8}_",
    "[0-9]{6}_",
    "[a-z0-9]{12}$"
  )

  if (!grepl(patron_id, trabajo_id)) {
    res$status <- 400

    return(charToRaw(
      "El identificador del trabajo no es válido."
    ))
  }

  directorio_trabajo <- file.path(
    CARPETA_TRABAJOS,
    trabajo_id
  )

  if (!dir.exists(directorio_trabajo)) {
    res$status <- 404

    return(charToRaw(
      "El trabajo solicitado no existe."
    ))
  }

  estado <- tryCatch(
    {
      leer_estado_trabajo(
        directorio_trabajo
      )
    },
    error = function(error) {
      error
    }
  )

  if (inherits(estado, "error")) {
    res$status <- 500

    return(charToRaw(
      paste(
        "No se pudo leer el estado del trabajo:",
        conditionMessage(estado)
      )
    ))
  }

  if (!identical(estado$estado, "completado")) {
    res$status <- 409

    return(charToRaw(
      paste(
        "El trabajo todavía no está completado.",
        "Estado actual:",
        estado$estado
      )
    ))
  }

  archivo_zip <- file.path(
    directorio_trabajo,
    "resultados",
    paste0(trabajo_id, ".zip")
  )

  if (!file.exists(archivo_zip)) {
    res$status <- 404

    return(charToRaw(
      "El trabajo está completado, pero no se encontró el ZIP."
    ))
  }

  tamano_archivo <- file.info(
    archivo_zip
  )$size

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
    as.character(tamano_archivo)
  )

  res$setHeader(
    "Cache-Control",
    "private, no-store"
  )

  res$setHeader(
    "X-Content-Type-Options",
    "nosniff"
  )

  readBin(
    con = archivo_zip,
    what = "raw",
    n = tamano_archivo
  )
}