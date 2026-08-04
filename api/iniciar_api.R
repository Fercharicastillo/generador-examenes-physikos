library(plumber)

argumentos <- commandArgs(trailingOnly = FALSE)
argumento_archivo <- grep(
  "^--file=",
  argumentos,
  value = TRUE
)

if (length(argumento_archivo) > 0) {
  archivo_inicio <- sub("^--file=", "", argumento_archivo[[1]])
  carpeta_api <- dirname(
    normalizePath(archivo_inicio, winslash = "/", mustWork = TRUE)
  )
} else {
  carpeta_actual <- normalizePath(getwd(), winslash = "/")
  carpeta_api <- if (basename(carpeta_actual) == "api") {
    carpeta_actual
  } else {
    file.path(carpeta_actual, "api")
  }

  carpeta_api <- normalizePath(
    carpeta_api,
    winslash = "/",
    mustWork = TRUE
  )
}

carpeta_proyecto <- dirname(carpeta_api)
Sys.setenv(PHYSIKOS_PROJECT_DIR = carpeta_proyecto)

host <- Sys.getenv("HOST", unset = "0.0.0.0")
puerto <- suppressWarnings(
  as.integer(Sys.getenv("PORT", unset = "8000"))
)

if (is.na(puerto)) {
  stop("La variable PORT debe contener un número entero.")
}

mostrar_swagger <- tolower(
  Sys.getenv("PLUMBER_SWAGGER", unset = "true")
) %in% c("1", "true", "yes", "si")

api <- plumber::plumb(
  file.path(carpeta_api, "plumber.R")
)

api$run(
  host = host,
  port = puerto,
  docs = if (mostrar_swagger) "swagger" else FALSE
)
