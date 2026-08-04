library(plumber)

carpeta_api <- normalizePath(
  "C:/Users/Usuario/Desktop/generador_examenes/api",
  winslash = "/"
)

api <- plumber::plumb(
  file.path(carpeta_api, "plumber.R")
)

api$run(
  host = "127.0.0.1",
  port = 8000,
  swagger = TRUE
)
