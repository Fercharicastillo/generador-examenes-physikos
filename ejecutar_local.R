carpeta_proyecto <- normalizePath(
  "C:/Users/Usuario/Desktop/generador_examenes",
  winslash = "/"
)

source(
  file.path(
    carpeta_proyecto,
    "R",
    "generar_examenes.R"
  ),
  encoding = "UTF-8"
)

estudiantes <- readLines(
  file.path(carpeta_proyecto, "estudiantes.txt"),
  encoding = "UTF-8",
  warn = FALSE
)

estudiantes <- trimws(estudiantes)
estudiantes <- estudiantes[nzchar(estudiantes)]

resultado <- generar_examenes(
  plantilla = 1,
  estudiantes = estudiantes,
  incluir_soluciones = TRUE,
  carpeta_salida = file.path(
    carpeta_proyecto,
    "resultados"
  ),
  semilla = 20260804,
  carpeta_proyecto = carpeta_proyecto
)

print(resultado)
