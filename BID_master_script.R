#### Script BID.  --------------------------------------------------------------
# Proyecto: BID Cohesion
"Este script crea las alertas de auditoría y exporta los resultados a unos
google sheets que alimentan un looker"

# Autor: Julián López - Equilibrium SDC


### Script Maestro para BID github actions ###

rm(list = ls())  # Limpia el entorno

# Instalar y cargar paquetes necesarios
if (!require("pacman")) install.packages("pacman")
library(pacman)
p_load(
  dplyr,
  tidyr,
  httr,
  jsonlite,
  googledrive,
  googlesheets4,
  writexl,
  haven,
  stringr,
  labelled,
  lubridate,
  gtsummary,
  dotenv,
  lubridate
)

# Configurar el directorio base del proyecto
project_path <- getwd()  # Ruta del proyecto en GitHub Actions
message("Directorio base: ", project_path)

# Configurar las credenciales
if (Sys.getenv("GITHUB_ACTIONS") == "true") {
  # En GitHub Actions, cargamos los secretos directamente
  message("Cargando credenciales desde secretos en GitHub Actions...")
  server <- Sys.getenv("SERVIDOR")
  username <- Sys.getenv("USERNAME")
  password <- Sys.getenv("PASSWORD")
  email <- Sys.getenv("EMAIL")
  formid <- Sys.getenv("FORMID")
  creds <- Sys.getenv("GOOGLE_SHEETS_CREDENTIALS")
  
  # Crear un archivo temporal para las credenciales de Google
  temp_creds_file <- tempfile(fileext = ".json")
  writeLines(creds, temp_creds_file)
} else {
  # Localmente, cargamos las credenciales desde el archivo .env
  if (file.exists(".env")) {
    message("Archivo .env encontrado en: ", project_path)
    dotenv::load_dot_env(".env")
    server <- Sys.getenv("SERVIDOR")
    username <- Sys.getenv("USERNAME")
    password <- Sys.getenv("PASSWORD")
    email <- Sys.getenv("EMAIL")
    formid <- Sys.getenv("FORMID")
    temp_creds_file <- Sys.getenv("GOOGLE_SHEETS_CREDENTIALS")
  } else {
    stop("El archivo .env no se encuentra. Asegúrate de haberlo configurado correctamente.")
  }
}

# Validar que todas las credenciales estén cargadas
if (any(is.na(c(server,username, password, email, formid, temp_creds_file)))) {
  stop("Faltan credenciales requeridas. Verifica la configuración.")
}

# Confirmar que las credenciales se cargaron correctamente
message("Credenciales cargadas correctamente:")
message("- Usuario Kobo: ", username)
message("- Email Google Sheets: ", email)

# Función para cargar scripts secundarios
load_script <- function(script_name) {
  script_path <- file.path(project_path, "scripts", script_name)
  if (file.exists(script_path)) {
    message("Ejecutando script: ", script_name)
    source(script_path)
  } else {
    stop(paste("No se encontró el script:", script_path))
  }
}

# Ejecutar scripts secundarios en orden
load_script("1_Import_data_BID.R")   # Importar datos desde survey
load_script("2_Alertas_BID.R")           # Crear alertas
load_script("3_Export_data_BID.R")  # Exportar los datos           
load_script("Procesamiento_juegos.R")
# Confirmación de finalización
message("Pipeline completado exitosamente.")



