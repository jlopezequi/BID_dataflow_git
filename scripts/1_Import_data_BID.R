### Importar datos desde survey ###


# Asegurarse de que las credenciales necesarias estén disponibles
if (exists("username") && exists("password") && exists("server") && exists("formid")) {
  message("Credenciales de Survey cargadas correctamente.")
} else {
  stop("No se encontraron las credenciales de Survey. Asegúrate de cargarlas desde el script maestro.")
}


# Download de API ----------------------------------------------------------

## Conect to SurveyCTO ----------------------------------------------------------------

servidor_BID <-  server
username_BID <-  email
password_BID <- password
formid_bid <- formid

API <- paste0('https://',servidor_BID,'.surveycto.com/api/v2/forms/data/wide/json/',formid_bid,'?date=0')


## Import data -------------------------------------------------------------

max_attempts <- 10
attempt <- 1

repeat {
  # Llamada a la API
  dataset_json <- POST(
    url = API,
    config = authenticate(username_BID, password_BID),
    add_headers("Content-Type: application/json"),
    encode = 'json'
  )
  
  # Convertir JSON a data frame
  df <- jsonlite::fromJSON(rawToChar(dataset_json$content), flatten = TRUE)
  
  # Si df es un data frame válido, salir del ciclo
  if (is.data.frame(df)) break
  
  # Si se alcanzó el número máximo de intentos, lanzar error y salir
  if (attempt >= max_attempts) {
    stop("Se alcanzó el número máximo de intentos sin obtener un data frame válido.")
  }
  
  # Esperar antes de reintentar
  Sys.sleep(300)
  attempt <- attempt + 1
}


# Modificación del Dataset  ---------------------------------------------------------------
# Creación de variables con saltos si no existen


vars_needed <- c(
  "verificacion_estudiante",
  "name_corr",
  "colegio_corr",
  "sede_corr",
  "curso_corr",
  "jornada_corr",
  "edad_corr",
  "student_country_o",
  "student_city",
  "student_mother_country_o",
  "student_father_country_o",
  "student_mother_age",
  "student_father_age",
  "games_1_2_1",
  "games_2_2_1",
  "games_2_2_2",
  "games_1_2_2",
  "games_3_a",
  "games_3_b",
  "mate_1_select",
  "mate_2_select",
  "mate_3_select",
  "mate_4_select",
  "mate_1_select_tec",
  "mate_2_select_tec",
  "mate_3_select_tec",
  "mate_4_select_tec",
  "feedback_dis",
  paste("which_data",1:6,sep="_"),
  "which_data",
  "name_corr",
  "colegio_corr",
  "sede_corr",
  "curso_corr",
  "jornada_corr",
  "edad_corr",
  "disc_pull"
)


for (v in vars_needed) {
  if (!(v %in% names(df))) {
    df[[v]] <- rep(NA, nrow(df))
  }
}


df <- df %>%
  mutate(
    fecha_limpia = parse_date_time(endtime, orders = "b d, Y I:M:S p")
  ) %>%
  filter(fecha_limpia >= ymd("2025-05-16"))

# Guardado en Diferentes Formato ---------------------------------------------------------------
dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)
# Asegurarse de que los nombres de columnas sean válidos
names(df) <- make.names(names(df), unique = TRUE)

## Rdata
save(
  df,
  file = paste0('data/raw/bid_cohesion_pruebas', Sys.Date(), '.RData')
)


## Excel
writexl::write_xlsx(
  df,
  path = paste0('data/raw/bid_cohesion_pruebas', Sys.Date(), '.xlsx'),
  col_names = T
)

## CSV
write.csv(
  df,
  file = paste0('data/raw/bid_cohesion_pruebas', Sys.Date(),'.csv'),
  sep = ','
)

