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
  "student_fifth_o",
  "student_country_o",
  "student_mother_country_o",
  "student_father_country_o",
  "student_mother_age",
  "student_father_age",
  "games_3_a",
  "games_3_b",
  "mate_1",
  "mate_1_name",
  "mate_1_lastname",
  "mate_2",
  "mate_2_name",
  "mate_2_lastname",
  "mate_3",
  "mate_3_name",
  "mate_3_lastname",
  "mate_4",
  "mate_4_name",
  "mate_4_lastname",
  "assent",
  "gender","stamp_pers_1", "stamp_pers_2", "time_pers_sec",
  "stamp_impul_1", "stamp_impul_2", "time_impul_sec",
  "stamp_preo_1", "stamp_preo_2", "time_preo_sec",
  "stamp_preju_1", "stamp_preju_2", "time_preju_sec",
  "stamp_norms_1", "stamp_norms_2", "time_norms_sec",
  "stamp_cohe_1", "stamp_cohe_2", "time_cohe_sec",
  "stamp_raven_1", "stamp_raven_2", "time_raven_sec",
  "stamp_ojos_1", "stamp_ojos_2", "time_ojos_sec",
  "stamp_nomi_1", "stamp_nomi_2", "time_nomi_sec",
  "stamp_stud_cohe_1", "stamp_stud_cohe_2", "time_stud_cohe_sec",
  "stamp_games_1", "stamp_games_2", "time_games_sec",
  "stamp_ubi_1", "stamp_ubi_2", "time_ubi_sec"
)

for (v in vars_needed) {
  if (!(v %in% names(df))) {
    df[[v]] <- rep(NA, nrow(df))
  }
}


df<- df %>%
  mutate(gender = if_else(
    !is.na(sexo),sexo,gender
  ))

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

