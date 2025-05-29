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
  "disc_pull",
  "colegio_str",
  "sede_str"
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

# # Unificar variables

df <- df %>%
  mutate(
    # Reemplazar strings vacíos en columnas "pull" por NA
    across(contains("pull"), ~if_else(str_squish(.) == "", NA_character_, .)),
    
    # Limpiar nombres
    name1 = na_if(str_squish(student_name1), ""),
    name2 = na_if(str_squish(student_name2), ""),
    name3 = na_if(str_squish(student_name3), ""),
    name4 = na_if(str_squish(student_name4), ""),
    
    # Convertir "9999" en NA explícitamente
    name2 = if_else(name2 == "9999", NA_character_, name2),
    name4 = if_else(name4 == "9999", NA_character_, name4),
    
    # Construcción condicional del nombre completo
    nombre_concatenado = case_when(
      is.na(name2) & !is.na(name3) & !is.na(name4) ~ str_c(name1, name3, name4, sep = " "),
      is.na(name2) & !is.na(name3) & is.na(name4) ~ str_c(name1, name3, sep = " "),
      is.na(name2) & is.na(name3) & !is.na(name4) ~ str_c(name1, name4, sep = " "),
      is.na(name2) & is.na(name3) & is.na(name4) ~ name1,
      !is.na(name2) & !is.na(name3) & is.na(name4) ~ str_c(name1, name2, name3, sep = " "),
      !is.na(name2) & is.na(name3) & !is.na(name4) ~ str_c(name1, name2, name4, sep = " "),
      !is.na(name2) & is.na(name3) & is.na(name4) ~ str_c(name1, name2, sep = " "),
      TRUE ~ str_c(name1, name2, name3, name4, sep = " ")
    ),
    
    # Limpiar espacios finales
    nombre_concatenado = str_squish(nombre_concatenado),
    
    # Consolidar variables de juegos
    games_1_1 = coalesce(games_1_1_1, games_1_1_2),
    games_1_2 = coalesce(games_1_2_1, games_1_2_2),
    games_1_3 = coalesce(games_1_3_1, games_1_3_2),
    games_1_4 = coalesce(games_1_4_1, games_1_4_2),
    games_1_5 = coalesce(games_1_5_1, games_1_5_2),
    games_1_6 = coalesce(games_1_6_1, games_1_6_2),
    games_2_1 = coalesce(games_2_1_1, games_2_1_2),
    games_2_2 = coalesce(games_2_2_1, games_2_2_2),
    games_2_3 = coalesce(games_2_3_1, games_2_3_2),
    games_2_4 = coalesce(games_2_4_1, games_2_4_2),
    games_2_5 = coalesce(games_2_5_1, games_2_5_2),
    games_2_6 = coalesce(games_2_6_1, games_2_6_2),
    age_final = coalesce(edad_pull,edad_corr,student_age),
    gender_final = coalesce(genero_pull,gender),
    gender_final = case_when(
      gender_final == "F" ~ "2",
      gender_final == "M" ~ "1",
      TRUE ~ gender_final),
    
    # Nombre final priorizando nombre_pull sobre el concatenado
    name_final = str_to_upper(coalesce(nombre_pull, nombre_concatenado)),
    name_final = if_else(str_squish(name_final) == "", NA_character_, name_final)
  )

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

