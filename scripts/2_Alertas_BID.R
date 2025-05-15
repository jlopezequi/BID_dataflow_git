#### Alertas ####

# Crear duración

alertas <- df %>% 
  mutate(duration_minutes = round((as.numeric(duration)/60),2))

# Flag duración
dev_estandar <- alertas %>%
  filter(duration_minutes <= 100) %>%
  summarise(sd_duracion = sd(duration_minutes),
            median_duracion = mean(duration_minutes)) %>%
  pull(sd_duracion)

median_duration <- alertas %>%
  filter(duration_minutes <= 100) %>%
  summarise(sd_duracion = sd(duration_minutes),
            median_duracion = median(duration_minutes)) %>%
  pull(median_duracion)



alertas <- alertas %>%
  mutate(
    flag_duration_mas = if_else(
      ((duration_minutes - median_duration) / dev_estandar > 3), 1, 0, missing = 0
    ),
    flag_duration_menos = if_else(
      ((duration_minutes - median_duration) / dev_estandar < -3), 1, 0, missing = 0
    )
  )

alertas <- alertas %>%
  mutate(flag_duration_mas = if_else(duration_minutes >= 130,0,flag_duration_mas),
         flag_duration_menos = if_else(duration_minutes <= 0,0,flag_duration_menos))

#### Validación de saltos #####


alertas <- alertas %>%
  mutate(
    s_student_fifth_o = if_else(!is.na(student_fifth_o) & as.numeric(student_fifth_l) != 88,1,0),
    s_student_country_o = if_else(!is.na(student_country_o) & as.numeric(student_country) != 88,1,0),
    s_student_mother_country_o = if_else(!is.na(student_mother_country_o) & as.numeric(student_mother_country) != 88,1,0),
    s_student_father_country_o = if_else(!is.na(student_father_country_o) & as.numeric(student_father_country) != 88,1,0),
    s_student_mother_age = if_else(!is.na(student_mother_age) & as.numeric(student_mother_country) == 66,1,0),
    s_student_father_age = if_else(!is.na(student_father_age) & as.numeric(student_father_country) == 66,1,0),
    s_games_3_a = if_else(!is.na(games_3_a) & as.numeric(random) != 1,1,0),
    s_games_3_b = if_else(!is.na(games_3_b) & as.numeric(random) != 2,1,0),
    s_mate_1_name = if_else(!is.na(mate_1_name) & as.numeric(ubi_3) != 1,1,0),
    s_mate_1_lastname = if_else(!is.na(mate_1_lastname) & as.numeric(ubi_3) != 1,1,0),
    s_mate_2_name = if_else(!is.na(mate_2_name) & as.numeric(ubi_4) != 1,1,0),
    s_mate_2_lastname = if_else(!is.na(mate_2_lastname) & as.numeric(ubi_4) != 1,1,0),
    s_mate_3_name = if_else(!is.na(mate_3_name) & as.numeric(ubi_5) != 1,1,0),
    s_mate_3_lastname = if_else(!is.na(mate_3_lastname) & as.numeric(ubi_5) != 1,1,0),
    s_mate_4_name = if_else(!is.na(mate_4_name) & as.numeric(ubi_6) != 1,1,0),
    s_mate_4_lastname = if_else(!is.na(mate_4_lastname) & as.numeric(ubi_6) != 1,1,0))

## Sumar total de saltos irregulares    

variables_salto <- names(alertas %>%
                           select(matches("^s_")))

alertas <- alertas %>%
  mutate(
    total_saltos = rowSums(alertas[,variables_salto], na.rm = T)
  )


#### Validación de preguntas obligatorias (missings) #####  


vars <- c(
  "student_name1", "student_name2", "student_name3", "student_name4",
  "student_school", "student_shift", "student_fifth_l", "student_birth",
  "student_birth1", "student_birth2", "student_birth3", "student_age",
  "student_country", "student_city", "student_mother_country", "student_father_country",
  "perspective_taking", "friends_problems", "understand_others", "understand_feelings",
  "talking_interrupt", "impulsive_do_now", "impulsive_do", "impulsive_talk",
  "thinking_first", "hyperactive", "no_move_dif", "impulsive_decisions",
  "impulsive_responses", "empathy_injustice", "empathy_defense", "empathy_feelings",
  "empathy_sorry", "empathy_good_hearth", "migrant_friends", "disability_fiends",
  "race_play", "migrant_smart", "migrant_equal", "religion_respect", "migrant_aggressive",
  "mates_make_fun", "mates_talk_behind", "mates_fight", "mates_fun_migrant",
  "mates_fun_indigena", "mates_nice", "mates_beat_migrant", "mates_beat_race",
  "mates_protect", "mates_scare_you", "mates_fun_you", "mates_hit_you",
  "mates_out_scare_you", "mates_out_fun_you", "mates_out_hit_you",
  "raven_ej1", "raven_ej2", "raven_ej3", paste0("raven_", 1:48),
  "eyes_test_ej", paste0("eyes_test_", 1:36),
  "friend_1_name", "friend_1_lastname", "friend_2_name", "friend_2_lastname",
  "friend_3_name", "friend_3_lastname", "emotional_1_name", "emotional_1_lastname",
  "emotional_2_name", "emotional_2_lastname", "emotional_3_name", "emotional_3_lastname",
  "academic_1_name", "academic_1_lastname", "academic_2_name", "academic_2_lastname",
  "academic_3_name", "academic_3_lastname",
  "cohes_help", "cohes_close_knit", "cohes_trust", "cohes_get_along", "cohes_values",
  paste0("games_1_", 1:6), paste0("games_2_", 1:6),
  paste0("ubi_", 1:6),"assent","gender")

# Crear las variables dummy de missing
alertas <- alertas %>%
  mutate(across(all_of(vars), ~ if_else(is.na(.x), 1, 0), .names = "m_{.col}"),
         m_mate_1_name = if_else(is.na(mate_1_name) & as.numeric(ubi_3) == 1,1,0),
         m_mate_1_lastname = if_else(is.na(mate_1_lastname) & as.numeric(ubi_3) == 1,1,0),
         m_mate_2_name = if_else(is.na(mate_2_name) & as.numeric(ubi_4) == 1,1,0),
         m_mate_2_lastname = if_else(is.na(mate_2_lastname) & as.numeric(ubi_4) == 1,1,0),
         m_mate_3_name = if_else(is.na(mate_3_name) & as.numeric(ubi_5) == 1,1,0),
         m_mate_3_lastname = if_else(is.na(mate_3_lastname) & as.numeric(ubi_5) == 1,1,0),
         m_mate_4_name = if_else(is.na(mate_4_name) & as.numeric(ubi_6) == 1,1,0),
         m_mate_4_lastname = if_else(is.na(mate_4_lastname) & as.numeric(ubi_6) == 1,1,0))

   
## Sumar total missings    

variables_missing <- names(alertas %>%
                             select(matches("^m_")))

alertas <- alertas %>%
  mutate(
    total_missing = rowSums(alertas[,variables_missing], na.rm = T))

# Alerta de valores numéricos extremos ####


alertas <- alertas %>%
  mutate(
    ex_student_age = if_else(
      abs((as.numeric(student_age) - median(as.numeric(student_age), na.rm = T)) / sd(as.numeric(student_age), na.rm = T)) > 3, 1, 0, missing = 0
    ),
    ex_student_mother_age = if_else(
      abs((as.numeric(student_mother_age) - median(as.numeric(student_mother_age), na.rm = T)) / sd(as.numeric(student_mother_age), na.rm = T)) > 3, 1, 0, missing = 0
    ),
    ex_student_father_age = if_else(
      abs((as.numeric(student_father_age) - median(as.numeric(student_father_age), na.rm = T)) / sd(as.numeric(student_father_age), na.rm = T)) > 3, 1, 0, missing = 0
    ))



## DUPLICADOS ----

caract_especi <- c("á" = "a", "é" = "e", "í" = "i", "ó" = "o", "ú" = "u",
                   "Á" = "A", "É" = "E", "Í" = "I", "Ó" = "O", "Ú" = "U",
                   "ñ" = "n", "Ñ" = "N")

alertas <- alertas %>% 
  mutate(
    nombre =  str_squish(str_replace_all(toupper(student_name1), caract_especi)),
    apellido1 = str_squish(str_replace_all(toupper(student_name3), caract_especi)),
    apellido2 = str_squish(str_replace_all(toupper(student_name4), caract_especi))
    )


alertas <- alertas %>%
  mutate(duplicado = if_else(duplicated(select(., nombre, apellido1, apellido2,student_school,student_shift,student_fifth_l)) |
                               duplicated(select(., nombre, apellido1, apellido2,student_school,student_shift,student_fifth_l), fromLast = TRUE),
                             1, 0, missing = 0))

## Exceso de no sabe

# Lista completa de variables que deben alertar si tienen el valor 99 "No sé"
vars_99 <- c(
  # Variables iniciales
  "perspective_taking", "friends_problems", "understand_others", "understand_feelings",
  "talking_interrupt", "impulsive_do_now", "impulsive_do", "impulsive_talk", "thinking_first",
  "hyperactive", "no_move_dif", "impulsive_decisions", "impulsive_responses", "empathy_injustice",
  "empathy_defense", "empathy_feelings", "empathy_sorry", "mates_make_fun", "mates_talk_behind",
  "mates_fight", "mates_fun_migrant", "mates_fun_indigena", "mates_nice", "mates_beat_migrant",
  "mates_beat_race", "mates_protect", "cohes_help", "cohes_close_knit", "cohes_trust",
  "cohes_get_along", "cohes_values","empathy_good_hearth", "migrant_friends",
  "disability_fiends", "race_play","migrant_smart", "migrant_equal", 
  "religion_respect", "migrant_aggressive","games_1_2", "games_2_2","student_country", 
  "student_mother_country", "student_father_country", "gender"
)

# Crear las variables ns_... con valor 1 si la variable es igual a 99
alertas <- alertas %>%
  mutate(across(
    all_of(vars_99),
    ~ if_else(as.numeric(.x) == 99, 1, 0),
    .names = "ns_{.col}"
  ))

# Crear las variables ns_... con valor 1 si la variable es igual a 9999

vars_9999 <- c(
  "student_name2", "student_name4", "student_city",
  "friend_1_name", "friend_1_lastname",
  "friend_2_name", "friend_2_lastname",
  "friend_3_name", "friend_3_lastname",
  "emotional_1_name", "emotional_1_lastname",
  "emotional_2_name", "emotional_2_lastname",
  "emotional_3_name", "emotional_3_lastname",
  "academic_1_name", "academic_1_lastname",
  "academic_2_name", "academic_2_lastname",
  "academic_3_name", "academic_3_lastname"
)

alertas <- alertas %>%
  mutate(across(
    all_of(vars_9999),
    ~ if_else(as.character(.x) == "9999", 1, 0),
    .names = "ns_{.col}"
  ))


## Sumar total no sabe    

variables_ns <- names(alertas %>%
                             select(matches("^ns_")))

alertas <- alertas %>%
  mutate(
    total_ns = rowSums(alertas[,variables_ns], na.rm = T))

media_ns <- mean(alertas$total_ns, na.rm = TRUE)
sd_ns <- sd(alertas$total_ns, na.rm = TRUE)

alertas <- alertas %>%
  mutate(
    flag_ns = if_else(total_ns > media_ns + 3 * sd_ns, 1, 0)
  )


# Corregir alertas para encuestas rechazadas

alertas <- alertas %>%
  mutate(across(
    .cols = matches("^flag_|^s_|^m_|^ex_|^ns_|^total_"),
    .fns = ~ if_else(as.numeric(assent) == 2, 0, .x),
    .names = "{.col}"
  ))

# Encuestas rechazadas y duplicados, valores faltante y atípicos


alertas <- alertas %>%
  mutate(
    flag_rejected = if_else(as.numeric(assent) == 2, 1, 0),
    flag_saltos = if_else(total_saltos > 0, 1, 0),
    flag_duplicated = if_else(duplicado == 1, 1, 0),
    flag_missing = if_else(total_missing > 0, 1, 0),
    flag_extreme_values = if_else(ex_student_age == 1 |
                                    ex_student_mother_age == 1 |
                                    ex_student_father_age == 1,1,0,missing = 0))


### Crear alertas LOOKER


alertas <- alertas %>%
  mutate(total_encuestas = n(),
         Exitos = if_else(flag_duration_mas == 0 & flag_duration_menos == 0 & flag_duplicated == 0 &  
                            flag_missing == 0 &  flag_saltos == 0 & flag_extreme_values == 0 & flag_ns == 0 &
                            flag_rejected == 0,1,0),
         Alertas = if_else(flag_duration_mas == 1 | flag_duration_menos == 1 | flag_duplicated == 1 |   
                             flag_missing == 1 | flag_saltos == 1 | flag_extreme_values == 1 | flag_ns == 1,1,0),
         Rechazos = if_else(flag_rejected == 1,1,0),
         tiempos_anomalos_mas = if_else(flag_duration_mas == 1,"Sí","No"),
         tiempos_anomalos_menos = if_else(flag_duration_menos == 1,"Sí","No"),
         duplicado = if_else(flag_duplicated == 1, "Sí","No"),
         valores_faltantes = if_else(flag_missing == 1,"Sí","No"),
         saltos_irregulares = if_else(flag_saltos == 1,"Sí","No"),
         valores_extremos = if_else(flag_extreme_values == 1, "Sí", "No"),
         exceso_ns = if_else(flag_ns == 1, "Sí","No")) 

table(alertas$Exitos)



### Labels para demográficas ___________________________________________________

# Género

genero_labels <- c("Masculino" = 1, "Femenino" = 2, "Prefierre no responder" = 99)
alertas$gender_str <- factor(alertas$gender, levels = c(1, 2,99), labels = names(genero_labels))
attr(alertas$gender_str, "label") <- "Género"

# Colegio

# Definir las etiquetas para los niveles de educación
educ_labels <- c(
  "El Tabora" = 1,
  "Villa Amalia" = 2,
  "Colegio Villamar" = 3,
  "Liceo Agustín Nieto Caballero" = 4)

# Convertir en factor
alertas$student_school_str <- factor(alertas$student_school, levels = c(1:4), labels = names(educ_labels))

# Asignar una etiqueta general a la variable
attr(alertas$student_school_str, "label") <- "Colegio"


# Definir las etiquetas para los niveles de nacionalidad

# Actualizar el vector de etiquetas de nacionalidad con todos los valores
nacionalidad_labels <- c(
  "Colombia" = 1,
  "Venezuela" = 2,
  "Perú" = 3,
  "Ecuador" = 4,
  "Otro" = 88,
  "No sé" = 99
)

# Convertir la variable c1_nacionalidad en factor y asignar las etiquetas correspondientes
alertas$student_country_str <- factor(alertas$student_country, levels = c(1:4,88,99), labels = names(nacionalidad_labels))

# Asignar una etiqueta general a la variable
attr(alertas$student_country_str, "label") <- "Nacionalidad"

# Confirmación de finalización
message("Alertas creadas exitosamente.")


