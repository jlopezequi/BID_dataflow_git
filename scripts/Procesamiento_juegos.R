resultados_juego <- df %>%
  filter(assent == 1) %>%
  mutate(
    games_3 = coalesce(games_3_a, games_3_b),
    across(starts_with("games"), ~as.numeric(.))
  )

# Inicializamos las nuevas columnas
resultados_juego$resultado_juego_mc <- NA_real_
resultados_juego$resultado_juego_oc <- NA_real_
resultados_juego$resultado_final   <- NA_real_

set.seed(123)

# Bucle robusto
for (i in seq_len(nrow(resultados_juego))) {
  
  colegio_i   <- resultados_juego$school_final[i]
  juego_mc_i  <- resultados_juego$games_1_1[i]
  juego_oc_i  <- resultados_juego$games_2_1[i]
  juego_3     <- resultados_juego$games_3[i]
  
  # ----------- Resultado juego MC (mismo colegio) ----------------
  if (isTRUE(juego_mc_i == 1)) {
    posibles <- resultados_juego$games_1_3[resultados_juego$school_final == colegio_i]
    if (length(posibles) > 0) {
      resultados_juego$resultado_juego_mc[i] <- 3 + sample(posibles, 1)
    }
  } else if (isTRUE(juego_mc_i == 2)) {
    posibles <- resultados_juego$games_1_4[resultados_juego$school_final == colegio_i]
    if (length(posibles) > 0) {
      resultados_juego$resultado_juego_mc[i] <- 2 + sample(posibles, 1)
    }
  } else if (isTRUE(juego_mc_i == 3)) {
    posibles <- resultados_juego$games_1_5[resultados_juego$school_final == colegio_i]
    if (length(posibles) > 0) {
      resultados_juego$resultado_juego_mc[i] <- 1 + sample(posibles, 1)
    }
  } else if (isTRUE(juego_mc_i == 4)) {
    posibles <- resultados_juego$games_1_6[resultados_juego$school_final == colegio_i]
    if (length(posibles) > 0) {
      resultados_juego$resultado_juego_mc[i] <- 0 + sample(posibles, 1)
    }
  } else if (isTRUE(juego_mc_i == 0)) {
    resultados_juego$resultado_juego_mc[i] <- 4
  }
  
  # ----------- Resultado juego OC (otro colegio) ----------------
  if (isTRUE(juego_oc_i == 1)) {
    posibles <- resultados_juego$games_2_3[resultados_juego$school_final != colegio_i]
    if (length(posibles) > 0) {
      resultados_juego$resultado_juego_oc[i] <- 3 + sample(posibles, 1)
    }
  } else if (isTRUE(juego_oc_i == 2)) {
    posibles <- resultados_juego$games_2_4[resultados_juego$school_final != colegio_i]
    if (length(posibles) > 0) {
      resultados_juego$resultado_juego_oc[i] <- 2 + sample(posibles, 1)
    }
  } else if (isTRUE(juego_oc_i == 3)) {
    posibles <- resultados_juego$games_2_5[resultados_juego$school_final != colegio_i]
    if (length(posibles) > 0) {
      resultados_juego$resultado_juego_oc[i] <- 1 + sample(posibles, 1)
    }
  } else if (isTRUE(juego_oc_i == 4)) {
    posibles <- resultados_juego$games_2_6[resultados_juego$school_final != colegio_i]
    if (length(posibles) > 0) {
      resultados_juego$resultado_juego_oc[i] <- 0 + sample(posibles, 1)
    }
  } else if (isTRUE(juego_oc_i == 0)) {
    resultados_juego$resultado_juego_oc[i] <- 4
  }
  
  # ----------- Resultado final: aleatorio entre mc, oc, juego_3 --------------
  opciones <- c(resultados_juego$resultado_juego_mc[i],
                resultados_juego$resultado_juego_oc[i],
                juego_3)
  opciones <- opciones[!is.na(opciones)]  # Quita NA
  
  if (length(opciones) > 0) {
    resultados_juego$resultado_final[i] <- sample(opciones, 1)
  }
  
  # Progreso
  if (i %% 100 == 0) cat("Fila", i, "procesada\n")
}


### Exportar_resultado

juego_clean <- resultados_juego %>%
  select(encuestador = username , colegio = colegio_str, sede = sede_final, curso = curso_final, jornada = jornada_final, nombre_estudiante = name_final, puntaje = resultado_final)%>%
  mutate(puntaje = if_else(puntaje == 0,1,puntaje))%>%
  arrange(desc(puntaje))
