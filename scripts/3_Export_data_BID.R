# Verificar si las credenciales están disponibles
if (!exists("temp_creds_file") || !file.exists(temp_creds_file)) {
  stop("No se encontraron las credenciales de Google Sheets. Asegúrate de cargarlas desde el script maestro.")
}

# Autenticación con Google Sheets usando la Service Account
message("Autenticando con Google Sheets...")
tryCatch({
  gs4_auth(
    path = temp_creds_file,  # Archivo temporal de credenciales
    cache = ".secrets"
  )
  message("Autenticación de Google Sheets completada.")
}, error = function(e) {
  stop("Error en la autenticación de Google Sheets: ", e)
})


### Exportar alertas

sheet_url_alertas <- "https://docs.google.com/spreadsheets/d/1dTDBqFrHo6ddxEK43z0kMB4rTCbFlYCoRjytSDnRTDY/edit?gid=0#gid=0"
message("Conectando al Google Sheet de alertas: ", sheet_url_alertas)

sheet_alertas <- tryCatch({
  gs4_get(sheet_url_alertas)
}, error = function(e) {
  stop("Error al conectar con el Google Sheet de alertas: ", e)
})


# Escribir datos en el Google Sheet
message("Exportando datos de alertas...")
tryCatch({
  sheet_write(alertas, ss = sheet_alertas, sheet = "alertas")
  message("Datos de alertas exportados correctamente.")
}, error = function(e) {
  stop("Error al exportar datos de alertas: ", e)
})


Sys.sleep(300)

message("Exportando datos crudos...")
tryCatch({
  sheet_write(df, ss = sheet_alertas, sheet = "raw_data")
  message("Datos exportados correctamente.")
}, error = function(e) {
  stop("Error al exportar datos de alertas: ", e)
})


Sys.sleep(200)

message("Exportando alertas encuestadores...")
tryCatch({
  sheet_write(alertas_encuestadores, ss = sheet_alertas, sheet = "alertas_encuestador")
  message("Datos exportados correctamente.")
}, error = function(e) {
  stop("Error al exportar datos de alertas: ", e)
})

message("Exportando alertas por colegio...")
tryCatch({
  sheet_write(seguimiento_colegios, ss = sheet_alertas, sheet = "encuestas_por_colegio")
  message("Datos exportados correctamente.")
}, error = function(e) {
  stop("Error al exportar datos de alertas: ", e)
})

Sys.sleep(200)

message("Exportando resultados de juegos...")
tryCatch({
  sheet_write(juego_clean, ss = sheet_alertas, sheet = "puntajes_juegos")
  message("Datos exportados correctamente.")
}, error = function(e) {
  stop("Error al exportar datos de alertas: ", e)
})










