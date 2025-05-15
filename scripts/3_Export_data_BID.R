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


