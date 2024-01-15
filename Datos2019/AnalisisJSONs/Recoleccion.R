# DEBEMOS ESTAR SITUADOS EN "trafico-jams/2019" o "trafico-jams/2020".

library("jsonlite")
library("dplyr")

# Recolección de datos

# Definimos la lista de meses disponibles en el sistema de archivos.
meses <- c("enero", "febrero", "marzo", "abril", "mayo", "junio",
           "julio", "agosto", "septiembre", "octubre", "noviembre", "diciembre")
df_list <- list()

# Esta línea es opcional, es para medir el tiempo de ejecución...
start_time <- proc.time()

# Luego, creamos un ciclo que recorra cada uno de los meses y, dentro de este loop, otro que recorra cada uno de los archivos JSON de ese mes.

# En cada iteración, leemos el archivo JSON y extraemos la información necesaria.
for (mes in meses) {
  archivos <- list.files(path = mes, pattern = "*.json", full.names = TRUE)
  lista_mes <- list()
  for (archivo in archivos) {
    tryCatch({
      datos_json <- jsonlite::fromJSON(txt = archivo)
      jams <- datos_json$jams
      lista_mes[[archivo]] <- jams
    }, error = function(e) {
      cat("Error en archivo:", archivo, "\n")
      print(e)
    })
  }
  df_mes <- bind_rows(lista_mes)
  df_list[[mes]] <- df_mes
}

# Opcional
end_time <- proc.time()
print(paste0("\nTiempo transcurrido: ",(end_time - start_time)/60, " minutos."))

jams <- rbind(df_list$enero, df_list$febrero, df_list$marzo,
              df_list$abril, df_list$mayo, df_list$junio,
              df_list$julio, df_list$agosto, df_list$septiembre,
              df_list$octubre, df_list$noviembre, df_list$diciembre)

# Exportamos como archivo rds
saveRDS(jams, file = "jams.rds")
print("jams guardado")

# Exportamos como archivo rds
saveRDS(df_list, file = "df_list")
print("df_list guardado")

print("terminado")

