# DEBEMOS ESTAR SITUADOS EN "trafico-jams/2019" o "trafico-jams/2020".

library("jsonlite")
library("dplyr")
library(data.table)

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


############ PASO EXTRA ############
# Noté que únicamente en los jams de Enero las últimas 2 columnas tienen un orden invertido
# Esto me impedía recolectar los dataframes debidamente.

# Intercambiar las columnas 27 y 28 de df_list$enero

df_list$enero <- df_list$enero[, c(1:26, 28, 27)]

# Exportamos como archivo rds
saveRDS(df_list, file = "df_list.rds")
print("df_list guardado")

jams <- bind_rows(df_list)
saveRDS(jams, file = "jams.rds")
print("jams guardados")

print("terminado")

