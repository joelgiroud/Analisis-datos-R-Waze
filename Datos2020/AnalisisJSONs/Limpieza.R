
# Obtenemos dataframe  
#jams <- as.data.frame(readRDS("df_list.rds"))
jams <- readRDS("jams.rds")

dim(jams) # 37,351,290 filas y 28 columnas
names(jams) # Nombres de columnas

# Creamos columna count_na
jams$count_na <- rowSums(is.na(jams))

# Aquí notamos que no hay una fila que no contenga datos con NA
length(which(jams$count_na == 0)) # 0
length(which(jams$count_na > 0))  # 37,351,290

# Analizamos respecto a la columna count_na
max(jams$count_na) #42
min(jams$count_na) #4
mean(jams$count_na) #37.4 ~ 37
sum(jams$count_na > mean(jams$count_na)) # Mayores al promedio = 30,069,004/37,351,290
sum(jams$count_na < mean(jams$count_na)) # Menores al promedio = 7,282,286/37,351,290

# Ordenamos de acuerdo a la cantidad de NA's
jams <- jams[order(jams$count_na, decreasing = TRUE), ] 
head(jams) # Observamos columnas con demasiados NA's


# Candidatos a remover:
# blockingAlertID
# blockExpiration
# endNode
# blockStartTime
# blockUpdate
# blockType
# blockingAlertUuid
# blockDescription
# startNode
# causeAlert (De nuevo demasiada información con NA's)
# country (repetitivo)

# Revisamos rangos de candidatos
sum(is.na(jams$blockingAlertID))  # nulos: 30,616,801 / 37,351,290 ~ 82%
sum(is.na(jams$blockExpiration))  # nulos: 30,616,801 / 37,351,290 ~ 82%
sum(is.na(jams$endNode))          # nulos: 3,860,081  / 37,351,290 ~ 10%
sum(is.na(jams$blockStartTime))   # nulos: 30,616,801 / 37,351,290 ~ 82%
sum(is.na(jams$blockUpdate))      # nulos: 30,616,801 / 37,351,290 ~ 82%
sum(is.na(jams$blockType))        # nulos: 30,616,801 / 37,351,290 ~ 82%
sum(is.na(jams$blockingAlertUuid))# nulos: 30,616,801 / 37,351,290 ~ 82%
sum(is.na(jams$blockDescription)) # nulos: 30,616,801 / 37,351,290 ~ 82%
sum(is.na(jams$startNode))        # nulos: 35,902,304 / 37,351,290 ~ 96%
sum(jams$country == 'MX')         # coincidencias: 37,351,290 / 37,351,290 = 100%

# NOTAMOS DE NUEVO QUE TODOS LOS CANDIDATOS, SALVO endNode SON APTOS PARA ELIMINARSE

# Eliminando...
cleanJams <- subset(jams, select = -c(country, causeAlert, blockDescription,
                                      startNode,blockingAlertUuid, blockType,
                                      blockUpdate, blockStartTime, blockExpiration,
                                      blockingAlertID, endNode, count_na))

# Volvemos a contar NA's y ordenamos...
cleanJams$count_na <- rowSums(is.na(cleanJams))
cleanJams <- cleanJams[order(cleanJams$count_na, decreasing = TRUE), ]
max(cleanJams$count_na) #2
min(cleanJams$count_na) #0
mean(cleanJams$count_na) #0.07 ~ 0.1
sum(cleanJams$count_na > mean(cleanJams$count_na)) # Mayores al promedio = 2,765,431/37,351,290
sum(cleanJams$count_na < mean(cleanJams$count_na)) # Menores al promedio = 34,585,859/37,351,290

# Los datos son trabajables ya. Aunque podemos hacer una segunda limpieza si es necesario...
head(cleanJams)

# Nos fijamos en las columnas street, turnType, type, severity, city
sum(is.na(cleanJams$street))      # nulos: 1,338,228 / 37,351,290 ~ 3.5%
sum(is.na(cleanJams$city))        # nulos: 1,449,070 / 37,351,290 ~ 3.8%
sum(cleanJams$turnType == "NONE") # coincidencias: 37,351,290 / 37,351,290 = 100%
sum(cleanJams$type == "NONE")     # coincidencias: 36,664,235 / 37,351,290 ~ 98%
sum(cleanJams$severity == 5)      # coincidencias: 37,351,290 / 37,351,290 = 100%
range(cleanJams$severity)         # Sus valores van desde 5 a 5 = 100%

# NOTAMOS QUE TODOS LOS CANDIDATOS SON FACTIBLES DE ELIMINAR, MENOS street, Y city.
# Eliminando...
cleanJams <- subset(cleanJams, select = -c(turnType, type, severity, type, count_na))

############# DATOS LIMPIOS #############

library(jsonlite)

# Exportamos como archivo RDS
saveRDS(cleanJams, file = "cleanJams_2020.rds")

# Exportamos como archivo JSON
write_json(cleanJams, "cleanJams_2020.json")
