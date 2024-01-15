
# Obtenemos dataframe  
jams <- as.data.frame(readRDS("df_list.rds"))

dim(jams) # 36,697,495 filas y 28 columnas
names(jams) # Nombres de columnas

# Creamos columna count_na
jams$count_na <- rowSums(is.na(jams))

# Aquí notamos que no hay una fila que no contenga datos con NA
length(which(jams$count_na == 0))
length(which(jams$count_na > 0))

# Analizamos respecto a la columna count_na
max(jams$count_na) #45
min(jams$count_na) #6
mean(jams$count_na) #39.9 ~ 40
sum(jams$count_na > mean(jams$count_na)) # Mayores al promedio = 32,464,693/36,697,495
sum(jams$count_na < mean(jams$count_na)) # Menores al promedio = 4,232,002/36,697,495

# Ordenamos de acuerdo a la cantidad de NA's
jams <- jams[order(jams$count_na, decreasing = TRUE), ] 
head(jams) # Observamos columnas con demasiados NA's

# Candidatos a remover:
# blockDescription
# startNode
# blockingAlertUuid
# blockType
# blockUpdate
# blockStartTime
# blockExpiration
# blockingAlertID
# endNode
# causeAlert (demasiadas subcolumnas con NA)
# country (repetitivo)

# Revisamos rangos de candidatos
sum(is.na(jams$blockDescription)) # nulos: 33,622,056/36,697,495
sum(is.na(jams$startNode)) # nulos:  35,477,796/36,697,495
sum(is.na(jams$blockingAlertUuid)) # nulos: 33,361,920/36,697,495 
sum(is.na(jams$blockType)) # nulos: 33,361,920/36,697,495
sum(is.na(jams$blockUpdate)) # nulos: 33,361,920/36,697,495 
sum(is.na(jams$blockStartTime)) # nulos: 33,361,920/36,697,495 
sum(is.na(jams$blockExpiration)) # nulos: 33,361,920/36,697,495 
sum(is.na(jams$blockingAlertID)) # nulos: 33,361,920/36,697,495
sum(is.na(jams$endNode)) # nulos: 3,396,335/36,697,495 < 10%

# NOTAMOS QUE TODOS LOS CANDIDATOS, SALVO endNode SON APTOS PARA ELIMINARSE

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
mean(cleanJams$count_na) #0.05 ~ 0.1
sum(cleanJams$count_na > mean(cleanJams$count_na)) # Mayores al promedio = 1,896,600/36,697,495
sum(cleanJams$count_na < mean(cleanJams$count_na)) # Menores al promedio = 34,800,895/36,697,495

# Los datos son trabajables ya. Aunque podemos hacer una segunda limpieza si es necesario...
head(cleanJams)

# Nos fijamos en las columnas street, turnType, type, severity, city
sum(is.na(cleanJams$street)) # nulos: 712,649/36,697,495 < 10%
sum(is.na(cleanJams$city)) # nulos: 1,221,652/36,697,495 < 10%
sum(cleanJams$turnType == "NONE") # 36,697,495/36,697,495
sum(cleanJams$type == "NONE") # 35,681,040/36,697,495
sum(cleanJams$severity == 5) # 19,052,467/36,697,495 ~ 50%
range(cleanJams$severity) # Sus valores van desde 0 a 6

# NOTAMOS QUE LOS CANDIDATOS DIGNOS A ELIMINAR SON turnType y type.
# Eliminando...
cleanJams <- subset(cleanJams, select = -c(turnType, type, count_na))

############# DATOS LIMPIOS #############

library(jsonlite)

# Exportamos como archivo RDS
saveRDS(cleanJams, file = "cleanJams_2019.rds")

# Exportamos como archivo JSON
write_json(cleanJams, "cleanJams_2019.json")
