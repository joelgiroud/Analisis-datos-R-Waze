library(jsonlite)

cleanJams <- readRDS("cleanJams_2019.rds")
print("Guardando como json")
write_json(cleanJams, "cleanJams_2019.json")