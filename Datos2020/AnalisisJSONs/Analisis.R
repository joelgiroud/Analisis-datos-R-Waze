library(jsonlite)
library(ggplot2)

cleanJams <- readRDS("cleanJams_2020.rds")

# Una vez preparados para la exploración, procederemos a seguir explorando los datos...

head(cleanJams)

# Datos estadísticos
summary(cleanJams$level, na.rm=TRUE) # 3.022
summary(cleanJams$speedKMH, na.rm=TRUE) # 7.997
summary(cleanJams$length, na.rm=TRUE) # 541.572
summary(cleanJams$speed, na.rm=TRUE) # 2.221
summary(cleanJams$roadType, na.rm=TRUE) # 3.179
summary(cleanJams$delay, na.rm=TRUE) # 155.965

# En orden de ejecución...
# Mínimo | 1er Cuartil |  Mediana | Promedio | 3er Cuartil  | Máximo
# 1.000    2.000          3.000     3.205      4.000          5.000 
# 0.000    3.720          6.590     7.220      10.000         79.780 
# 5.000    212.00         349.00    475.1      566.0          23,620.0 
# 0.0      1.033          1.831     2.006      2.778          22.161
# 1.000    1.289          2.000     3.194      6.000          22.000
# -1.0     67.00          96.00     124.9      147.0          43,275.0

# VARIANZAS
var(cleanJams$level, na.rm=TRUE)    # 1.249     -> Varianza baja
var(cleanJams$speedKMH, na.rm=TRUE) # 34.702    -> Varianza moderada
var(cleanJams$length, na.rm=TRUE)   # 287,499   -> Varianza muy alta
var(cleanJams$speed, na.rm=TRUE)    # 2.677     -> Varianza baja
var(cleanJams$roadType, na.rm=TRUE) # 7.052     -> Varianza baja
var(cleanJams$delay, na.rm=TRUE)    # 21,718    -> Varianza alta

# cleanJams:
# Varianzas altas: length, delay
# Varianzas bajas: level, speed, roadType.


################################# CORRELACIONES #################################

numeric_vars <- cleanJams[, c("level", "speedKMH", "length", "speed", "roadType", "delay")]
corr_matrix <- cor(numeric_vars)
print(corr_matrix)

#             level   speedKMH  length    speed     roadType  delay
# level      1.000    -0.685    -0.249    -0.685    0.139     0.053
# speedKMH  -0.685    1.000     0.588     1.000     0.175     -0.089
# length    -0.249    0.588     1.000     0.588     0.176     0.400
# speed     -0.685    1.000     0.588     1.000     0.175     -0.089
# roadType   0.139    0.175     0.176     0.175     1.000     0.117
# delay      0.053    -0.089    0.400     -0.089    0.117     1.000

# RELACIONES POSITIVAS
# "speedKMH", "length" y "speed" están altamente correlacionadas entre sí.
# Lo que significa que a medida que una de ellas aumenta, las otras también tienden a aumentar.
#
# "delay" muestra una correlación moderada con la variable "length".
# Esto indica que a medida que aumenta la longitud, es probable que aumente el retraso.
#
# "roadType" muestra una correlación débil con las demás variables.
# Esto sugiere que existe una relación débil entre el tipo de carretera y las otras variables.


# RELACIONES NEGATIVAS
# "level" muestra una correlación negativa moderada con las variables "speedKMH", "length" y "speed".
# Esto sugiere que a medida que el nivel aumenta, la velocidad, la longitud y la velocidad disminuyan.
# 

cut_length <- cut(cleanJams$length, breaks = 5)
cut_speedKMH <- cut(cleanJams$speedKMH, breaks = 20)
cut_delay <- cut(cleanJams$delay, breaks = 3)
print("speedKMH x length")
table(cut_speedKMH, cut_length)

# speedKMH↓ | length➝ (-18.6,4.73e+3]  (4.73e+03,9.45e+3] (9.45e+03,1.42e+4] (1.42e+04,1.89e+4] (1.89e+4,2.36e+4]
# (-0.0798,3.99]              9902449          41656                  7               14                   0
# (3.99,7.98]                12922816          1155                  53                3                   7
# (7.98,12]                   8667963          2109                 103                3                   4
# (12,16]                     3391001          4332                 144               19                   4
# (16,19.9]                   1277706          4500                 158               18                  10
# (19.9,23.9]                  536812          4360                  98               20                   4
# (23.9,27.9]                  238662          3293                  60               30                   0
# (27.9,31.9]                  136742          1902                  54               65                  10
# (31.9,35.9]                   87854          1346                  62               86                  35
# (35.9,39.9]                   49662          1165                  68              120                 119
# (39.9,43.9]                   29244          1319                  99               99                 100
# (43.9,47.9]                   16261          1440                 153              105                 133
# (47.9,51.9]                    8571          1849                 142              105                 146
# (51.9,55.8]                    3739          2083                 151               77                  53
# (55.8,59.8]                    1356          1390                 161               82                  50
# (59.8,63.8]                     708           646                 164               66                  33
# (63.8,67.8]                     472           302                 106               42                  11
# (67.8,71.8]                     427           201                  18               10                   0
# (71.8,75.8]                      47           131                   6                0                   0
# (75.8,79.9]                       1           128                   0                0                   0


print("speedKMH x delay")
table(cut_speedKMH, cut_delay)

# speedKMH↓ | delay➝  (-44.3,1.44e+04] (1.44e+04,2.88e+04] (2.88e+04,4.33e+04]
# (-0.0798,3.99]          9944072                  45                   9
# (3.99,7.98]            12924033                   1                   0
# (7.98,12]               8670182                   0                   0
# (12,16]                 3395500                   0                   0
# (16,19.9]               1282392                   0                   0
# (19.9,23.9]              541294                   0                   0
# (23.9,27.9]              242045                   0                   0
# (27.9,31.9]              138773                   0                   0
# (31.9,35.9]               89383                   0                   0
# (35.9,39.9]               51134                   0                   0
# (39.9,43.9]               30861                   0                   0
# (43.9,47.9]               18092                   0                   0
# (47.9,51.9]               10813                   0                   0
# (51.9,55.8]                6103                   0                   0
# (55.8,59.8]                3039                   0                   0
# (59.8,63.8]                1617                   0                   0
# (63.8,67.8]                 933                   0                   0
# (67.8,71.8]                 656                   0                   0
# (71.8,75.8]                 184                   0                   0
# (75.8,79.9]                 129                   0                   0

cut_length <- cut(cleanJams$length, breaks = 15)

print("length x delay")
table(cut_length, cut_delay)

# length↓ | delay➝      (-44.3,1.44e+04] (1.44e+04,2.88e+04] (2.88e+04,4.33e+04]
# (-18.6,1.58e+03]            36282699                   3                   2
# (1.58e+03,3.15e+03]           791156                   8                   5
# (3.15e+03,4.73e+03]           198620                   0                   0
# (4.73e+03,6.3e+03]             66930                   4                   1
# (6.3e+03,7.88e+03]              5985                   5                   0
# (7.88e+03,9.45e+03]             2374                   8                   0
# (9.45e+03,1.1e+04]               681                   0                   0
# (1.1e+04,1.26e+04]               886                   4                   0
# (1.26e+04,1.42e+04]              236                   0                   0
# (1.42e+04,1.57e+04]              553                  12                   1
# (1.57e+04,1.73e+04]               76                   0                   0
# (1.73e+04,1.89e+04]              321                   1                   0
# (1.89e+04,2.05e+04]               41                   1                   0
# (2.05e+04,2.2e+04]                13                   0                   0
# (2.2e+04,2.36e+04]               664                   0                   0


################# Graficando #################################

# Crear un histograma para la variable 'level'
histogram <- ggplot(cleanJams, aes(level)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(x = "Level", y = "Frequency", title = "Histogram of Level")

# Guardar el gráfico como un archivo PNG
ggsave("histogram_level.png", plot = histogram, width = 800, height = 600, units = "px", dpi = 300)

# Crear un histograma para la variable 'speedKMH'
histogram <- ggplot(cleanJams, aes(speedKMH)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(x = "SpeedKMH", y = "Frequency", title = "Histogram of SpeedKMH")

# Guardar el gráfico como un archivo PNG
ggsave("histogram_speedKMH.png", plot = histogram, width = 800, height = 600, units = "px", dpi = 300)

# Crear un histograma para la variable 'length'
histogram <- ggplot(cleanJams, aes(length)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(x = "Length", y = "Frequency", title = "Histogram of Length")

# Guardar el gráfico como un archivo PNG
ggsave("histogram_length.png", plot = histogram, width = 800, height = 600, units = "px", dpi = 300)

#

# Crear un diagrama de dispersión entre speed y length
diagrama <- ggplot(cleanJams, aes(x = length, y = speed)) +
  geom_point(size = 0.5) +
  labs(x = "Length", y = "Speed", title = "Speed vs. Length") +
  theme_minimal()

ggsave("diagrama_dispersion.png", plot = diagrama, width = 800, height = 600, units = "px", dpi = 300)

#

# Crear diagramas de cajas
diagrama <- ggplot(cleanJams, aes(x = "", y = roadType)) +
  geom_boxplot() +
  labs(x = "", y = "RoadType", title = "Diagrama de Cajas - RoadType")

# Guardar el gráfico como un archivo PNG en el directorio actual
ggsave("box_roadType.png", plot = diagrama, width = 900, height = 680, units = "px", dpi = 350)

# Crear diagramas de cajas
diagrama <- ggplot(cleanJams, aes(x = "", y = speed)) +
  geom_boxplot() +
  labs(x = "", y = "Speed", title = "Diagrama de Cajas - Speed")

# Guardar el gráfico como un archivo PNG en el directorio actual
ggsave("box_speed.png", plot = diagrama, width = 900, height = 680, units = "px", dpi = 350)

#

# Gráfico de densidad
diagrama <- ggplot(cleanJams, aes(x = length)) +
  geom_density() +
  labs(x = "Length", y = "Density", title = "Densidad de Length")

# Guardar el gráfico como un archivo PNG en el directorio actual
ggsave("density_length.png", plot = diagrama, width = 900, height = 680, units = "px", dpi = 350)


########### EXPORTACIÓN ###########

# Convertimos a Json
exportJSON <- toJSON(cleanJams)
 
# Escribimos el documento
write(exportJSON, "clean_data.json")

print("Script terminado.")