library(jsonlite)
library(ggplot2)

cleanJams <- as.data.frame(readRDS("cleanJams_2019.rds"))

# Una vez preparados para la exploración, procederemos a seguir explorando los datos...

head(cleanJams)

# Datos estadísticos
summary(cleanJams$severity, na.rm=TRUE) # 2.787
summary(cleanJams$level, na.rm=TRUE) # 3.022
summary(cleanJams$speedKMH, na.rm=TRUE) # 7.997
summary(cleanJams$length, na.rm=TRUE) # 541.572
summary(cleanJams$speed, na.rm=TRUE) # 2.221
summary(cleanJams$roadType, na.rm=TRUE) # 3.179
summary(cleanJams$delay, na.rm=TRUE) # 155.965

# En orden de ejecución...
# Mínimo | 1er Cuartil |  Mediana | Promedio | 3er Cuartil  | Máximo
# 0.000    0.000          5.000     2.788      5.000          6.000 
# 1.000    2.000          3.000     3.023      4.000          5.000 
# 0.000    4.640          7.210     7.998      10.450         78.050 
# 5.0      250.0          404.0     541.6      640.0          23620.0
# 0.000    1.289          2.003     2.222      2.903          21.681
# 1.000    1.000          2.000     3.179      6.000          22.000
# -1       77            108        156        177            29158

# VARIANZAS
var(cleanJams$severity, na.rm=TRUE) # 5.516   -> Varianza moderada.
var(cleanJams$level, na.rm=TRUE) # 1.003      -> Varianza baja.
var(cleanJams$speedKMH, na.rm=TRUE) # 30.908  -> Varianza alta.
var(cleanJams$length, na.rm=TRUE) # 307,169   -> Varianza muy alta.
var(cleanJams$speed, na.rm=TRUE) # 2.384      -> Varianza moderada.
var(cleanJams$roadType, na.rm=TRUE) # 6.087   -> Varianza moderada.
var(cleanJams$delay, na.rm=TRUE) # 29447.34   -> Varianza alta.

# cleanJams:
# Varianzas altas: speedKMH, delay
# Varianzas bajas: level.


################# Graficando Histogramas #################################

# Crear un histograma para la variable delay'
histogram <- ggplot(cleanJams, aes(level)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(x = "Delay", y = "Frequency", title = "Histogram of Delay")

# Guardar el gráfico como un archivo PNG
ggsave("histogram_delay.png", plot = histogram, width = 800, height = 600, units = "px", dpi = 300)

# Crear un histograma para la variable roadType'
histogram <- ggplot(cleanJams, aes(level)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(x = "RoadType", y = "Frequency", title = "Histogram of RoadType")

# Guardar el gráfico como un archivo PNG
ggsave("histogram_roadType.png", plot = histogram, width = 800, height = 600, units = "px", dpi = 300)

# Crear un histograma para la variable speed'
histogram <- ggplot(cleanJams, aes(level)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(x = "Speed", y = "Frequency", title = "Histogram of Speed")

# Guardar el gráfico como un archivo PNG
ggsave("histogram_speed.png", plot = histogram, width = 800, height = 600, units = "px", dpi = 300)

# Crear un histograma para la variable 'speedKMH'
histogram <- ggplot(cleanJams, aes(speedKMH)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(x = "SpeedKMH", y = "Frequency", title = "Histogram of SpeedKMH")

# Guardar el gráfico como un archivo PNG
ggsave("histogram_speedKMH.png", plot = histogram, width = 800, height = 600, units = "px", dpi = 300)

# Crear un histograma para la variable 'level'
histogram <- ggplot(cleanJams, aes(level)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(x = "Level", y = "Frequency", title = "Histogram of Level")

# Guardar el gráfico como un archivo PNG
ggsave("histogram_level.png", plot = histogram, width = 800, height = 600, units = "px", dpi = 300)

# Crear un histograma para la variable 'severity'
histogram <- ggplot(cleanJams, aes(severity)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(x = "Severity", y = "Frequency", title = "Histogram of Severity")

# Guardar el gráfico como un archivo PNG
ggsave("histogram_severity.png", plot = histogram, width = 800, height = 600, units = "px", dpi = 300)


################################# CORRELACIONES #################################

numeric_vars <- cleanJams[, c("severity", "level", "speedKMH", "length", "speed", "roadType", "delay")]
corr_matrix <- cor(numeric_vars)

#             severity    level       speedKMH     length       speed       roadType    delay
# severity  1.000000000  0.09234532 -0.04537614 -0.009861704 -0.04537614  0.02514426 -0.004280124
# level     0.092345319  1.00000000 -0.60920235 -0.202096082 -0.60920235  0.18852962  0.165094411
# speedKMH -0.045376140 -0.60920235  1.00000000  0.612861766  1.00000000  0.19916358 -0.046737064
# length   -0.009861704 -0.20209608  0.61286177  1.000000000  0.61286177  0.21420536  0.500536369
# speed    -0.045376140 -0.60920235  1.00000000  0.612861766  1.00000000  0.19916358 -0.046737064
# roadType  0.025144263  0.18852962  0.19916358  0.214205361  0.19916358  1.00000000  0.189097240
# delay    -0.004280124  0.16509441 -0.04673706  0.500536369 -0.04673706  0.18909724  1.000000000

# RELACIONES POSITIVAS
# roadType muestra una correlación positiva muy débil con "speedKMH" (0.199) y "speed" (0.199).
# severity muestra una correlación positiva muy débil con "level" (0.092).
# roadType muestra una correlación positiva débil con "level" (0.189).
# delay muestra una correlación positiva moderada con "length" (0.501)
  # lo cual indica que a medida que aumenta la longitud del atasco, también tiende a aumentar el retraso.
# level: tiene una correlación positiva con "delay" (0.165).
# length: tiene una correlación positiva con "delay" (0.501) y "speedKMH" (0.613).
# speed y speedKMH tienen una correlación positiva perfecta (1.000), lo cual indica que los de velocidad están bien capturados.

# RELACIONES NEGATIVAS
# severity muestra una correlación negativa muy débil con "speedKMH" (-0.045) y "speed" (-0.045).
# level: tiene una correlación negativa con "speedKMH" (-0.609).


print("roadType x severity")
table(cleanJams$roadType, cleanJams$severity)
#          0       1       2       3       4       5       6  severity
# 1  5840179  257264 1022785    1427       0 7393059       0
# 2  3159156  319761  136683      22       0 4348926       0
# 3   130796  530222  134268   20341    1093  686515       0
# 4   149657   43714   80220    1242       1  323362       0
# 5        0       0       0    1138       0    1312       0
# 6  1350975 1066117  372008   18882      15 2881037       0
# 7  2065630  691578  236533    5253       0 3407104       0
# ...
# 17       0       0    4995     290       0    8835       0
# ...
# 20       0       0    2769       6       0    2317       5
# ...
# 22       0       0       3       0       0       0       0
# roadType


print("level x severity")
table(cleanJams$level, cleanJams$severity)

#         0       1       2       3       4       5       6   severity
# 1  770397   40775    1783     207       0  808719       0
# 2 4600271  164480    4280     934     167 5017748       0
# 3 5764485  971524   87322    2832     310 7949308       0
# 4 1561240 1731877  226086   16427     631 3624112       0
# 5       0       0 1670793   28201       1 1652580       5
# level


print("roadType x level")
table(cleanJams$roadType, cleanJams$level)
#          1       2       3       4       5    level
# 1   928434 5092207 5355173 1288455 1850445
# 2   398166 2540128 3266151 1243096  517007
# 3    86874  293360  719695  373336   29970
# 4     1909   13360  166386  274284  142257
# 5        0       0       0       0    2450
# 6    85712  716603 2376350 2082921  427448
# 7   120781 1132221 2892026 1898281  362789
# ...
# 17       5       1       0       0   14114
# ...
# 20       0       0       0       0    5097
# ...
# 22       0       0       0       0       3
# roadType



# Crear el gráfico
plot <- ggplot(cleanJams, aes(x = length, y = severity)) +
  geom_point() +
  labs(title = "Relación entre largo del tráfico y severidad",
       x = "Largo del tráfico",
       y = "Severidad")

# Guardar el gráfico como un archivo PNG en el directorio actual
ggsave("plot_severity&&length.png", plot = plot, width = 900, height = 680, units = "px", dpi = 350)


########### EXPORTACIÓN ###########

#0 Convertimos a Json
exportJSON <- toJSON(listaJson)
 
# Escribimos el documento
write(exportJSON, "clean_data.json")

print("Script terminado.")