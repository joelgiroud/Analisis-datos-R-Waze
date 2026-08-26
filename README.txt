Análisis Estadístico de Datos de Movilidad (Waze API)

![R](https://img.shields.io/badge/r-%23276DC3.svg?style=for-the-badge&logo=r&logoColor=white)
![JSON](https://img.shields.io/badge/JSON-000000?style=for-the-badge&logo=json&logoColor=white)
![Data Analysis](https://img.shields.io/badge/Data_Analysis-FF6F00?style=for-the-badge&logo=google-analytics&logoColor=white)

## Descripción del Proyecto
Este proyecto es una canalización integral de datos (Data Pipeline) desarrollada en **R** para extraer, transformar y analizar registros estructurados en formato **JSON** provenientes de datos de movilidad de Waze. 

El objetivo principal de este análisis es [**AQUÍ PON EL OBJETIVO DE NEGOCIO/ESTUDIO, ej: identificar patrones de congestión vehicular, zonas de alto riesgo de accidentes o evaluar los tiempos de respuesta ante alertas de tráfico en la Ciudad de México**].

Este desarrollo fue planteado como un proyecto de investigación y ciencia de datos durante mi formación en la Universidad Autónoma Metropolitana (UAM), aplicando bases sólidas de estadística y tratamiento de datos.

## Tecnologías y Librerías Utilizadas
* **Lenguaje:** R
* **Manipulación de Datos (ETL):** `dplyr`, `tidyr` (o las que hayas usado para limpiar los datos).
* **Procesamiento JSON:** `jsonlite` [o la librería exacta que usaste para parsear los JSON].
* **Visualización:** `ggplot2` [o la que aplique, ej. leaflet para mapas].
* **Metodología Estadística:** [Ej: Análisis exploratorio de datos (EDA), pruebas de hipótesis, etc.]

## Arquitectura del Flujo de Datos (ETL)

El procesamiento superó el reto de lidiar con estructuras JSON altamente anidadas. El flujo se divide en:
1. **Extracción (Extract):** Lectura masiva de registros JSON crudos simulando peticiones a un endpoint de Waze.
2. **Transformación (Transform):** Aplanamiento de arrays anidados (Flattening), limpieza de valores nulos, y normalización de variables temporales y geoespaciales utilizando expresiones regulares y lógica vectorial en R.
3. **Carga y Análisis (Load & Analyze):** Generación de DataFrames estructurados (tipo relacional) listos para la agregación estadística.

##  Hallazgos y Resultados Clave
A través del análisis estadístico, se descubrió lo siguiente:
* **Hallazgo 1:** [Ej: El 45% de las alertas de tráfico pesado ("jams") ocurren en un rango de solo 3 horas y están fuertemente correlacionadas con...]
* **Hallazgo 2:** [Ej: Se logró optimizar el parseo del JSON, reduciendo el tiempo de procesamiento en X% al vectorizar la lectura].
* **Impacto visual:** [AÑADE AQUÍ UNA IMAGEN. Puedes tomar un screenshot de una gráfica de RStudio que hayas generado y arrastrarla aquí. Es crucial que haya al menos una imagen de tus resultados].

## 🚀 Cómo ejecutar este proyecto localmente

Para reproducir este análisis en tu máquina local:

1. Clona este repositorio:
   ```bash
   git clone [https://github.com/joelgiroud/Analisis-datos-R-Waze.git](https://github.com/joelgiroud/Analisis-datos-R-Waze.git)

2. Instala las dependencias necesarias en R:
   install.packages(c("jsonlite", "dplyr", "ggplot2"))

3. Consejos de oro para este repositorio:
* Agrega imágenes (Vital):** El 90% de los reclutadores no van a ejecutar tu código. En la sección de "Hallazgos", haz capturas de pantalla de las gráficas que te escupió RStudio y arrástralas al archivo README mientras lo editas en GitHub. Eso hará que el proyecto se vea "terminado".

* El "Traductor" a SQL:** Como mencionaste que eres muy fuerte en SQL, puedes agregar una nota en el README que diga: *"La transformación realizada con dplyr en R simula la lógica de las siguientes consultas SQL complejas (JOINs, Window Functions)..."* para demostrar que dominas ambos mundos.

* Fija (Pin) el repositorio:** Una vez que actualices el README, ve a la página principal de tu GitHub y usa la opción "Customize your pins" para anclar este proyecto hasta arriba.

<FollowUp label="¿Quieres que revisemos cómo responder entrevistas técnicas sobre este código?" query="Hagamos una simulación. Si el líder técnico me pregunta sobre los retos que tuve al aplanar el JSON en R y cómo aseguré la integridad de los datos, ¿cómo debería responder usando el método STAR?"/>
