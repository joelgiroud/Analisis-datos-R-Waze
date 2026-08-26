# 🚗📊 Waze Traffic Data Pipeline & EDA 

[![R](https://img.shields.io/badge/R-276DC3?style=for-the-badge&logo=r&logoColor=white)]()
[![JSON](https://img.shields.io/badge/JSON-000000?style=for-the-badge&logo=json&logoColor=white)]()
[![Data Analysis](https://img.shields.io/badge/Data_Analysis-FF6F00?style=for-the-badge&logo=google-analytics&logoColor=white)]()
[![ETL](https://img.shields.io/badge/ETL_Pipeline-4CAF50?style=for-the-badge)]()

##  Descripción del Proyecto
Este proyecto es una canalización de datos (Data Pipeline) integral desarrollada en **R** para la extracción, transformación y análisis estadístico de registros de movilidad urbana provenientes de archivos **JSON** de Waze.

Desarrollado como parte de la investigación en ciencia de datos dentro de la Universidad Autónoma Metropolitana (UAM), el objetivo principal es diseccionar estructuras de datos complejas para identificar patrones de congestión vehicular, severidad de incidentes y tiempos de retraso.

A diferencia de un script de análisis tradicional, la arquitectura de este proyecto está modularizada siguiendo las mejores prácticas de **Ingeniería de Software y flujos ETL**, separando las responsabilidades de recolección, limpieza y visualización.

##  Arquitectura del Flujo de Datos (ETL Modular)

El procesamiento superó el reto de lidiar con estructuras JSON altamente anidadas, dividiendo la ejecución en 4 módulos independientes:

1. **`Recoleccion.R` (Extract):** Automatización de la lectura y carga en memoria de los registros JSON crudos.
2. **`Convert.R` (Transform - Parsing):** Aplanamiento de arrays anidados (Flattening) y transformación de diccionarios JSON a estructuras de datos tabulares y relacionales.
3. **`Limpieza.R` (Transform - Cleaning):** Normalización de variables geoespaciales, imputación de valores nulos y formateo de series temporales y tipos de datos (ej. conversión de velocidades a KM/H).
4. **`Analisis.R` & `Main.Rmd` (Load & EDA):** Procesamiento estadístico, análisis exploratorio de datos (EDA) y generación de reportes reproducibles.

##  Hallazgos y Análisis Visual
*(Nota: El análisis bivariado y las distribuciones de frecuencia revelan el comportamiento del tráfico en tiempo real)*

### Relación entre Severidad y Longitud del Embotellamiento
![Severidad vs Longitud](plot_severity&&length.png)
*Análisis bivariado que evalúa cómo la categorización de severidad de Waze se correlaciona con la extensión física del tráfico.*

### Distribución de Velocidades y Tiempos de Retraso
* **Velocidad promedio (KM/H):** Evaluación de la fluidez en incidentes reportados.
  ![Velocidad KMH](jams$speedKMH.png)
* **Retraso temporal:** Impacto directo en el tiempo del usuario.
  ![Retraso](jams$delay.png)

### Clasificación de Incidentes
* **Nivel del embotellamiento:** ![Nivel](jams$level.png)
* **Tipo de Vía Afectada:** ![Tipo de Vía](jams$roadType.png)

##  Cómo ejecutar este proyecto localmente

Para reproducir este análisis y visualizar el flujo de datos:

1. Clona este repositorio:
   ```bash
   git clone [https://github.com/joelgiroud/Analisis-datos-R-Waze.git](https://github.com/joelgiroud/Analisis-datos-R-Waze.git)
