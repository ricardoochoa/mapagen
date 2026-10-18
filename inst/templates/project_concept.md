# Concepto del Proyecto y Objetivos Analíticos

## 1. Identificación
* **Nombre del Proyecto:** [Nombre descriptivo del proyecto]
* **Organización / Unidad:** [Laboratorio o departamento]
* **Fecha de Inicio:** [AAAA-MM-DD]
* **Investigador / Analista Principal:** [Nombre del analista]

## 2. Objetivos de Negocio / Investigación
* **Objetivo General:** [Describir la meta central del análisis territorial o espacial]
* **Preguntas Específicas:**
  1. [Pregunta 1, ej. ¿Cómo se distribuyen espacialmente las variables de interés?]
  2. [Pregunta 2, ej. ¿Existe correlación o proximidad entre capas de demanda y oferta?]
  3. [Pregunta 3, ej. ¿Cuáles son los polígonos con mayor vulnerabilidad o asimetría?]

## 3. Criterios Metodológicos y Espaciales
* **Región de Estudio / Bounding Box:** [Ámbito geográfico, ej. Área Metropolitana, Municipio, Nacional]
* **Unidad Espacial de Análisis:** [ej. Manzanas censales, AGEBs, Cuadrículas 1km, Municipios]
* **Sistemas de Referencia de Coordenadas (CRS):**
  * Proyección métrica de trabajo preferida: Definida en `style.json` (ej. EPSG:6362).
  * Las capas en coordenadas geográficas (EPSG:4326) deben reproyectarse antes de cálculos métricos (buffers, distancias, áreas).

## 4. Lineamientos de Salida y Publicación
* Todas las figuras deben exportarse mediante `mapagen::export_iteration()`.
* Los reportes técnicos se compilarán en Quarto utilizando `mapagen::include_figure()`.
* Cada visualización debe contar con un sustento numérico resumido en JSON para alimentar la narrativa técnica generada por el agente LLM.
