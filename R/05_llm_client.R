#' Verifica la disponibilidad del paquete ellmer
#' @keywords internal
check_ellmer <- function() {
  if (!requireNamespace("ellmer", quietly = TRUE)) {
    stop(
      "El paquete 'ellmer' es necesario para la comunicacion con modelos de lenguaje (LLM).\n",
      "Por favor instalalo ejecutando:\n",
      "  install.packages('ellmer')\n",
      "o desde GitHub:\n",
      "  remotes::install_github('tidyverse/ellmer')",
      call. = FALSE
    )
  }
}

#' Ensambla el contexto del sistema para el LLM a partir de los archivos del proyecto
#'
#' @param concept_path Ruta a `project_concept.md`.
#' @param style_path Ruta a `style.json`.
#' @param context_path Ruta a `context_metadata.json`.
#'
#' @return Cadena de texto con las instrucciones del sistema y metadatos inyectados.
#' @export
get_llm_system_context <- function(concept_path = "project_concept.md",
                                   style_path = "style.json",
                                   context_path = "context_metadata.json") {
  # 1. Concepto y objetivos
  concept_txt <- if (file.exists(concept_path)) {
    paste(readLines(concept_path, warn = FALSE), collapse = "\n")
  } else {
    "No se especifico archivo project_concept.md."
  }

  # 2. Resumen del catalogo de datos
  catalog_summary <- summary_context(context_path)

  # 3. Preferencias de estilo y cartografia
  style_cfg <- if (file.exists(style_path)) read_style(style_path) else list()
  pref_crs <- style_cfg$cartography$preferred_crs$projected %||% "EPSG:6362"
  lang <- style_cfg$project_metadata$language %||% "es"

  # Ensamblar prompt de sistema
  paste0(
    "Eres un asistente cientifico y cartografo experto en R (ecosistema sf, terra, tidyverse y ggplot2).\n\n",
    "## REGLAS CRITICAS DE DISENO Y ANALISIS ESPACIAL:\n",
    "1. LENGUAJE Y CODIGO: Escribe explicaciones en idioma '", lang, "'. Todo codigo R debe ser limpio, moderno y reproducible.\n",
    "2. SISTEMAS DE COORDENADAS (CRS): El CRS proyectado preferido para calculos metricos (areas, buffers, distancias) es '", pref_crs, "'. NUNCA realices calculos metricos directamente sobre capas en coordenadas geograficas no proyectadas (EPSG:4326); reproyectalas primero usando `sf::st_transform(x, '", pref_crs, "')`.\n",
    "3. ESTILO VISUAL: En todos los graficos ggplot2, debes invocar siempre `mapagen::theme_from_json()` y las decoraciones cartograficas con `mapagen::add_cartographic_decorations()`.\n",
    "4. EXPORTACION Y TRAZABILIDAD: Para guardar graficos, usa siempre `mapagen::export_iteration(plot, id = '...', caption = '...', summary_data = ...)`, asegurando pasar una tabla resumen de metricas a `summary_data`.\n\n",
    "## CONTEXTO Y OBJETIVOS DEL PROYECTO:\n", concept_txt, "\n\n",
    "## CATALOGO DE DATOS DISPONIBLES:\n", catalog_summary
  )
}

#' Operador de concatenacion interna
#' @keywords internal
`%s%` <- function(a, b) paste0(a, b)

#' Crea un cliente de chat agéntico multi-proveedor utilizando ellmer
#'
#' Inicializa una sesion de chat conectada al proveedor seleccionado (Gemini por defecto,
#' Claude, OpenAI u Ollama), inyectando automaticamente el contexto del proyecto y el
#' catalogo de datos geoespaciales.
#'
#' @param provider Proveedor de LLM: `"gemini"` (por defecto), `"claude"`, `"openai"`, o `"ollama"`.
#' @param model Nombre especifico del modelo. Si es `NULL`, se usa el recomendado por cada proveedor.
#' @param system_prompt Instrucciones del sistema opcionales para sobreescribir las generadas por `get_llm_system_context()`.
#' @param concept_path Ruta a `project_concept.md`.
#' @param style_path Ruta a `style.json`.
#' @param context_path Ruta a `context_metadata.json`.
#' @param api_key Clave de API opcional. Si no se provee, `ellmer` la busca en las variables de entorno correspondientes (`GEMINI_API_KEY`, etc.).
#' @param ... Argumentos adicionales pasados a la funcion constructora de `ellmer`.
#'
#' @return Objeto Chat de clase `ellmer::Chat`.
#' @export
#'
#' @examples
#' \dontrun{
#' chat <- geo_chat()
#' chat$chat("Cual es la mejor manera de mapear la relacion entre parques y densidad?")
#' }
geo_chat <- function(provider = c("gemini", "claude", "openai", "ollama"),
                     model = NULL,
                     system_prompt = NULL,
                     concept_path = "project_concept.md",
                     style_path = "style.json",
                     context_path = "context_metadata.json",
                     api_key = NULL,
                     ...) {
  check_ellmer()
  provider <- match.arg(provider)

  sys_prompt <- system_prompt %||% get_llm_system_context(
    concept_path = concept_path,
    style_path = style_path,
    context_path = context_path
  )

  chat_obj <- switch(
    provider,
    gemini = {
      m <- model %||% "gemini-2.5-pro"
      if (!is.null(api_key)) {
        ellmer::chat_gemini(model = m, system_prompt = sys_prompt, api_key = api_key, ...)
      } else {
        ellmer::chat_gemini(model = m, system_prompt = sys_prompt, ...)
      }
    },
    claude = {
      m <- model %||% "claude-3-7-sonnet-latest"
      if (!is.null(api_key)) {
        ellmer::chat_claude(model = m, system_prompt = sys_prompt, api_key = api_key, ...)
      } else {
        ellmer::chat_claude(model = m, system_prompt = sys_prompt, ...)
      }
    },
    openai = {
      m <- model %||% "gpt-4o"
      if (!is.null(api_key)) {
        ellmer::chat_openai(model = m, system_prompt = sys_prompt, api_key = api_key, ...)
      } else {
        ellmer::chat_openai(model = m, system_prompt = sys_prompt, ...)
      }
    },
    ollama = {
      m <- model %||% "qwen2.5-coder"
      ellmer::chat_ollama(model = m, system_prompt = sys_prompt, ...)
    }
  )

  chat_obj
}
