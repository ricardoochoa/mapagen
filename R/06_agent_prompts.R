#' Extrae bloques de codigo R de una respuesta de texto generada por LLM
#'
#' @param text Cadena de texto con la respuesta del LLM.
#' @return Vector de caracteres con el codigo R limpio.
#' @export
extract_r_code <- function(text) {
  # Regex para capturar bloques ```r o ```R multilínea
  pattern <- "(?s)```[rR]?[ \\t]*\\n(.*?)\\n```"
  matches <- gregexpr(pattern, text, perl = TRUE)
  extracted <- regmatches(text, matches)[[1]]

  if (length(extracted) == 0) return("")

  # Limpiar las etiquetas ``` iniciales y finales
  cleaned <- sapply(extracted, function(block) {
    sub("^```[a-zA-Z]*[ \\t]*\\n", "", sub("\\n```$", "", block))
  }, USE.NAMES = FALSE)

  paste(cleaned, collapse = "\n\n")
}

#' Solicita al LLM codigo R para analisis geoespacial o visualizacion
#'
#' Envia el requerimiento del usuario a Gemini (o al proveedor configurado),
#' inyectando automaticamente el catalogo de datos y reglas de estilo.
#'
#' @param prompt Instruccion en lenguaje natural (ej. `"Genera un mapa coropletico de marginacion"`).
#' @param chat Objeto `ellmer::Chat` opcional. Si es `NULL`, se inicializa uno nuevo con `geo_chat()`.
#' @param provider Proveedor a usar si se crea un nuevo chat (`"gemini"` por defecto).
#' @param ... Argumentos adicionales pasados a `geo_chat()`.
#'
#' @return Lista con el texto explicativo y el codigo R extraido.
#' @export
#'
#' @examples
#' \dontrun{
#' res <- ask_code("Calcula el area verde por habitante en cada manzana y genera un mapa")
#' cat(res$code)
#' }
ask_code <- function(prompt, chat = NULL, provider = "gemini", ...) {
  check_ellmer()
  if (is.null(chat)) {
    chat <- geo_chat(provider = provider, ...)
  }

  full_prompt <- sprintf(
    "%s\n\n[REQUISITO]: Proporciona el codigo R ejecutable completo usando sf, terra y ggplot2, aplicando `mapagen::theme_from_json()` y guardando con `mapagen::export_iteration()`.",
    prompt
  )

  response_txt <- chat$chat(full_prompt)
  code_extracted <- extract_r_code(response_txt)

  structure(
    list(
      response = response_txt,
      code = code_extracted,
      chat = chat
    ),
    class = "mapagen_code_response"
  )
}

#' Imprime de forma legible la respuesta de ask_code
#' @param x Objeto de clase `mapagen_code_response`.
#' @param ... Argumentos adicionales pasados a `print`.
#' @return Retorna el objeto `x` de forma invisible.
#' @export
print.mapagen_code_response <- function(x, ...) {
  cat(x$response)
  invisible(x)
}

#' Solicita al LLM una narrativa analitica y tecnica a partir de los datos de una figura
#'
#' Lee el archivo `.json` de datos numericos de soporte generado por `export_iteration()`
#' y solicita a Gemini una interpretacion rigurosa para incorporar en reportes tecnicos.
#'
#' @param id Identificador unico de la figura registrada (ej. `"fig_001"`).
#' @param version Version de la figura (`"latest"` por defecto).
#' @param chat Objeto `ellmer::Chat` opcional.
#' @param prompt_extra Instrucciones adicionales para orientar el analisis.
#' @param registry_path Ruta a `figures_registry.json`.
#' @param provider Proveedor LLM (`"gemini"` por defecto).
#' @param ... Argumentos adicionales pasados a `geo_chat()`.
#'
#' @return Cadena de texto con la narrativa analitica sugerida.
#' @export
ask_narrative <- function(id,
                          version = "latest",
                          chat = NULL,
                          prompt_extra = NULL,
                          registry_path = file.path("output", "figures_registry.json"),
                          provider = "gemini",
                          ...) {
  check_ellmer()
  entry <- get_figure_entry(id, version = version, registry_path = registry_path)

  if (is.null(entry)) {
    stop(sprintf("No se encontro la figura '%s' en el registro: %s", id, registry_path), call. = FALSE)
  }

  data_path <- entry$artifacts$data_json %||% ""
  if (!file.exists(data_path)) {
    stop(sprintf("No existe el archivo de datos numericos de la figura: %s", data_path), call. = FALSE)
  }

  data_content <- paste(readLines(data_path, warn = FALSE), collapse = "\n")

  if (is.null(chat)) {
    chat <- geo_chat(provider = provider, ...)
  }

  extra_str <- if (!is.null(prompt_extra)) paste0("\nEnfasis solicitado: ", prompt_extra) else ""

  narrative_prompt <- sprintf(
    "Analiza los siguientes datos numericos exactos correspondientes a la Figura '%s' (v%s - '%s'):\n\n```json\n%s\n```\n%s\n\nRedacta un parrafo analitico y riguroso para un reporte tecnico o publicacion cientifica. Destaca asimetrias, valores extremos, patrones espaciales y conclusiones clave.",
    id, as.character(entry$version %||% 1), entry$caption %||% "", data_content, extra_str
  )

  chat$chat(narrative_prompt)
}

#' Realiza una auditoria visual (Vision AI) de una figura contra las reglas de style.json
#'
#' Envia el archivo `.png` renderizado junto con las reglas de estilo a Gemini
#' para evaluar contraste, legibilidad de etiquetas, composicion y cumplimiento de marca.
#'
#' @param id_or_path Identificador de la figura en el registro o ruta directa al archivo `.png`.
#' @param chat Objeto `ellmer::Chat` opcional (debe soportar vision, como Gemini).
#' @param style_path Ruta a `style.json`.
#' @param registry_path Ruta a `figures_registry.json`.
#' @param provider Proveedor LLM (`"gemini"` por defecto).
#' @param ... Argumentos adicionales pasados a `geo_chat()`.
#'
#' @return Diagnostico visual detallado con recomendaciones de ajuste en ggplot2.
#' @export
audit_figure <- function(id_or_path,
                         chat = NULL,
                         style_path = "style.json",
                         registry_path = file.path("output", "figures_registry.json"),
                         provider = "gemini",
                         ...) {
  check_ellmer()

  # Resolver ruta de imagen
  png_file <- if (file.exists(id_or_path) && grepl("\\.png$", id_or_path, ignore.case = TRUE)) {
    id_or_path
  } else {
    entry <- get_figure_entry(id_or_path, version = "latest", registry_path = registry_path)
    if (is.null(entry) || !file.exists(entry$artifacts$png %||% "")) {
      stop(sprintf("No se encontro el archivo de imagen PNG para '%s'", id_or_path), call. = FALSE)
    }
    entry$artifacts$png
  }

  style_txt <- if (file.exists(style_path)) {
    paste(readLines(style_path, warn = FALSE), collapse = "\n")
  } else {
    "{}"
  }

  if (is.null(chat)) {
    chat <- geo_chat(provider = provider, ...)
  }

  audit_prompt <- sprintf(
    "Eres una experta en diseno de informacion, accesibilidad visual y cartografia tematica.\n" %s%
    "Revisa la imagen adjunta contra las especificaciones del manual de estilo (style.json):\n\n" %s%
    "```json\n%s\n```\n\n" %s%
    "Evalua minuciosamente:\n" %s%
    "1. Contraste de colores y legibilidad contra fondos (apto para daltonismo y fondos claros).\n" %s%
    "2. Solapamiento de etiquetas de texto, tamano de tipografias en ejes, titulos y leyendas.\n" %s%
    "3. Presencia y estetica de los elementos cartograficos (flecha de norte y barra de escala).\n" %s%
    "4. Propuestas exactas de codigo ggplot2 para corregir cualquier discrepancia o problema detectado.",
    "", "", style_txt, "", "", "", "", ""
  )

  # Enviar imagen multimodal a ellmer
  img_content <- ellmer::content_image_file(png_file)
  chat$chat(list(audit_prompt, img_content))
}
