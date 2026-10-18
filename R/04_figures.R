#' Obtiene el registro completo de figuras del proyecto
#'
#' @param registry_path Ruta al archivo `figures_registry.json`. Por defecto `"output/figures_registry.json"`.
#' @return Lista con todas las entradas del registro de figuras.
#' @export
get_figure_registry <- function(registry_path = file.path("output", "figures_registry.json")) {
  if (!file.exists(registry_path)) {
    return(list())
  }
  read_json_safe(registry_path)
}

#' Obtiene los metadatos de la version mas reciente (o especifica) de una figura
#'
#' @param id Identificador unico de la figura (ej. `"fig_001"`).
#' @param version Numero de version o `"latest"`.
#' @param registry_path Ruta al archivo `figures_registry.json`.
#' @return Lista con los metadatos de la figura encontrada, o `NULL` si no existe.
#' @export
get_figure_entry <- function(id, version = "latest", registry_path = file.path("output", "figures_registry.json")) {
  reg <- get_figure_registry(registry_path)
  matches <- Filter(function(x) identical(x$figure_id, id), reg)

  if (length(matches) == 0) return(NULL)

  if (identical(version, "latest")) {
    versions <- sapply(matches, function(x) x$version %||% 1)
    matches[[which.max(versions)]]
  } else {
    idx <- which(sapply(matches, function(x) identical(x$version, as.integer(version))))
    if (length(idx) == 0) NULL else matches[[idx[1]]]
  }
}

#' Exporta y registra una iteracion de visualizacion grafica
#'
#' Renderiza el objeto grafico a formato PNG (y opcionalmente PDF), serializa
#' los datos numericos de soporte a JSON y registra o actualiza la entrada
#' en `figures_registry.json` para trazabilidad y ensamblaje en Quarto.
#'
#' @param plot Objeto grafico de clase `ggplot`.
#' @param id Identificador unico de la figura (ej. `"fig_001"` o `"mapa_marginacion"`).
#' @param caption Texto explicativo o pie de figura.
#' @param sources_id Vector con los identificadores de las fuentes de datos utilizadas (ej. `c("censo_2020", "directorio_parques")`).
#' @param summary_data `data.frame`, `tibble` o lista con los valores numericos o agregaciones que sustentan la figura. Si es `NULL`, se extrae un resumen del objeto `plot`.
#' @param output_dir Directorio base de salida. Por defecto `"output"`.
#' @param style_path Ruta a `style.json` para obtener dimensiones y DPI por defecto.
#' @param final Lógico. Si es `TRUE`, genera adicionalmente el archivo vectorial `.pdf` y marca el estatus como `"final"`.
#' @param width Ancho de la imagen. Si es `NULL`, se lee de `style.json`.
#' @param height Alto de la imagen. Si es `NULL`, se lee de `style.json`.
#' @param dpi Resolucion de la imagen. Si es `NULL`, se lee de `style.json`.
#' @param units Unidades de medida (`"in"`, `"cm"`, `"mm"`).
#'
#' @return Lista invisible con las rutas de los archivos generados y los metadatos registrados.
#' @export
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' p <- ggplot(cars, aes(speed, dist)) + geom_point() + theme_from_json()
#' export_iteration(p, id = "fig_001", caption = "Relacion entre velocidad y distancia",
#'                  sources_id = "cars_dataset")
#' }
export_iteration <- function(plot,
                             id,
                             caption = "",
                             sources_id = character(0),
                             summary_data = NULL,
                             output_dir = "output",
                             style_path = "style.json",
                             final = FALSE,
                             width = NULL,
                             height = NULL,
                             dpi = NULL,
                             units = NULL) {
  if (!inherits(plot, "ggplot")) {
    stop("El argumento 'plot' debe ser un objeto de clase ggplot.", call. = FALSE)
  }

  cfg <- read_style(style_path)
  export_cfg <- cfg$export %||% list()

  img_width  <- width  %||% export_cfg$width  %||% 8
  img_height <- height %||% export_cfg$height %||% 6
  img_dpi    <- dpi    %||% export_cfg$dpi    %||% 300
  img_units  <- units  %||% export_cfg$units  %||% "in"

  fig_dir <- file.path(output_dir, "figures")
  if (!dir.exists(fig_dir)) dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

  registry_path <- file.path(output_dir, "figures_registry.json")
  reg <- get_figure_registry(registry_path)

  # Determinar version de la figura
  existing <- Filter(function(x) identical(x$figure_id, id), reg)
  current_version <- if (length(existing) == 0) 1 else max(sapply(existing, function(x) x$version %||% 1)) + 1

  base_filename <- sprintf("%s_v%d", id, current_version)
  png_rel_path  <- file.path(fig_dir, paste0(base_filename, ".png"))
  data_rel_path <- file.path(fig_dir, paste0(base_filename, "_data.json"))
  pdf_rel_path  <- if (isTRUE(final)) file.path(fig_dir, paste0(base_filename, ".pdf")) else NULL

  # 1. Guardar PNG
  ggplot2::ggsave(
    filename = png_rel_path,
    plot = plot,
    width = img_width,
    height = img_height,
    dpi = img_dpi,
    units = img_units
  )
  message(sprintf("  + Imagen PNG exportada: %s", png_rel_path))

  # 2. Guardar PDF si final es TRUE
  if (isTRUE(final)) {
    ggplot2::ggsave(
      filename = pdf_rel_path,
      plot = plot,
      width = img_width,
      height = img_height,
      device = "pdf",
      units = img_units
    )
    message(sprintf("  + Imagen PDF vectorial exportada: %s", pdf_rel_path))
  }

  # 3. Serializar datos de soporte numerico
  clean_summary <- if (!is.null(summary_data)) {
    if (is.data.frame(summary_data)) {
      # Si tiene columna de geometria sf, quitarla para serializacion limpia
      if (inherits(summary_data, "sf")) {
        summary_data <- sf::st_drop_geometry(summary_data)
      }
      as.list(summary_data)
    } else {
      summary_data
    }
  } else {
    # Extraer resumen basico de las capas si no se proporcionaron datos explicitos
    list(
      note = "Datos extraidos automaticamente de la estructura de capas del grafico",
      layers_count = length(plot$layers),
      labels = plot$labels
    )
  }

  write_json_safe(clean_summary, data_rel_path)
  message(sprintf("  + Datos numericos de soporte guardados: %s", data_rel_path))

  # 4. Actualizar el registro de figuras
  entry <- list(
    figure_id = id,
    version = current_version,
    timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    caption = caption,
    sources_id = as.list(sources_id),
    status = ifelse(isTRUE(final), "final", "draft"),
    artifacts = list(
      png = png_rel_path,
      pdf = pdf_rel_path,
      data_json = data_rel_path
    )
  )

  reg[[length(reg) + 1]] <- entry
  write_json_safe(reg, registry_path)
  message(sprintf("  + Registro de figuras actualizado: %s (version %d)", registry_path, current_version))

  invisible(entry)
}

#' Helper para incluir dinamicamente figuras registradas en reportes Quarto (.qmd)
#'
#' Lee los metadatos desde `figures_registry.json`, formatea la ruta de la imagen,
#' el pie de figura (`caption`) y la cita bibliografica institucional.
#'
#' @param id Identificador unico de la figura registrada (ej. `"fig_001"`).
#' @param version Version a incluir (`"latest"` por defecto, o un numero entero).
#' @param width Ancho en formato HTML/CSS (ej. `"100%"` o `"650px"`).
#' @param registry_path Ruta a `figures_registry.json`.
#' @param style_path Ruta a `style.json` para obtener citas y configuraciones de idioma.
#'
#' @return Imprime y devuelve codigo Markdown formateado para ser interpretado con `results: asis`.
#' @export
include_figure <- function(id,
                           version = "latest",
                           width = "100%",
                           registry_path = file.path("output", "figures_registry.json"),
                           style_path = "style.json") {
  entry <- get_figure_entry(id, version = version, registry_path = registry_path)

  if (is.null(entry)) {
    txt <- sprintf("\n> **[mapagen aviso]:** No se encontro la figura `%s` (version: %s) en el registro `%s`.\n",
                   id, as.character(version), registry_path)
    cat(txt)
    return(invisible(txt))
  }

  cfg <- read_style(style_path)
  meta <- cfg$project_metadata %||% list()
  default_citation <- meta$citation_default %||% "Fuente: Elaboracion propia."

  sources_list <- unlist(entry$sources_id)
  source_txt <- if (length(sources_list) > 0) {
    paste0("Fuente: ", paste(sources_list, collapse = ", "), ".")
  } else {
    default_citation
  }

  png_path <- entry$artifacts$png %||% ""
  caption_txt <- entry$caption %||% ""
  fig_v <- entry$version %||% 1

  # Generar bloque Markdown para Quarto con atributos de figura
  md_output <- sprintf(
    "\n::: {#fig-%s layout-align=\"center\"}\n![](%s){width=%s}\n\n**Figura %s (v%d):** %s  \n*<small>%s</small>*\n:::\n",
    id, png_path, width, id, fig_v, caption_txt, source_txt
  )

  cat(md_output)
  invisible(md_output)
}
