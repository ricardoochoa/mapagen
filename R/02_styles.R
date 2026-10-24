#' Read and validate style configuration file
#'
#' @param style_path Path to the `style.json` file. Defaults to `"style.json"`.
#' @return List with parsed style rules.
#' @export
read_style <- function(style_path = "style.json") {
  if (!file.exists(style_path)) {
    # If not found in current directory, fallback to default package template
    warning(sprintf("File '%s' not found. Using default package style configuration.", style_path), call. = FALSE)
    style_path <- get_template_path("style.json")
  }
  read_json_safe(style_path)
}

#' Generate a ggplot2 theme from a style.json file
#'
#' Translates typographic rules, background colors, and borders defined
#' in `style.json` into a `ggplot2` `theme` object.
#'
#' @param style_path Path to the `style.json` file.
#' @param base_size Optional base font size override.
#' @param base_family Optional base font family override.
#'
#' @return A `ggplot2` `theme` object.
#' @export
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' ggplot(cars, aes(speed, dist)) +
#'   geom_point() +
#'   theme_from_json()
#' }
theme_from_json <- function(style_path = "style.json", base_size = NULL, base_family = NULL) {
  cfg <- read_style(style_path)

  typo   <- cfg$typography %||% list()
  colors <- cfg$colors %||% list()
  sizes  <- typo$sizes %||% list()

  b_size   <- base_size %||% typo$base_size %||% 11
  b_family <- base_family %||% typo$family_body %||% "Helvetica"
  t_family <- typo$family_title %||% b_family

  # Colores
  c_bg        <- colors$background %||% "#FFFFFF"
  c_panel_bg  <- colors$panel_background %||% "#F8F9FA"
  c_grid      <- colors$grid_color %||% "#E9ECEF"
  c_text_main <- colors$text_main %||% "#212529"
  c_text_mute <- colors$text_muted %||% "#6C757D"
  c_border    <- colors$border_color %||% "#495057"

  # Construir tema ggplot2
  ggplot2::theme_minimal(base_size = b_size, base_family = b_family) +
    ggplot2::theme(
      # Fondos
      plot.background   = ggplot2::element_rect(fill = c_bg, color = NA),
      panel.background  = ggplot2::element_rect(fill = c_panel_bg, color = NA),
      panel.border      = ggplot2::element_rect(color = c_border, fill = NA, linewidth = 0.4),

      # Grilla
      panel.grid.major  = ggplot2::element_line(color = c_grid, linewidth = 0.3),
      panel.grid.minor  = ggplot2::element_blank(),

      # Textos
      plot.title        = ggplot2::element_text(family = t_family, face = "bold",
                                                size = sizes$plot_title %||% (b_size * 1.25),
                                                color = c_text_main, margin = ggplot2::margin(b = 6)),
      plot.subtitle     = ggplot2::element_text(family = b_family,
                                                size = sizes$plot_subtitle %||% (b_size * 0.95),
                                                color = c_text_mute, margin = ggplot2::margin(b = 10)),
      plot.caption      = ggplot2::element_text(family = b_family,
                                                size = sizes$caption %||% (b_size * 0.75),
                                                color = c_text_mute, hjust = 0,
                                                margin = ggplot2::margin(t = 8)),

      # Ejes
      axis.title        = ggplot2::element_text(size = sizes$axis_title %||% (b_size * 0.9),
                                                color = c_text_main),
      axis.text         = ggplot2::element_text(size = sizes$axis_text %||% (b_size * 0.8),
                                                color = c_text_mute),

      # Leyendas
      legend.background = ggplot2::element_rect(fill = c_bg, color = NA),
      legend.key        = ggplot2::element_blank(),
      legend.title       = ggplot2::element_text(size = b_size * 0.85, face = "bold", color = c_text_main),
      legend.text       = ggplot2::element_text(size = b_size * 0.8, color = c_text_main),

      # Margenes del lienzo
      plot.margin       = ggplot2::margin(t = 12, r = 12, b = 12, l = 12)
    )
}

#' Add standardized cartographic decorations (north arrow and scale bar)
#'
#' Reads guidelines from `style.json` and applies corresponding `ggspatial` layers.
#'
#' @param plot A `ggplot` object to decorate. If `NULL`, returns a list of layers ready to add with `+`.
#' @param style_path Path to `style.json`.
#' @param north Logical. If `TRUE`, attempts to add a north arrow if enabled in the JSON.
#' @param scale Logical. If `TRUE`, attempts to add a scale bar if enabled in the JSON.
#'
#' @return A decorated `ggplot` object or a list of `ggplot2` / `ggspatial` components.
#' @export
add_cartographic_decorations <- function(plot = NULL, style_path = "style.json", north = TRUE, scale = TRUE) {
  cfg <- read_style(style_path)
  carto <- cfg$cartography %||% list()

  decorations <- list()

  # 1. Flecha de Norte
  north_cfg <- carto$north_arrow %||% list()
  if (isTRUE(north) && isTRUE(north_cfg$enabled)) {
    loc <- north_cfg$location %||% "tl"
    px  <- north_cfg$pad_x %||% 0.5
    py  <- north_cfg$pad_y %||% 0.5
    unit_str <- north_cfg$pad_unit %||% "cm"

    # Seleccionar estilo de flecha
    n_style <- switch(
      north_cfg$style %||% "minimal",
      "minimal" = ggspatial::north_arrow_minimal(text_size = north_cfg$text_size %||% 8),
      "orienteering" = ggspatial::north_arrow_orienteering(text_size = north_cfg$text_size %||% 8),
      "fancy_orienteering" = ggspatial::north_arrow_fancy_orienteering(text_size = north_cfg$text_size %||% 8),
      ggspatial::north_arrow_minimal(text_size = north_cfg$text_size %||% 8)
    )

    decorations[[length(decorations) + 1]] <- ggspatial::annotation_north_arrow(
      location = loc,
      which_north = "true",
      pad_x = grid::unit(px, unit_str),
      pad_y = grid::unit(py, unit_str),
      style = n_style
    )
  }

  # 2. Barra de Escala
  scale_cfg <- carto$scale_bar %||% list()
  if (isTRUE(scale) && isTRUE(scale_cfg$enabled)) {
    decorations[[length(decorations) + 1]] <- ggspatial::annotation_scale(
      location = scale_cfg$location %||% "bl",
      width_hint = scale_cfg$width_hint %||% 0.25,
      unit_category = scale_cfg$unit_category %||% "metric",
      style = scale_cfg$style %||% "ticks",
      text_cex = (scale_cfg$text_size %||% 7) / 10
    )
  }

  if (is.null(plot)) {
    return(decorations)
  } else {
    for (layer in decorations) {
      plot <- plot + layer
    }
    return(plot)
  }
}

#' Retrieve color palette configured in style.json
#'
#' @param name Name of the palette: `"primary"`, `"categorical"`, `"diverging"`, `"sequential"`.
#' @param style_path Path to `style.json`.
#' @return Character vector of hexadecimal color codes.
#' @export
get_palette <- function(name = "primary", style_path = "style.json") {
  cfg <- read_style(style_path)
  palettes <- cfg$colors$palettes %||% list()
  pal <- palettes[[name]]
  if (is.null(pal) || length(pal) == 0) {
    warning(sprintf("Palette '%s' not found in style.json. Using 'primary' by default.", name), call. = FALSE)
    pal <- palettes[["primary"]] %||% c("#1D3557", "#457B9D", "#A8DADC", "#F1FAEE", "#E63946")
  }
  unlist(pal)
}

#' ggplot2 fill scales based on style.json
#'
#' @param palette Name of the palette to use.
#' @param style_path Path to `style.json`.
#' @param discrete Logical. Whether the scale is for discrete or continuous data.
#' @param ... Additional arguments passed to `scale_fill_manual` or `scale_fill_gradientn`.
#' @return A `ggplot2` scale.
#' @export
scale_fill_mapagen <- function(palette = "primary", style_path = "style.json", discrete = TRUE, ...) {
  cols <- get_palette(palette, style_path)
  if (discrete) {
    ggplot2::scale_fill_manual(values = cols, ...)
  } else {
    ggplot2::scale_fill_gradientn(colors = cols, ...)
  }
}

#' ggplot2 color scales based on style.json
#'
#' @param palette Name of the palette to use.
#' @param style_path Path to `style.json`.
#' @param discrete Logical. Whether the scale is for discrete or continuous data.
#' @param ... Additional arguments passed to `scale_color_manual` or `scale_color_gradientn`.
#' @return A `ggplot2` scale.
#' @export
scale_color_mapagen <- function(palette = "primary", style_path = "style.json", discrete = TRUE, ...) {
  cols <- get_palette(palette, style_path)
  if (discrete) {
    ggplot2::scale_color_manual(values = cols, ...)
  } else {
    ggplot2::scale_color_gradientn(colors = cols, ...)
  }
}

#' Retrieve preferred Coordinate Reference System (CRS) from style.json
#'
#' @param style_path Path to `style.json`.
#' @param type CRS type: `"projected"` (metric for calculations) or `"geographic"` (lat/lon).
#' @return Character string containing the EPSG code (e.g. `"EPSG:6362"`).
#' @export
get_preferred_crs <- function(style_path = "style.json", type = c("projected", "geographic")) {
  type <- match.arg(type)
  cfg <- read_style(style_path)
  crs_cfg <- cfg$cartography$preferred_crs %||% list()
  if (type == "projected") {
    crs_cfg$projected %||% "EPSG:6362"
  } else {
    crs_cfg$geographic %||% "EPSG:4326"
  }
}

#' Retrieve project rules and guidelines for AI agents
#'
#' Extracts style configuration (`style.json`) and project concept (`project_concept.md`)
#' to structure actionable guidelines for agents in list, JSON, or Markdown format.
#' Respects the project language configured in `style.json`.
#'
#' @param style_path Path to `style.json`.
#' @param concept_path Path to `project_concept.md`.
#' @param format Output format: `"list"`, `"json"`, or `"markdown"`.
#' @return Formatted list, JSON string, or Markdown text according to `format`.
#' @export
get_project_rules <- function(style_path = "style.json",
                              concept_path = "project_concept.md",
                              format = c("list", "json", "markdown")) {
  format <- match.arg(format)
  cfg <- read_style(style_path)

  concept_txt <- if (file.exists(concept_path)) {
    paste(readLines(concept_path, warn = FALSE), collapse = "\n")
  } else {
    "No project_concept.md file specified."
  }

  meta <- cfg$project_metadata %||% list()
  lang <- meta$language %||% "es"
  pref_proj <- get_preferred_crs(style_path, type = "projected")
  pref_geo  <- get_preferred_crs(style_path, type = "geographic")

  is_es <- tolower(lang) %in% c("es", "es_mx", "es_es")

  conventions_list <- if (is_es) {
    list(
      metric_calculations = sprintf("Usa siempre '%s' para calculos metricos (areas, buffers, distancias).", pref_proj),
      styling = "Aplica mapagen::theme_from_json() y mapagen::add_cartographic_decorations() en mapas ggplot2.",
      figure_export = "Exporta con mapagen::export_iteration(plot, id = '...', caption = '...', summary_data = ...).",
      report_inclusion = "Incluye figuras en Quarto (.qmd) usando mapagen::include_figure(id = '...')."
    )
  } else {
    list(
      metric_calculations = sprintf("Always use '%s' for metric calculations (areas, buffers, distances).", pref_proj),
      styling = "Apply mapagen::theme_from_json() and mapagen::add_cartographic_decorations() in ggplot2 maps.",
      figure_export = "Export with mapagen::export_iteration(plot, id = '...', caption = '...', summary_data = ...).",
      report_inclusion = "Include figures in Quarto (.qmd) using mapagen::include_figure(id = '...')."
    )
  }

  rules_list <- list(
    language = lang,
    concept = concept_txt,
    crs = list(
      preferred_projected = pref_proj,
      preferred_geographic = pref_geo
    ),
    style = list(
      typography = cfg$typography %||% list(),
      colors = cfg$colors %||% list()
    ),
    conventions = conventions_list
  )

  if (format == "list") {
    return(rules_list)
  }

  if (format == "json") {
    return(jsonlite::toJSON(rules_list, pretty = TRUE, auto_unbox = TRUE))
  }

  # format == "markdown"
  md <- if (is_es) {
    paste0(
      "### Directrices del Proyecto mapagen\n\n",
      "- **Idioma preferido:** ", lang, "\n",
      "- **CRS proyectado (calculos metricos):** `", pref_proj, "`\n",
      "- **CRS geografico (lat/lon):** `", pref_geo, "`\n\n",
      "#### Convenciones de Codigo para Agentes:\n",
      "1. **CRS:** Nunca calcules areas o distancias en coordenadas geograficas. Reproyecta a `", pref_proj, "` usando `sf::st_transform()`.\n",
      "2. **Estilo:** Usa `mapagen::theme_from_json()` y `mapagen::add_cartographic_decorations()` en figuras ggplot2.\n",
      "3. **Trazabilidad:** Guarda siempre con `mapagen::export_iteration(plot, id, caption, summary_data = ...)`.\n\n",
      "#### Concepto del Proyecto:\n",
      concept_txt, "\n"
    )
  } else {
    paste0(
      "### mapagen Project Guidelines\n\n",
      "- **Preferred language:** ", lang, "\n",
      "- **Projected CRS (metric calculations):** `", pref_proj, "`\n",
      "- **Geographic CRS (lat/lon):** `", pref_geo, "`\n\n",
      "#### Agent Code Conventions:\n",
      "1. **CRS:** Never calculate areas or distances in geographic coordinates. Reproject to `", pref_proj, "` using `sf::st_transform()`.\n",
      "2. **Style:** Use `mapagen::theme_from_json()` and `mapagen::add_cartographic_decorations()` in ggplot2 figures.\n",
      "3. **Traceability:** Always save using `mapagen::export_iteration(plot, id, caption, summary_data = ...)`.\n\n",
      "#### Project Concept:\n",
      concept_txt, "\n"
    )
  }

  md
}
