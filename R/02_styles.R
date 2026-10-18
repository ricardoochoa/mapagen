#' Lee y valida el archivo de configuracion de estilos
#'
#' @param style_path Ruta al archivo `style.json`. Por defecto `"style.json"`.
#' @return Lista con las reglas de estilo parseadas.
#' @export
read_style <- function(style_path = "style.json") {
  if (!file.exists(style_path)) {
    # Si no se encuentra en el directorio actual, buscar en la plantilla por defecto
    warning(sprintf("No se encontro '%s'. Usando configuracion de estilo por defecto.", style_path), call. = FALSE)
    style_path <- get_template_path("style.json")
  }
  read_json_safe(style_path)
}

#' Genera un tema de ggplot2 a partir de un archivo style.json
#'
#' Traduce las propiedades tipograficas, colores de fondo y bordes
#' definidos en `style.json` a un objeto `theme` de `ggplot2`.
#'
#' @param style_path Ruta al archivo `style.json`.
#' @param base_size Tamano base de fuente opcional para sobreescribir el del JSON.
#' @param base_family Familia tipografica base opcional.
#'
#' @return Objeto de clase `theme` de `ggplot2`.
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

#' Agrega elementos cartograficos estandarizados (flecha de norte y barra de escala)
#'
#' Lee las directrices de `style.json` y aplica las capas de `ggspatial` correspondientes.
#'
#' @param plot Objeto `ggplot` al que se anadiran los elementos. Si es `NULL`, devuelve una lista de capas lista para sumar `+`.
#' @param style_path Ruta al archivo `style.json`.
#' @param north Lógico. Si es `TRUE`, intenta agregar la flecha de norte si esta habilitada en el JSON.
#' @param scale Lógico. Si es `TRUE`, intenta agregar la escala grafica si esta habilitada en el JSON.
#'
#' @return Lista de componentes `ggplot2` / `ggspatial`.
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

#' Obtiene la paleta de colores configurada en style.json
#'
#' @param name Nombre de la paleta: `"primary"`, `"categorical"`, `"diverging"`, `"sequential"`.
#' @param style_path Ruta al archivo `style.json`.
#' @return Vector de caracteres con codigos hexadecimales.
#' @export
get_palette <- function(name = "primary", style_path = "style.json") {
  cfg <- read_style(style_path)
  palettes <- cfg$colors$palettes %||% list()
  pal <- palettes[[name]]
  if (is.null(pal) || length(pal) == 0) {
    warning(sprintf("Paleta '%s' no encontrada en style.json. Usando 'primary' por defecto.", name), call. = FALSE)
    pal <- palettes[["primary"]] %||% c("#1D3557", "#457B9D", "#A8DADC", "#F1FAEE", "#E63946")
  }
  unlist(pal)
}

#' Escalas de relleno de ggplot2 basadas en style.json
#'
#' @param palette Nombre de la paleta a utilizar.
#' @param style_path Ruta a `style.json`.
#' @param discrete Lógico. ¿Es para variable discreta o continua?
#' @param ... Argumentos adicionales pasados a `scale_fill_manual` o `scale_fill_gradientn`.
#' @return Escala de `ggplot2`.
#' @export
scale_fill_mapagen <- function(palette = "primary", style_path = "style.json", discrete = TRUE, ...) {
  cols <- get_palette(palette, style_path)
  if (discrete) {
    ggplot2::scale_fill_manual(values = cols, ...)
  } else {
    ggplot2::scale_fill_gradientn(colors = cols, ...)
  }
}

#' Escalas de color (lineas/puntos) de ggplot2 basadas en style.json
#'
#' @param palette Nombre de la paleta a utilizar.
#' @param style_path Ruta a `style.json`.
#' @param discrete Lógico. ¿Es para variable discreta o continua?
#' @param ... Argumentos adicionales pasados a `scale_color_manual` o `scale_color_gradientn`.
#' @return Escala de `ggplot2`.
#' @export
scale_color_mapagen <- function(palette = "primary", style_path = "style.json", discrete = TRUE, ...) {
  cols <- get_palette(palette, style_path)
  if (discrete) {
    ggplot2::scale_color_manual(values = cols, ...)
  } else {
    ggplot2::scale_color_gradientn(colors = cols, ...)
  }
}

#' Obtiene el CRS preferido configurado en style.json
#'
#' @param style_path Ruta a `style.json`.
#' @param type Tipo de CRS: `"projected"` (metrico para calculos) o `"geographic"` (lat/lon).
#' @return Cadena con el codigo EPSG (ej. `"EPSG:6362"`).
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
