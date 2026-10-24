#' Retrieve full figure registry of the project
#'
#' @param registry_path Path to `figures_registry.json`. Defaults to `"output/figures_registry.json"`.
#' @return List of all figure registry entries.
#' @export
get_figure_registry <- function(registry_path = file.path("output", "figures_registry.json")) {
  if (!file.exists(registry_path)) {
    return(list())
  }
  read_json_safe(registry_path)
}

#' Retrieve metadata for a specific figure version
#'
#' @param id Unique identifier of the figure (e.g. `"fig_001"`).
#' @param version Version number or `"latest"`.
#' @param registry_path Path to `figures_registry.json`.
#' @return List with figure metadata, or `NULL` if not found.
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

#' Export and register a visualization iteration
#'
#' Renders a `ggplot` graphic to PNG (and optionally PDF), serializes supporting
#' numerical data to JSON, and logs or updates an entry in `figures_registry.json`
#' for analytical traceability and Quarto assembly.
#'
#' @param plot A `ggplot` graphic object.
#' @param id Unique figure identifier (e.g. `"fig_001"` or `"walkability_map"`).
#' @param caption Explanatory figure caption.
#' @param sources_id Vector of data source identifiers (e.g. `c("census_2020", "parks")`).
#' @param summary_data A `data.frame`, `tibble`, or list of supporting numerical values. If `NULL`, extracts a basic summary from the plot layers.
#' @param output_dir Output base directory. Defaults to `"output"`.
#' @param style_path Path to `style.json` to obtain default dimensions and DPI.
#' @param final Logical. If `TRUE`, additionally generates a vector `.pdf` and marks status as `"final"`.
#' @param width Image width. If `NULL`, read from `style.json`.
#' @param height Image height. If `NULL`, read from `style.json`.
#' @param dpi Image resolution (DPI). If `NULL`, read from `style.json`.
#' @param units Measurement units (`"in"`, `"cm"`, `"mm"`).
#'
#' @return Invisible list with exported artifact paths and registered metadata.
#' @export
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' p <- ggplot(cars, aes(speed, dist)) + geom_point() + theme_from_json()
#' export_iteration(p, id = "fig_001", caption = "Speed vs Distance", sources_id = "cars")
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
    stop("The 'plot' argument must be an object of class ggplot.", call. = FALSE)
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
  message(sprintf("  + PNG image exported: %s", png_rel_path))

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
    message(sprintf("  + Vector PDF image exported: %s", pdf_rel_path))
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
      note = "Data automatically extracted from plot layer structure",
      layers_count = length(plot$layers),
      labels = plot$labels
    )
  }

  write_json_safe(clean_summary, data_rel_path)
  message(sprintf("  + Supporting numerical data saved: %s", data_rel_path))

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
  message(sprintf("  + Figure registry updated: %s (version %d)", registry_path, current_version))

  invisible(entry)
}

#' Helper to dynamically embed registered figures in Quarto reports (.qmd)
#'
#' Reads metadata from `figures_registry.json`, formatting the image path,
#' caption, and institutional citation based on `style.json` language preferences.
#'
#' @param id Unique figure identifier (e.g. `"fig_001"`).
#' @param version Version to embed (`"latest"` by default, or an integer).
#' @param width Width in HTML/CSS format (e.g. `"100%"` or `"650px"`).
#' @param registry_path Path to `figures_registry.json`.
#' @param style_path Path to `style.json`.
#'
#' @return Prints and returns formatted Markdown code for `results: asis` chunks.
#' @export
include_figure <- function(id,
                           version = "latest",
                           width = "100%",
                           registry_path = file.path("output", "figures_registry.json"),
                           style_path = "style.json") {
  entry <- get_figure_entry(id, version = version, registry_path = registry_path)

  if (is.null(entry)) {
    txt <- sprintf("\n> **[mapagen notice]:** Figure '%s' (version: %s) was not found in registry '%s'.\n",
                   id, as.character(version), registry_path)
    cat(txt)
    return(invisible(txt))
  }

  cfg <- read_style(style_path)
  meta <- cfg$project_metadata %||% list()
  lang <- tolower(meta$language %||% "es")
  is_es <- lang %in% c("es", "es_mx", "es_es")

  default_citation <- meta$citation_default %||% if (is_es) "Fuente: Elaboracion propia." else "Source: Author's own elaboration."

  sources_list <- unlist(entry$sources_id)
  source_txt <- if (length(sources_list) > 0) {
    prefix <- if (is_es) "Fuente: " else "Source: "
    paste0(prefix, paste(sources_list, collapse = ", "), ".")
  } else {
    default_citation
  }

  png_path <- entry$artifacts$png %||% ""
  caption_txt <- entry$caption %||% ""
  fig_v <- entry$version %||% 1
  label_txt <- if (is_es) "Figura" else "Figure"

  # Generar bloque Markdown para Quarto con atributos de figura
  md_output <- sprintf(
    "\n::: {#fig-%s layout-align=\"center\"}\n![](%s){width=%s}\n\n**%s %s (v%d):** %s  \n*<small>%s</small>*\n:::\n",
    id, png_path, width, label_txt, id, fig_v, caption_txt, source_txt
  )

  cat(md_output)
  invisible(md_output)
}

#' List all figures registered in the project
#'
#' Returns a data.frame summarizing all iterations and versions
#' registered in `figures_registry.json`.
#'
#' @param registry_path Path to `figures_registry.json`.
#' @return A `data.frame` with columns: `figure_id`, `version`, `status`, `caption`, `timestamp`, `has_png`, `has_pdf`, `has_data`.
#' @export
list_figures <- function(registry_path = file.path("output", "figures_registry.json")) {
  empty_df <- data.frame(
    figure_id = character(0),
    version = integer(0),
    status = character(0),
    caption = character(0),
    timestamp = character(0),
    has_png = logical(0),
    has_pdf = logical(0),
    has_data = logical(0),
    stringsAsFactors = FALSE
  )

  reg <- get_figure_registry(registry_path)
  if (length(reg) == 0) return(empty_df)

  rows <- lapply(reg, function(entry) {
    art <- entry$artifacts %||% list()
    data.frame(
      figure_id = entry$figure_id %||% "",
      version = as.integer(entry$version %||% 1),
      status = entry$status %||% "draft",
      caption = entry$caption %||% "",
      timestamp = entry$timestamp %||% "",
      has_png = file.exists(art$png %||% ""),
      has_pdf = file.exists(art$pdf %||% ""),
      has_data = file.exists(art$data_json %||% ""),
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, rows)
}

#' Retrieve supporting numerical data for a registered figure
#'
#' Reads and deserializes the `_data.json` artifact generated alongside the figure.
#'
#' @param id Unique figure identifier (e.g. `"fig_001"`).
#' @param version Version of the figure (`"latest"` or an integer).
#' @param registry_path Path to `figures_registry.json`.
#' @return List with `success`, `figure_id`, `version`, `data_path`, and `data` (parsed content).
#' @export
get_figure_data <- function(id, version = "latest", registry_path = file.path("output", "figures_registry.json")) {
  entry <- get_figure_entry(id, version = version, registry_path = registry_path)
  if (is.null(entry)) {
    return(list(
      success = FALSE,
      error = sprintf("Figure '%s' was not found in registry: %s", id, registry_path)
    ))
  }

  data_path <- entry$artifacts$data_json %||% ""
  if (!file.exists(data_path)) {
    return(list(
      success = FALSE,
      error = sprintf("Supporting numerical data file does not exist for '%s': %s", id, data_path)
    ))
  }

  content <- read_json_safe(data_path)
  list(
    success = TRUE,
    figure_id = id,
    version = entry$version %||% 1,
    data_path = data_path,
    data = content
  )
}

#' Update figure metadata in the registry
#'
#' Updates caption, sources, or publication status (`draft`, `reviewed`, `final`)
#' for an existing figure iteration without re-rendering image files.
#'
#' @param id Unique figure identifier.
#' @param version Version to update (`"latest"` or an integer).
#' @param caption New caption text (optional).
#' @param sources_id New vector of data sources (optional).
#' @param status New status string (`"draft"`, `"reviewed"`, `"final"`, optional).
#' @param registry_path Path to `figures_registry.json`.
#' @return List with `success` and the updated entry.
#' @export
update_figure_metadata <- function(id,
                                   version = "latest",
                                   caption = NULL,
                                   sources_id = NULL,
                                   status = NULL,
                                   registry_path = file.path("output", "figures_registry.json")) {
  reg <- get_figure_registry(registry_path)
  if (length(reg) == 0) {
    return(list(success = FALSE, error = "Figure registry is empty or does not exist."))
  }

  target_entry <- get_figure_entry(id, version = version, registry_path = registry_path)
  if (is.null(target_entry)) {
    return(list(success = FALSE, error = sprintf("Figure '%s' (version: %s) was not found.", id, as.character(version))))
  }

  target_v <- target_entry$version
  idx <- which(sapply(reg, function(x) identical(x$figure_id, id) && identical(x$version, target_v)))

  if (length(idx) == 0) {
    return(list(success = FALSE, error = "Error locating figure entry index."))
  }

  entry_idx <- idx[1]
  if (!is.null(caption)) reg[[entry_idx]]$caption <- caption
  if (!is.null(sources_id)) reg[[entry_idx]]$sources_id <- as.list(sources_id)
  if (!is.null(status)) reg[[entry_idx]]$status <- status

  write_json_safe(reg, registry_path)
  list(success = TRUE, entry = reg[[entry_idx]])
}

#' Delete a figure version from the registry and optionally delete files
#'
#' @param id Unique figure identifier.
#' @param version Version to delete (`"latest"` or an integer).
#' @param delete_files Logical. If `TRUE`, physically deletes associated `.png`, `.pdf`, and `_data.json` files.
#' @param registry_path Path to `figures_registry.json`.
#' @return List with `success`, details of deleted version, and list of deleted files.
#' @export
delete_figure_version <- function(id,
                                  version = "latest",
                                  delete_files = FALSE,
                                  registry_path = file.path("output", "figures_registry.json")) {
  reg <- get_figure_registry(registry_path)
  if (length(reg) == 0) {
    return(list(success = FALSE, error = "Figure registry is empty or does not exist."))
  }

  target_entry <- get_figure_entry(id, version = version, registry_path = registry_path)
  if (is.null(target_entry)) {
    return(list(success = FALSE, error = sprintf("Figure '%s' (version: %s) was not found.", id, as.character(version))))
  }

  target_v <- target_entry$version
  deleted_files <- character(0)

  if (isTRUE(delete_files)) {
    for (art_path in unlist(target_entry$artifacts)) {
      if (nzchar(art_path) && file.exists(art_path)) {
        unlink(art_path)
        deleted_files <- c(deleted_files, art_path)
      }
    }
  }

  new_reg <- Filter(function(x) !(identical(x$figure_id, id) && identical(x$version, target_v)), reg)
  write_json_safe(new_reg, registry_path)

  list(
    success = TRUE,
    figure_id = id,
    deleted_version = target_v,
    files_deleted = deleted_files,
    remaining_entries = length(new_reg)
  )
}
