#' Calculate size in megabytes of a file or directory
#'
#' @param path Path to the file or directory (e.g. a .gdb folder).
#' @return Number representing the size in MB rounded to 2 decimal places.
#' @export
get_path_size_mb <- function(path) {
  if (dir.exists(path)) {
    archivos <- list.files(path, full.names = TRUE, recursive = TRUE)
    tamanio <- sum(file.info(archivos)$size, na.rm = TRUE)
  } else {
    tamanio <- file.info(path)$size
  }
  round(tamanio / (1024^2), 2)
}

#' Fast inventory of project data files
#'
#' Performs a lightweight scan of data files in a directory without reading
#' geometries or heavy rasters into memory.
#'
#' @param dir_path Path to the directory to inspect. Defaults to `"data"`.
#' @return A `data.frame` with columns: `file_path`, `file_name`, `extension`, `type`, `size_mb`.
#' @export
list_project_data <- function(dir_path = "data") {
  empty_df <- data.frame(
    file_path = character(0),
    file_name = character(0),
    extension = character(0),
    type = character(0),
    size_mb = numeric(0),
    stringsAsFactors = FALSE
  )

  if (!dir.exists(dir_path)) {
    return(empty_df)
  }

  vector_exts  <- c("shp", "kml", "geojson", "gpkg", "gdb")
  raster_exts  <- c("tif", "tiff", "asc")
  tabular_exts <- c("csv", "xlsx", "xls")
  text_exts    <- c("txt", "md", "html", "json")

  all_dirs <- list.dirs(dir_path, recursive = TRUE, full.names = TRUE)
  gdb_dirs <- all_dirs[grepl("\\.gdb$", tolower(all_dirs))]

  all_files <- list.files(dir_path, recursive = TRUE, full.names = TRUE)
  archivos_validos <- all_files[!grepl("\\.gdb/", tolower(all_files))]

  items <- unique(c(gdb_dirs, archivos_validos))
  items <- items[!grepl("context_metadata", basename(items))]

  if (length(items) == 0) {
    return(empty_df)
  }

  rows <- lapply(items, function(item) {
    ext <- tolower(tools::file_ext(item))
    tipo <- if (ext %in% vector_exts) {
      "vectorial"
    } else if (ext %in% raster_exts) {
      "raster"
    } else if (ext %in% tabular_exts) {
      "tabular"
    } else if (ext %in% text_exts) {
      "documento"
    } else {
      "otro"
    }

    data.frame(
      file_path = item,
      file_name = basename(item),
      extension = ext,
      type = tipo,
      size_mb = get_path_size_mb(item),
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, rows)
}

#' Extract column summary schema from a data.frame or tibble
#'
#' @param df A data.frame or tibble.
#' @return A list with schema information for each column (name, type, NA percentage, sample values).
#' @export
extract_column_schema <- function(df) {
  lapply(names(df), function(col_name) {
    col_data <- df[[col_name]]
    list(
      column = col_name,
      type = paste(class(col_data), collapse = ", "),
      na_percentage = round(sum(is.na(col_data)) / max(length(col_data), 1) * 100, 2),
      sample_values = as.character(utils::head(unique(stats::na.omit(col_data)), 3))
    )
  })
}

#' Scan and extract metadata from spatial vector files or objects
#'
#' Supports vector files (.shp, .geojson, .gpkg, .kml, .gdb) or in-memory `sf`/`sfc` objects.
#'
#' @param x Path to a spatial vector file/directory or an `sf`/`sfc` object.
#' @return Structured list containing layer metadata, CRS, geometries, and attribute schema.
#' @export
scan_vector <- function(x) {
  if (inherits(x, c("sf", "sfc"))) {
    sf_obj <- if (inherits(x, "sfc")) sf::st_sf(geometry = x) else x
    sample_data <- if (nrow(sf_obj) > 50) utils::head(sf_obj, 50) else sf_obj
    bbox <- sf::st_bbox(sf_obj)
    df_no_geom <- sf::st_drop_geometry(sample_data)
    crs_obj <- sf::st_crs(sf_obj)
    is_proj <- tryCatch(sf::st_is_longlat(sf_obj) == FALSE, error = function(e) NA)

    return(list(
      success = TRUE,
      type = "vectorial",
      source = "in_memory",
      total_layers = 1,
      layers = list(
        list(
          layer_name = "in_memory",
          features_count = nrow(sf_obj),
          spatial_metadata = list(
            crs = ifelse(is.na(crs_obj$input) || crs_obj$input == "", "Desconocido", crs_obj$input),
            crs_epsg = crs_obj$epsg,
            is_projected = is_proj,
            units = ifelse(isTRUE(is_proj), "metros (proyectado)", "grados decimales (geografico)"),
            geometry_type = as.character(unique(sf::st_geometry_type(sf_obj))),
            bbox = list(xmin = unname(bbox["xmin"]), ymin = unname(bbox["ymin"]),
                        xmax = unname(bbox["xmax"]), ymax = unname(bbox["ymax"]))
          ),
          attributes_schema = extract_column_schema(df_no_geom)
        )
      )
    ))
  }

  file_path <- as.character(x)
  if (!file.exists(file_path)) {
    return(list(success = FALSE, type = "vectorial", error = sprintf("File does not exist: %s", file_path)))
  }

  layers_info <- tryCatch(sf::st_layers(file_path), error = function(e) NULL)

  if (is.null(layers_info) || length(layers_info$name) == 0) {
    return(list(success = FALSE, type = "vectorial", error = "No spatial layers found or invalid file"))
  }

  layers_metadata <- list()

  for (i in seq_along(layers_info$name)) {
    layer_name <- layers_info$name[i]
    features_count <- layers_info$features[i]

    # Leer muestra ligera para esquemas y CRS (max 50 filas)
    sample_data <- tryCatch(
      sf::read_sf(file_path, layer = layer_name, as_tibble = TRUE, n_max = 50, quiet = TRUE),
      error = function(e) {
        tryCatch(
          sf::read_sf(file_path, layer = layer_name, as_tibble = TRUE, quiet = TRUE),
          error = function(e2) NULL
        )
      }
    )

    if (!is.null(sample_data) && nrow(sample_data) > 50) {
      sample_data <- utils::head(sample_data, 50)
    }

    if (is.null(sample_data) || nrow(sample_data) == 0) {
      layers_metadata[[layer_name]] <- list(
        layer_name = layer_name,
        error = "Layer is empty or an error occurred while reading geometry sample"
      )
      next
    }

    bbox <- sf::st_bbox(sample_data)
    df_no_geom <- sf::st_drop_geometry(sample_data)
    crs_obj <- sf::st_crs(sample_data)
    is_proj <- tryCatch(sf::st_is_longlat(sample_data) == FALSE, error = function(e) NA)

    layers_metadata[[layer_name]] <- list(
      layer_name = layer_name,
      features_count = ifelse(is.na(features_count), "Unknown", features_count),
      spatial_metadata = list(
        crs = ifelse(is.na(crs_obj$input) || crs_obj$input == "", "Desconocido", crs_obj$input),
        crs_epsg = crs_obj$epsg,
        is_projected = is_proj,
        units = ifelse(isTRUE(is_proj), "metros (proyectado)", "grados decimales (geografico)"),
        geometry_type = as.character(unique(sf::st_geometry_type(sample_data))),
        bbox = list(xmin = unname(bbox["xmin"]), ymin = unname(bbox["ymin"]),
                    xmax = unname(bbox["xmax"]), ymax = unname(bbox["ymax"]))
      ),
      attributes_schema = extract_column_schema(df_no_geom)
    )
  }

  list(
    success = TRUE,
    type = "vectorial",
    source = file_path,
    total_layers = length(layers_info$name),
    layers = unname(layers_metadata)
  )
}

#' Scan and extract metadata from raster files or objects
#'
#' Inspects spatial raster headers using `terra` without loading full data into RAM.
#' Supports file paths or in-memory `SpatRaster` objects.
#'
#' @param x Path to a raster file (.tif, .asc, etc.) or a `SpatRaster` object.
#' @return List with resolution, dimensions, extent, bands, nodata, and CRS.
#' @export
scan_raster <- function(x) {
  r <- if (inherits(x, "SpatRaster")) {
    x
  } else {
    file_path <- as.character(x)
    if (!file.exists(file_path)) {
      return(list(success = FALSE, type = "raster", error = sprintf("Raster file does not exist: %s", file_path)))
    }
    tryCatch(terra::rast(file_path), error = function(e) NULL)
  }

  if (is.null(r)) {
    return(list(success = FALSE, type = "raster", error = "Could not read raster file or object"))
  }

  crs_desc <- tryCatch(terra::crs(r, describe = TRUE), error = function(e) list())
  is_proj <- tryCatch(!terra::is.lonlat(r), error = function(e) NA)
  nodat <- tryCatch(as.list(terra::NAflag(r)), error = function(e) NULL)

  list(
    success = TRUE,
    type = "raster",
    spatial_metadata = list(
      crs = crs_desc$name %||% "Desconocido",
      crs_code = crs_desc$code %||% NA,
      is_projected = is_proj,
      resolution = as.list(terra::res(r)),
      dimensions = list(x = terra::ncol(r), y = terra::nrow(r), bands = terra::nlyr(r)),
      bbox = as.list(as.vector(terra::ext(r))),
      nodata = nodat
    ),
    band_names = names(r)
  )
}

#' Scan tabular files or data frame objects
#'
#' @param x Path to a tabular file (.csv, .xlsx, .xls) or in-memory data.frame/tibble.
#' @param ext File extension in lowercase (when x is a file path).
#' @return List containing column schemas and row samples.
#' @export
scan_tabular <- function(x, ext = if (is.character(x)) tools::file_ext(x) else "") {
  if (is.data.frame(x)) {
    sample_data <- if (nrow(x) > 50) utils::head(x, 50) else x
    return(list(
      success = TRUE,
      type = "tabular",
      source = "in_memory",
      rows_sample = nrow(sample_data),
      attributes_schema = extract_column_schema(sample_data)
    ))
  }

  file_path <- as.character(x)
  if (!file.exists(file_path)) {
    return(list(success = FALSE, type = "tabular", error = sprintf("File does not exist: %s", file_path)))
  }

  ext <- tolower(ext)
  sample_data <- tryCatch({
    if (ext %in% c("csv")) {
      readr::read_csv(file_path, n_max = 50, show_col_types = FALSE)
    } else if (ext %in% c("xlsx", "xls")) {
      readxl::read_excel(file_path, n_max = 50)
    }
  }, error = function(e) NULL)

  if (is.null(sample_data)) {
    return(list(success = FALSE, type = "tabular", error = "Error reading tabular file"))
  }

  list(
    success = TRUE,
    type = "tabular",
    source = file_path,
    rows_sample = nrow(sample_data),
    attributes_schema = extract_column_schema(sample_data)
  )
}

#' Scan text or documentation files
#'
#' @param file_path Path to the text file (.txt, .md, .json, .html).
#' @param max_chars Maximum number of characters to read before truncating.
#' @return List with truncated text content and truncation status.
#' @export
scan_text <- function(file_path, max_chars = 5000) {
  if (!file.exists(file_path)) {
    return(list(success = FALSE, type = "documento", error = sprintf("File does not exist: %s", file_path)))
  }

  text_content <- tryCatch(readLines(file_path, warn = FALSE), error = function(e) "")
  collapsed_text <- paste(text_content, collapse = "\n")
  is_truncated <- nchar(collapsed_text) > max_chars
  if (is_truncated) {
    collapsed_text <- paste0(substr(collapsed_text, 1, max_chars), "\n... [TRUNCATED FOR TOKEN SAFETY]")
  }
  list(success = TRUE, type = "documento", truncated = is_truncated, content = collapsed_text)
}

#' Build project data catalog (geospatial repomix)
#'
#' Recursively scans a data directory (defaults to `data/`), inspects vector,
#' raster, tabular, and document files, and generates `context_metadata.json`.
#'
#' @param dir_path Path to the directory to scan. Defaults to `"data"`.
#' @param output_file Output JSON file path. Defaults to `"context_metadata.json"`.
#' @param include_non_supported Logical. If `TRUE`, logs unsupported files to an auxiliary JSON file.
#'
#' @return Invisible list containing the consolidated catalog.
#' @export
#'
#' @examples
#' \dontrun{
#' build_context("data")
#' }
build_context <- function(dir_path = "data", output_file = "context_metadata.json", include_non_supported = TRUE) {
  if (!dir.exists(dir_path)) {
    stop(sprintf("Directory does not exist: %s", dir_path), call. = FALSE)
  }

  message(sprintf("Starting data scan in: %s", dir_path))

  vector_exts  <- c("shp", "kml", "geojson", "gpkg", "gdb")
  raster_exts  <- c("tif", "tiff", "asc")
  tabular_exts <- c("csv", "xlsx", "xls")
  text_exts    <- c("txt", "md", "html", "json")

  # Detectar carpetas .gdb y archivos normales
  all_dirs <- list.dirs(dir_path, recursive = TRUE, full.names = TRUE)
  gdb_dirs <- all_dirs[grepl("\\.gdb$", tolower(all_dirs))]

  all_files <- list.files(dir_path, recursive = TRUE, full.names = TRUE)
  # Excluir los archivos binarios internos de las carpetas .gdb
  archivos_validos <- all_files[!grepl("\\.gdb/", tolower(all_files))]

  items_to_process <- unique(c(gdb_dirs, archivos_validos))

  # Excluir los propios archivos de salida
  items_to_process <- items_to_process[!grepl("context_metadata", basename(items_to_process))]

  summary_list <- list()

  for (item in items_to_process) {
    ext <- tolower(tools::file_ext(item))
    file_info <- list(
      file_path = item,
      file_name = basename(item),
      size_mb = get_path_size_mb(item)
    )

    result <- if (ext %in% vector_exts) {
      scan_vector(item)
    } else if (ext %in% raster_exts) {
      scan_raster(item)
    } else if (ext %in% tabular_exts) {
      scan_tabular(item, ext)
    } else if (ext %in% text_exts) {
      scan_text(item)
    } else {
      list(type = "no_soportado", note = "Extension not mapped for deep analysis")
    }

    summary_list[[item]] <- c(file_info, result)
  }

  archivos_soportados    <- Filter(function(x) isTRUE(x$type != "no_soportado"), summary_list)
  archivos_no_soportados <- Filter(function(x) isTRUE(x$type == "no_soportado"), summary_list)

  export_catalog <- function(archivos, filename) {
    obj <- list(
      project_directory = normalizePath(dir_path, mustWork = FALSE),
      scanned_at = as.character(Sys.time()),
      total_files = length(archivos),
      files = unname(archivos)
    )
    write_json_safe(obj, filename)
    obj
  }

  json_soportados <- export_catalog(archivos_soportados, output_file)
  message(sprintf("-> Data catalog successfully generated at: %s (%d files processed)",
                  output_file, json_soportados$total_files))

  if (include_non_supported && length(archivos_no_soportados) > 0) {
    no_sop_file <- sub("\\.json$", "_no_soportados.json", output_file)
    export_catalog(archivos_no_soportados, no_sop_file)
    message(sprintf("-> Unsupported files logged at: %s (%d files)",
                    no_sop_file, length(archivos_no_soportados)))
  }

  invisible(json_soportados)
}

#' Generate concise data catalog summary
#'
#' Formats metadata from the project's data catalog into a structured list,
#' pure JSON, or concise Markdown text.
#'
#' @param context_obj_or_path Object returned by `build_context()` or path to `context_metadata.json`.
#' @param format Output format: `"markdown"`, `"list"`, or `"json"`.
#' @return According to `format`, Markdown string, structured list, or JSON string.
#' @export
summary_context <- function(context_obj_or_path = "context_metadata.json",
                            format = c("markdown", "list", "json")) {
  format <- match.arg(format)

  ctx <- if (is.character(context_obj_or_path)) {
    if (!file.exists(context_obj_or_path)) {
      if (format == "list") return(list(error = "No data catalog available."))
      if (format == "json") return("{\"error\": \"No data catalog available.\"}")
      return("No data catalog available.")
    }
    read_json_safe(context_obj_or_path)
  } else {
    context_obj_or_path
  }

  if (format == "list") {
    return(ctx)
  }

  if (format == "json") {
    return(jsonlite::toJSON(ctx, pretty = TRUE, auto_unbox = TRUE))
  }

  # format == "markdown"
  files <- ctx$files %||% list()
  if (length(files) == 0) return("Data catalog is empty.")

  lines <- c("### Available Project Data Catalog:\n")

  for (f in files) {
    tipo <- f$type %||% "desconocido"
    fname <- f$file_name %||% "archivo"
    fpath <- f$file_path %||% ""

    if (tipo == "vectorial") {
      lines <- c(lines, sprintf("- **Vectorial (%s):** `%s` (Path: `%s`)", fname, fpath, fpath))
      for (l in f$layers %||% list()) {
        crs_info <- l$spatial_metadata$crs %||% "Desconocido"
        units_info <- l$spatial_metadata$units %||% ""
        geom_type <- paste(l$spatial_metadata$geometry_type %||% "Geometry", collapse = ", ")
        cols <- sapply(l$attributes_schema %||% list(), function(c) c$column)
        cols_preview <- paste(utils::head(cols, 8), collapse = ", ")
        if (length(cols) > 8) cols_preview <- paste0(cols_preview, "...")

        lines <- c(lines, sprintf("  * Layer: `%s` | Geometry: %s | CRS: %s (%s) | Columns: [%s]",
                                  l$layer_name %||% "default", geom_type, crs_info, units_info, cols_preview))
      }
    } else if (tipo == "raster") {
      sm <- f$spatial_metadata %||% list()
      dim_str <- sprintf("%dx%d px (%d bands)", sm$dimensions$x %||% 0, sm$dimensions$y %||% 0, sm$dimensions$bands %||% 1)
      lines <- c(lines, sprintf("- **Raster (%s):** `%s` | CRS: %s | Dim: %s | Bands: [%s]",
                                fname, fpath, sm$crs %||% "Desconocido", dim_str, paste(f$band_names %||% list(), collapse = ", ")))
    } else if (tipo == "tabular") {
      cols <- sapply(f$attributes_schema %||% list(), function(c) c$column)
      cols_preview <- paste(utils::head(cols, 10), collapse = ", ")
      if (length(cols) > 10) cols_preview <- paste0(cols_preview, "...")
      lines <- c(lines, sprintf("- **Tabular (%s):** `%s` | Columns: [%s]", fname, fpath, cols_preview))
    }
  }

  paste(lines, collapse = "\n")
}
