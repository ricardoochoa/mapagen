#' Calcula el tamano en megabytes de un archivo o directorio
#'
#' @param path Ruta al archivo o directorio (por ejemplo un .gdb).
#' @return Numero que representa el tamano en MB redondeado a 2 decimales.
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

#' Extrae el esquema resumido de columnas de un data.frame o tibble
#'
#' @param df Un data.frame o tibble.
#' @return Lista con el esquema de cada columna (nombre, tipo, porcentaje de NAs y valores de muestra).
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

#' Escanea y extrae metadatos de archivos vectoriales espaciales
#'
#' Soporta formatos multicapa y monocapa (.shp, .geojson, .gpkg, .kml, .gdb).
#'
#' @param file_path Ruta al archivo o directorio vectorial.
#' @return Lista estructurada con metadatos de las capas y atributos.
#' @export
scan_vector <- function(file_path) {
  layers_info <- tryCatch(sf::st_layers(file_path), error = function(e) NULL)

  if (is.null(layers_info) || length(layers_info$name) == 0) {
    return(list(type = "vectorial", error = "No se encontraron capas espaciales o archivo invalido"))
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
        error = "La capa esta vacia o hubo un error al leer la muestra de geometrias"
      )
      next
    }

    bbox <- sf::st_bbox(sample_data)
    df_no_geom <- sf::st_drop_geometry(sample_data)
    crs_obj <- sf::st_crs(sample_data)
    is_proj <- tryCatch(sf::st_is_longlat(sample_data) == FALSE, error = function(e) NA)

    layers_metadata[[layer_name]] <- list(
      layer_name = layer_name,
      features_count = ifelse(is.na(features_count), "Desconocido", features_count),
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
    type = "vectorial",
    total_layers = length(layers_info$name),
    layers = unname(layers_metadata)
  )
}

#' Escanea y extrae metadatos de archivos raster
#'
#' Lee unicamente punteros de memoria con `terra` sin cargar el raster a RAM.
#'
#' @param file_path Ruta al archivo raster (.tif, .asc, etc.).
#' @return Lista con resolucion, extension, bandas y CRS.
#' @export
scan_raster <- function(file_path) {
  r <- tryCatch(terra::rast(file_path), error = function(e) NULL)
  if (is.null(r)) return(list(type = "raster", error = "No se pudo leer el archivo raster"))

  crs_desc <- tryCatch(terra::crs(r, describe = TRUE), error = function(e) list())
  is_proj <- tryCatch(!terra::is.lonlat(r), error = function(e) NA)

  list(
    type = "raster",
    spatial_metadata = list(
      crs = crs_desc$name %||% "Desconocido",
      crs_code = crs_desc$code %||% NA,
      is_projected = is_proj,
      resolution = as.list(terra::res(r)),
      dimensions = list(x = terra::ncol(r), y = terra::nrow(r), bands = terra::nlyr(r)),
      bbox = as.list(as.vector(terra::ext(r)))
    ),
    band_names = names(r)
  )
}

#' Escanea archivos tabulares
#'
#' @param file_path Ruta al archivo (.csv, .xlsx, .xls).
#' @param ext Extension del archivo en minusculas.
#' @return Lista con el esquema de columnas.
#' @export
scan_tabular <- function(file_path, ext = tools::file_ext(file_path)) {
  ext <- tolower(ext)
  sample_data <- tryCatch({
    if (ext %in% c("csv")) {
      readr::read_csv(file_path, n_max = 50, show_col_types = FALSE)
    } else if (ext %in% c("xlsx", "xls")) {
      readxl::read_excel(file_path, n_max = 50)
    }
  }, error = function(e) NULL)

  if (is.null(sample_data)) {
    return(list(type = "tabular", error = "Error al leer el archivo tabular"))
  }

  list(
    type = "tabular",
    rows_sample = nrow(sample_data),
    attributes_schema = extract_column_schema(sample_data)
  )
}

#' Escanea documentos de texto o documentacion
#'
#' @param file_path Ruta al archivo (.txt, .md, .json, .html).
#' @param max_chars Maximo numero de caracteres a leer antes de truncar.
#' @return Lista con el contenido textual truncado.
#' @export
scan_text <- function(file_path, max_chars = 5000) {
  text_content <- tryCatch(readLines(file_path, warn = FALSE), error = function(e) "")
  collapsed_text <- paste(text_content, collapse = "\n")
  is_truncated <- nchar(collapsed_text) > max_chars
  if (is_truncated) {
    collapsed_text <- paste0(substr(collapsed_text, 1, max_chars), "\n... [TRUNCADO POR SEGURIDAD DE TOKENS]")
  }
  list(type = "documento", truncated = is_truncated, content = collapsed_text)
}

#' Construye el Catalogo de Datos del Proyecto (Repomix Geoespacial)
#'
#' Escanea recursivamente un directorio de datos (por defecto `data/`),
#' analiza archivos vectoriales, raster, tabulares y documentales,
#' y genera el archivo `context_metadata.json`.
#'
#' @param dir_path Ruta al directorio a escanear. Por defecto `"data"`.
#' @param output_file Ruta del archivo JSON consolidado de salida. Por defecto `"context_metadata.json"`.
#' @param include_non_supported Logico. Si es `TRUE`, guarda un archivo adicional con extensiones no procesadas.
#'
#' @return Lista invisible con el contenido consolidado del catalogo.
#' @export
#'
#' @examples
#' \dontrun{
#' build_context("data")
#' }
build_context <- function(dir_path = "data", output_file = "context_metadata.json", include_non_supported = TRUE) {
  if (!dir.exists(dir_path)) {
    stop(sprintf("El directorio no existe: %s", dir_path), call. = FALSE)
  }

  message(sprintf("Iniciando escaneo de datos en: %s", dir_path))

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
      list(type = "no_soportado", note = "Extension no mapeada para analisis profundo")
    }

    summary_list[[item]] <- c(file_info, result)
  }

  archivos_soportados    <- Filter(function(x) x$type != "no_soportado", summary_list)
  archivos_no_soportados <- Filter(function(x) x$type == "no_soportado", summary_list)

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
  message(sprintf("-> Catalogo de datos generado exitosamente en: %s (%d archivos procesados)",
                  output_file, json_soportados$total_files))

  if (include_non_supported && length(archivos_no_soportados) > 0) {
    no_sop_file <- sub("\\.json$", "_no_soportados.json", output_file)
    export_catalog(archivos_no_soportados, no_sop_file)
    message(sprintf("-> Archivos no soportados registrados en: %s (%d archivos)",
                    no_sop_file, length(archivos_no_soportados)))
  }

  invisible(json_soportados)
}

#' Genera una sintesis concisa del catalogo para inyectar en prompts LLM
#'
#' @param context_obj_or_path Objeto devuelto por `build_context()` o ruta a `context_metadata.json`.
#' @return Cadena de texto en formato Markdown lista para incluir en prompts.
#' @export
summary_context <- function(context_obj_or_path = "context_metadata.json") {
  ctx <- if (is.character(context_obj_or_path)) {
    if (!file.exists(context_obj_or_path)) return("No hay catalogo de datos disponible.")
    read_json_safe(context_obj_or_path)
  } else {
    context_obj_or_path
  }

  files <- ctx$files %||% list()
  if (length(files) == 0) return("El catalogo de datos esta vacio.")

  lines <- c("### Catalogo de Datos Disponibles en el Proyecto:\n")

  for (f in files) {
    tipo <- f$type %||% "desconocido"
    fname <- f$file_name %||% "archivo"
    fpath <- f$file_path %||% ""

    if (tipo == "vectorial") {
      lines <- c(lines, sprintf("- **Vectorial (%s):** `%s` (Ruta: `%s`)", fname, fpath, fpath))
      for (l in f$layers %||% list()) {
        crs_info <- l$spatial_metadata$crs %||% "Desconocido"
        units_info <- l$spatial_metadata$units %||% ""
        geom_type <- paste(l$spatial_metadata$geometry_type %||% "Geometria", collapse = ", ")
        cols <- sapply(l$attributes_schema %||% list(), function(c) c$column)
        cols_preview <- paste(utils::head(cols, 8), collapse = ", ")
        if (length(cols) > 8) cols_preview <- paste0(cols_preview, "...")

        lines <- c(lines, sprintf("  * Capa: `%s` | Geometria: %s | CRS: %s (%s) | Columnas: [%s]",
                                  l$layer_name %||% "default", geom_type, crs_info, units_info, cols_preview))
      }
    } else if (tipo == "raster") {
      sm <- f$spatial_metadata %||% list()
      dim_str <- sprintf("%dx%d px (%d bandas)", sm$dimensions$x %||% 0, sm$dimensions$y %||% 0, sm$dimensions$bands %||% 1)
      lines <- c(lines, sprintf("- **Raster (%s):** `%s` | CRS: %s | Dim: %s | Bandas: [%s]",
                                fname, fpath, sm$crs %||% "Desconocido", dim_str, paste(f$band_names %||% list(), collapse = ", ")))
    } else if (tipo == "tabular") {
      cols <- sapply(f$attributes_schema %||% list(), function(c) c$column)
      cols_preview <- paste(utils::head(cols, 10), collapse = ", ")
      if (length(cols) > 10) cols_preview <- paste0(cols_preview, "...")
      lines <- c(lines, sprintf("- **Tabular (%s):** `%s` | Columnas: [%s]", fname, fpath, cols_preview))
    }
  }

  paste(lines, collapse = "\n")
}
