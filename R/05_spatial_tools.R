#' Extract CRS from a spatial file or in-memory object
#' @keywords internal
get_spatial_crs <- function(x) {
  if (inherits(x, c("sf", "sfc", "bbox"))) {
    return(sf::st_crs(x))
  }
  if (inherits(x, "SpatRaster")) {
    crs_txt <- terra::crs(x)
    return(sf::st_crs(crs_txt))
  }
  if (is.character(x) && file.exists(x)) {
    # Try as vector
    v_crs <- tryCatch({
      layers <- sf::st_layers(x)
      if (length(layers$name) > 0) {
        sf::st_crs(sf::read_sf(x, layer = layers$name[1], n_max = 1, quiet = TRUE))
      } else NULL
    }, error = function(e) NULL)

    if (!is.null(v_crs) && !is.na(v_crs$input)) return(v_crs)

    # Try as raster
    r_crs <- tryCatch({
      r <- terra::rast(x)
      sf::st_crs(terra::crs(r))
    }, error = function(e) NULL)

    if (!is.null(r_crs) && !is.na(r_crs$input)) return(r_crs)
  }

  sf::st_crs(NA)
}

#' Extract sfc bounding box polygon from a spatial file or in-memory object
#' @keywords internal
get_spatial_bbox_sfc <- function(x) {
  if (inherits(x, c("sf", "sfc"))) {
    return(sf::st_as_sfc(sf::st_bbox(x)))
  }
  if (inherits(x, "SpatRaster")) {
    ext_v <- as.vector(terra::ext(x))
    bb <- sf::st_bbox(c(xmin = ext_v[1], xmax = ext_v[2], ymin = ext_v[3], ymax = ext_v[4]),
                      crs = sf::st_crs(terra::crs(x)))
    return(sf::st_as_sfc(bb))
  }
  if (is.character(x) && file.exists(x)) {
    # Try as vector
    sfc_v <- tryCatch({
      sf_sample <- sf::read_sf(x, quiet = TRUE)
      sf::st_as_sfc(sf::st_bbox(sf_sample))
    }, error = function(e) NULL)

    if (!is.null(sfc_v)) return(sfc_v)

    # Try as raster
    sfc_r <- tryCatch({
      r <- terra::rast(x)
      ext_v <- as.vector(terra::ext(r))
      bb <- sf::st_bbox(c(xmin = ext_v[1], xmax = ext_v[2], ymin = ext_v[3], ymax = ext_v[4]),
                        crs = sf::st_crs(terra::crs(r)))
      sf::st_as_sfc(bb)
    }, error = function(e) NULL)

    if (!is.null(sfc_r)) return(sfc_r)
  }

  NULL
}

#' Validate and compare Coordinate Reference System (CRS) match between two layers
#'
#' Verifies whether two spatial layers (file paths or `sf`/`terra` objects)
#' share an identical CRS, indicating whether reprojection is required.
#'
#' @param x Path to first spatial file or in-memory spatial object (`sf`, `sfc`, `SpatRaster`).
#' @param y Path to second spatial file or in-memory spatial object (`sf`, `sfc`, `SpatRaster`).
#' @return Structured list with: `success`, `crs_match` (logical), `crs_x`, `crs_y`, and `message`.
#' @export
#'
#' @examples
#' \dontrun{
#' check_crs_match("layer_a.geojson", "layer_b.geojson")
#' }
check_crs_match <- function(x, y) {
  crs_x <- get_spatial_crs(x)
  crs_y <- get_spatial_crs(y)

  if (is.na(crs_x$input) || crs_x$input == "") {
    return(list(
      success = FALSE,
      crs_match = FALSE,
      error = "Could not determine the CRS of the first element (x)."
    ))
  }

  if (is.na(crs_y$input) || crs_y$input == "") {
    return(list(
      success = FALSE,
      crs_match = FALSE,
      error = "Could not determine the CRS of the second element (y)."
    ))
  }

  is_proj_x <- tryCatch(sf::st_is_longlat(crs_x) == FALSE, error = function(e) NA)
  is_proj_y <- tryCatch(sf::st_is_longlat(crs_y) == FALSE, error = function(e) NA)

  match_bool <- (crs_x == crs_y)

  msg <- if (match_bool) {
    "Coordinate reference systems match perfectly."
  } else {
    sprintf(
      "CRS mismatch detected: x uses '%s' (EPSG: %s) and y uses '%s' (EPSG: %s). Reprojection recommended using sf::st_transform().",
      crs_x$input, crs_x$epsg %||% "NA", crs_y$input, crs_y$epsg %||% "NA"
    )
  }

  list(
    success = TRUE,
    crs_match = match_bool,
    crs_x = list(
      input = crs_x$input,
      epsg = crs_x$epsg,
      is_projected = is_proj_x,
      units = ifelse(isTRUE(is_proj_x), "meters (projected)", "decimal degrees (geographic)")
    ),
    crs_y = list(
      input = crs_y$input,
      epsg = crs_y$epsg,
      is_projected = is_proj_y,
      units = ifelse(isTRUE(is_proj_y), "meters (projected)", "decimal degrees (geographic)")
    ),
    message = msg
  )
}

#' Verify if a layer is in a projected (metric) Coordinate Reference System
#'
#' Checks whether a spatial layer is projected (meters/feet) or geographic (degrees).
#' Warns if distance or buffer operations will yield angular unit distortions.
#'
#' @param x Path to spatial file or in-memory spatial object (`sf`, `sfc`, `SpatRaster`).
#' @return Structured list with: `success`, `is_projected`, `crs`, `crs_epsg`, `units`, and `message`.
#' @export
#'
#' @examples
#' \dontrun{
#' check_is_projected("counties.geojson")
#' }
check_is_projected <- function(x) {
  crs_obj <- get_spatial_crs(x)

  if (is.na(crs_obj$input) || crs_obj$input == "") {
    return(list(
      success = FALSE,
      is_projected = FALSE,
      error = "Could not extract or CRS is undefined for the element."
    ))
  }

  is_proj <- tryCatch(sf::st_is_longlat(crs_obj) == FALSE, error = function(e) FALSE)

  msg <- if (isTRUE(is_proj)) {
    "Layer is in a projected coordinate system (suitable for metric calculations like areas, buffers, or distances)."
  } else {
    sprintf(
      "WARNING: Layer is in unprojected geographic coordinates ('%s', EPSG: %s). Distances and buffers will be computed in decimal degrees. Reproject to a metric CRS before operating.",
      crs_obj$input, crs_obj$epsg %||% "4326"
    )
  }

  list(
    success = TRUE,
    is_projected = is_proj,
    crs = crs_obj$input,
    crs_epsg = crs_obj$epsg,
    units = ifelse(isTRUE(is_proj), "meters (projected)", "decimal degrees (geographic)"),
    message = msg
  )
}

#' Validate topological integrity of spatial geometries
#'
#' Performs topological validity checks (`sf::st_is_valid`) on vector geometries,
#' detecting self-intersections or empty geometries and recommending `sf::st_make_valid`.
#'
#' @param x Path to spatial vector file or in-memory `sf`/`sfc` object.
#' @param fix_suggestion Logical. If `TRUE`, includes repair code recommendations.
#' @return Structured list with: `success`, `valid`, `total_features`, `valid_count`, `invalid_count`, `empty_count`, `invalid_reasons`, and `message`.
#' @export
#'
#' @examples
#' \dontrun{
#' validate_geometries("parcels.shp")
#' }
validate_geometries <- function(x, fix_suggestion = TRUE) {
  sf_obj <- if (inherits(x, c("sf", "sfc"))) {
    if (inherits(x, "sfc")) sf::st_sf(geometry = x) else x
  } else {
    file_path <- as.character(x)
    if (!file.exists(file_path)) {
      return(list(success = FALSE, valid = FALSE, error = sprintf("File does not exist: %s", file_path)))
    }
    tryCatch(sf::read_sf(file_path, quiet = TRUE), error = function(e) NULL)
  }

  if (is.null(sf_obj)) {
    return(list(success = FALSE, valid = FALSE, error = "Could not load spatial geometries."))
  }

  total_f <- nrow(sf_obj)
  if (total_f == 0) {
    return(list(
      success = TRUE,
      valid = FALSE,
      total_features = 0,
      valid_count = 0,
      invalid_count = 0,
      empty_count = 0,
      message = "Geometry set is empty (0 rows)."
    ))
  }

  geom_col <- sf::st_geometry(sf_obj)
  is_empty <- sf::st_is_empty(geom_col)
  empty_count <- sum(is_empty)

  valid_res <- sf::st_is_valid(geom_col, reason = TRUE)
  is_valid_bool <- sf::st_is_valid(geom_col)

  invalid_idx <- which(!is_valid_bool)
  invalid_count <- length(invalid_idx)
  valid_count <- total_f - invalid_count - empty_count

  all_ok <- (invalid_count == 0 && empty_count == 0)
  invalid_reasons <- if (invalid_count > 0) unique(valid_res[invalid_idx]) else character(0)

  msg <- if (all_ok) {
    sprintf("All geometries are topologically valid (%d features).", total_f)
  } else {
    sprintf(
      "Topological issues detected: %d invalid geometries and %d empty features out of %d total.",
      invalid_count, empty_count, total_f
    )
  }

  sugg <- if (!all_ok && isTRUE(fix_suggestion)) {
    "Use `sf::st_make_valid()` to repair self-intersections and invalid rings before spatial joins or area calculations."
  } else {
    NULL
  }

  list(
    success = TRUE,
    valid = all_ok,
    total_features = total_f,
    valid_count = valid_count,
    invalid_count = invalid_count,
    empty_count = empty_count,
    invalid_reasons = invalid_reasons,
    suggestion = sugg,
    message = msg
  )
}

#' Verify if two spatial layers overlap in bounding box extent
#'
#' Checks whether the bounding boxes of two layers intersect in a common CRS,
#' warning if spatial joins (`st_intersection`, `st_join`) will produce empty outputs.
#'
#' @param x First layer (path or `sf`/`terra` object).
#' @param y Second layer (path or `sf`/`terra` object).
#' @return Structured list with `success`, `overlap` (logical), `common_crs`, and `message`.
#' @export
#'
#' @examples
#' \dontrun{
#' check_spatial_overlap("schools.geojson", "counties.geojson")
#' }
check_spatial_overlap <- function(x, y) {
  sfc_x <- get_spatial_bbox_sfc(x)
  sfc_y <- get_spatial_bbox_sfc(y)

  if (is.null(sfc_x) || is.null(sfc_y)) {
    return(list(
      success = FALSE,
      overlap = FALSE,
      error = "Could not extract geographic extent (bbox) from one or both elements."
    ))
  }

  crs_x <- sf::st_crs(sfc_x)
  crs_y <- sf::st_crs(sfc_y)

  if (is.na(crs_x$input) || is.na(crs_y$input)) {
    return(list(
      success = FALSE,
      overlap = FALSE,
      error = "One or both elements lack a defined CRS to compute spatial overlap."
    ))
  }

  # Reproject bbox of y to CRS of x if they differ
  sfc_y_trans <- if (crs_x != crs_y) {
    tryCatch(sf::st_transform(sfc_y, crs_x), error = function(e) NULL)
  } else {
    sfc_y
  }

  if (is.null(sfc_y_trans)) {
    return(list(
      success = FALSE,
      overlap = FALSE,
      error = "Failed to reproject spatial extents to a common CRS."
    ))
  }

  intersects_mat <- sf::st_intersects(sfc_x, sfc_y_trans, sparse = FALSE)
  has_overlap <- isTRUE(intersects_mat[1, 1])

  msg <- if (has_overlap) {
    "Spatial extents of both layers overlap correctly."
  } else {
    "WARNING: Layers have no spatial overlap. Spatial intersection operations (st_intersection, st_join) will produce empty results."
  }

  list(
    success = TRUE,
    overlap = has_overlap,
    common_crs = crs_x$input,
    message = msg
  )
}

#' Lint and inspect ggplot2 map composition and cartographic decorations
#'
#' Heuristically inspects a `ggplot` object to verify presence of essential
#' cartographic decorations such as scale bars, north arrows, titles, and captions.
#'
#' @param plot A `ggplot` graphic object.
#' @param style_path Optional path to `style.json`.
#' @return Structured list with completeness diagnosis and actionable suggestions.
#' @export
#'
#' @examples
#' \dontrun{
#' p <- ggplot(cars, aes(speed, dist)) + geom_point()
#' lint_map(p)
#' }
lint_map <- function(plot, style_path = "style.json") {
  if (!inherits(plot, "ggplot")) {
    return(list(
      success = FALSE,
      error = "The 'plot' argument is not an object of class ggplot."
    ))
  }

  # Inspect ggspatial layers
  layer_classes <- unlist(lapply(plot$layers, function(l) class(l$geom)))

  has_scalebar <- any(grepl("GeomSpatialScaleBar|SpatialScaleBar", layer_classes, ignore.case = TRUE))
  has_north_arrow <- any(grepl("GeomSpatialNorthArrow|NorthArrow", layer_classes, ignore.case = TRUE))

  has_title <- !is.null(plot$labels$title) && nzchar(as.character(plot$labels$title))
  has_caption <- !is.null(plot$labels$caption) && nzchar(as.character(plot$labels$caption))

  suggestions <- character(0)
  if (!has_scalebar) {
    suggestions <- c(suggestions, "Add a scale bar using `mapagen::add_cartographic_decorations()` or `ggspatial::annotation_scale()`.")
  }
  if (!has_north_arrow) {
    suggestions <- c(suggestions, "Add a cartographic north arrow using `ggspatial::annotation_north_arrow()`.")
  }
  if (!has_title) {
    suggestions <- c(suggestions, "Map lacks a descriptive title (add `labs(title = '...')`).")
  }
  if (!has_caption) {
    suggestions <- c(suggestions, "Recommended to add a source caption (add `labs(caption = '...')`).")
  }

  is_compliant <- (has_scalebar && has_north_arrow && has_title)

  list(
    success = TRUE,
    is_compliant = is_compliant,
    has_scalebar = has_scalebar,
    has_north_arrow = has_north_arrow,
    has_title = has_title,
    has_caption = has_caption,
    layers_count = length(plot$layers),
    suggestions = suggestions,
    message = if (is_compliant) "Map meets basic cartographic element criteria." else paste(suggestions, collapse = " ")
  )
}
