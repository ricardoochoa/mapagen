#' @keywords internal
"_PACKAGE"

## The null-coalescing operator
`%||%` <- function(lhs, rhs) {
  if (is.null(lhs) || length(lhs) == 0 || (is.character(lhs) && !nzchar(lhs[1]))) {
    rhs
  } else {
    lhs
  }
}

#' Retrieve the path to an internal package template
#' @param template_name Name of the template file in inst/templates.
#' @return Absolute path to the template file.
#' @keywords internal
get_template_path <- function(template_name) {
  dev_p <- file.path("inst", "templates", template_name)
  if (file.exists(dev_p)) return(normalizePath(dev_p))
  p <- suppressWarnings(system.file("templates", template_name, package = "mapagen"))
  if (p != "" && file.exists(p)) return(normalizePath(p))
  stop(sprintf("Template '%s' not found in mapagen package.", template_name), call. = FALSE)
}

#' Safely read a JSON file using jsonlite
#' @param path Path to the JSON file.
#' @return Parsed list content.
#' @keywords internal
read_json_safe <- function(path) {
  if (!file.exists(path)) {
    stop(sprintf("JSON file does not exist: %s", path), call. = FALSE)
  }
  jsonlite::fromJSON(path, simplifyVector = FALSE)
}

#' Recursively sanitize strings and names in R objects to ensure valid UTF-8
#' @param x Any R object (list, vector, character, etc.).
#' @return The sanitized object with valid UTF-8 characters and names.
#' @keywords internal
sanitize_utf8 <- function(x) {
  if (is.character(x)) {
    invalid <- !validUTF8(x)
    if (any(invalid)) {
      x[invalid] <- iconv(x[invalid], from = "latin1", to = "UTF-8", sub = "")
      still_invalid <- !validUTF8(x)
      if (any(still_invalid)) {
        x[still_invalid] <- iconv(x[still_invalid], from = "", to = "UTF-8", sub = "")
      }
    }
    enc2utf8(x)
  } else if (is.list(x)) {
    if (!is.null(names(x))) {
      names(x) <- sanitize_utf8(names(x))
    }
    lapply(x, sanitize_utf8)
  } else {
    x
  }
}

#' Safely write a formatted JSON file using jsonlite
#' @param obj R object to serialize.
#' @param path Output path.
#' @return The output path invisibly.
#' @keywords internal
write_json_safe <- function(obj, path) {
  dir_name <- dirname(path)
  if (!dir.exists(dir_name)) dir.create(dir_name, recursive = TRUE, showWarnings = FALSE)
  clean_obj <- sanitize_utf8(obj)
  jsonlite::write_json(clean_obj, path, pretty = TRUE, auto_unbox = TRUE, null = "null")
  invisible(path)
}
