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

#' Obtiene la ruta a una plantilla interna del paquete
#' @param template_name Nombre del archivo de plantilla dentro de inst/templates
#' @return Ruta absoluta al archivo de plantilla
#' @keywords internal
get_template_path <- function(template_name) {
  p <- system.file("templates", template_name, package = "mapagen")
  if (p == "" || !file.exists(p)) {
    # Fallback para desarrollo cuando no esta instalado como paquete
    dev_p <- file.path("inst", "templates", template_name)
    if (file.exists(dev_p)) return(normalizePath(dev_p))
    stop(sprintf("No se encontro la plantilla '%s' en el paquete mapagen.", template_name), call. = FALSE)
  }
  normalizePath(p)
}

#' Lee un archivo JSON de forma segura con jsonlite
#' @param path Ruta al archivo JSON
#' @return Lista con el contenido parseado
#' @keywords internal
read_json_safe <- function(path) {
  if (!file.exists(path)) {
    stop(sprintf("El archivo JSON no existe: %s", path), call. = FALSE)
  }
  jsonlite::fromJSON(path, simplifyVector = FALSE)
}

#' Escribe un archivo JSON formateado de forma segura
#' @param obj Objeto R para serializar
#' @param path Ruta de salida
#' @keywords internal
write_json_safe <- function(obj, path) {
  dir_name <- dirname(path)
  if (!dir.exists(dir_name)) dir.create(dir_name, recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(obj, path, pretty = TRUE, auto_unbox = TRUE, null = "null")
  invisible(path)
}
