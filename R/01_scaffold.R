#' Inicializa un proyecto agéntico de análisis geoespacial
#'
#' Crea la estructura de directorios estandarizada, archivos de configuración
#' de estilos (`style.json`), objetivos del proyecto (`project_concept.md`),
#' registro de figuras y plantillas de reporte en Quarto.
#'
#' @param path Directorio raíz donde se inicializará el proyecto. Por defecto el directorio actual `"."`.
#' @param name Nombre opcional del proyecto. Si no se provee, se usa el nombre del directorio.
#' @param overwrite Lógico. Si es `TRUE`, sobrescribe los archivos de configuración existentes.
#'
#' @return Lista invisible con las rutas creadas.
#' @export
#'
#' @examples
#' \dontrun{
#' init_agent_project("mi_analisis_territorial")
#' }
init_agent_project <- function(path = ".", name = NULL, overwrite = FALSE) {
  target_dir <- normalizePath(path, mustWork = FALSE)
  if (!dir.exists(target_dir)) {
    dir.create(target_dir, recursive = TRUE, showWarnings = FALSE)
  }

  proj_name <- name %||% basename(target_dir)

  message(sprintf("Inicializando proyecto mapagen '%s' en: %s", proj_name, target_dir))

  # 1. Crear estructura de carpetas
  subdirs <- c(
    file.path("data", "raw"),
    file.path("data", "processed"),
    file.path("output", "figures"),
    "scripts",
    "R"
  )

  for (sd in subdirs) {
    full_sd <- file.path(target_dir, sd)
    if (!dir.exists(full_sd)) {
      dir.create(full_sd, recursive = TRUE, showWarnings = FALSE)
    }
  }

  # 2. Copiar o generar plantillas
  copy_template <- function(template_name, dest_name = template_name) {
    dest_path <- file.path(target_dir, dest_name)
    if (file.exists(dest_path) && !overwrite) {
      message(sprintf("  - [Omitido] Ya existe: %s", dest_name))
      return(dest_path)
    }
    src_path <- get_template_path(template_name)
    file.copy(src_path, dest_path, overwrite = overwrite)
    message(sprintf("  + [Creado] %s", dest_name))
    dest_path
  }

  # Copiar archivos base
  p_concept <- copy_template("project_concept.md")
  p_style   <- copy_template("style.json")
  p_quarto  <- copy_template("_quarto.yml")
  p_report  <- copy_template("report.qmd")

  # 3. Inicializar registro de figuras vacío si no existe
  registry_path <- file.path(target_dir, "output", "figures_registry.json")
  if (!file.exists(registry_path) || overwrite) {
    write_json_safe(list(), registry_path)
    message("  + [Creado] output/figures_registry.json")
  }

  # 4. Crear archivo .gitignore inicial si no existe
  gitignore_path <- file.path(target_dir, ".gitignore")
  if (!file.exists(gitignore_path)) {
    writeLines(c(".Rproj.user", ".Rhistory", ".RData", ".DS_Store", "output/figures/*.png", "output/figures/*.pdf"), gitignore_path)
    message("  + [Creado] .gitignore")
  }

  message("\n Proyecto inicializado exitosamente.")
  message("Siguiente paso sugerido:")
  message("  1. Coloca tus capas y archivos fuente en 'data/raw/'")
  message("  2. Ajusta tus preferencias en 'style.json' y 'project_concept.md'")
  message("  3. Ejecuta mapagen::build_context() para generar el catalogo de datos.")

  invisible(list(
    project_dir = target_dir,
    concept = p_concept,
    style = p_style,
    registry = registry_path
  ))
}
