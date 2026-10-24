#' Initialize an agentic geospatial analysis project
#'
#' Creates a standardized directory structure, style configuration files
#' (`style.json`), project objectives (`project_concept.md`), figure
#' registry, and Quarto report templates.
#'
#' @param path Root directory where the project will be initialized. Defaults to current directory `"."`.
#' @param name Optional project name. If not provided, directory name is used.
#' @param overwrite Logical. If `TRUE`, overwrites existing configuration files.
#'
#' @return Invisible list containing the created file and directory paths.
#' @export
#'
#' @examples
#' \dontrun{
#' init_agent_project("my_spatial_project")
#' }
init_agent_project <- function(path = ".", name = NULL, overwrite = FALSE) {
  target_dir <- normalizePath(path, mustWork = FALSE)
  if (!dir.exists(target_dir)) {
    dir.create(target_dir, recursive = TRUE, showWarnings = FALSE)
  }

  proj_name <- name %||% basename(target_dir)

  message(sprintf("Initializing mapagen project '%s' at: %s", proj_name, target_dir))

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
      message(sprintf("  - [Skipped] Already exists: %s", dest_name))
      return(dest_path)
    }
    src_path <- get_template_path(template_name)
    file.copy(src_path, dest_path, overwrite = overwrite)
    message(sprintf("  + [Created] %s", dest_name))
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
    message("  + [Created] output/figures_registry.json")
  }

  # 4. Crear archivo .gitignore inicial si no existe
  gitignore_path <- file.path(target_dir, ".gitignore")
  if (!file.exists(gitignore_path)) {
    writeLines(c(".Rproj.user", ".Rhistory", ".RData", ".DS_Store", "output/figures/*.png", "output/figures/*.pdf"), gitignore_path)
    message("  + [Created] .gitignore")
  }

  message("\nProject initialized successfully.")
  message("Suggested next steps:")
  message("  1. Place your spatial layers and raw files in 'data/raw/'")
  message("  2. Adjust your settings in 'style.json' and 'project_concept.md'")
  message("  3. Run mapagen::build_context() to generate the data catalog.")

  invisible(list(
    project_dir = target_dir,
    concept = p_concept,
    style = p_style,
    registry = registry_path
  ))
}
