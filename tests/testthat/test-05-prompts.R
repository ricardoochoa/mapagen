test_that("extract_r_code extrae bloques limpios de codigo R", {
  text_with_code <- paste0(
    "Aqui tienes el analisis solicitado:\n\n",
    "```r\n",
    "library(sf)\n",
    "p <- ggplot(data) + geom_sf()\n",
    "```\n\n",
    "Y para guardarlo:\n",
    "```R\n",
    "export_iteration(p, 'fig_01')\n",
    "```\n"
  )

  extracted <- extract_r_code(text_with_code)
  expect_true(grepl("library\\(sf\\)", extracted))
  expect_true(grepl("export_iteration", extracted))
  expect_false(grepl("```", extracted))
})

test_that("get_llm_system_context ensambla reglas y catalogo correctamente", {
  tmp_proj <- tempfile(pattern = "proj_ctx_")
  init_agent_project(tmp_proj)
  on.exit(unlink(tmp_proj, recursive = TRUE), add = TRUE)

  # Crear un archivo de datos y construir catalogo
  data_dir <- file.path(tmp_proj, "data", "raw")
  readr::write_csv(data.frame(a = 1:2, b = 3:4), file.path(data_dir, "muestra.csv"))
  cat_path <- file.path(tmp_proj, "context_metadata.json")
  build_context(data_dir, cat_path, include_non_supported = FALSE)

  sys_ctx <- get_llm_system_context(
    concept_path = file.path(tmp_proj, "project_concept.md"),
    style_path = file.path(tmp_proj, "style.json"),
    context_path = cat_path
  )

  expect_true(grepl("EPSG:6362", sys_ctx))
  expect_true(grepl("muestra.csv", sys_ctx))
  expect_true(grepl("theme_from_json", sys_ctx))
})
