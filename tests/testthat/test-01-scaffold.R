test_that("init_agent_project crea la estructura completa esperada", {
  tmp_dir <- tempfile(pattern = "mapagen_test_proj_")
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  res <- init_agent_project(tmp_dir, name = "Proyecto Prueba")

  # Verificar directorios
  expect_true(dir.exists(file.path(tmp_dir, "data", "raw")))
  expect_true(dir.exists(file.path(tmp_dir, "data", "processed")))
  expect_true(dir.exists(file.path(tmp_dir, "output", "figures")))
  expect_true(dir.exists(file.path(tmp_dir, "scripts")))

  # Verificar archivos creados
  expect_true(file.exists(file.path(tmp_dir, "project_concept.md")))
  expect_true(file.exists(file.path(tmp_dir, "style.json")))
  expect_true(file.exists(file.path(tmp_dir, "_quarto.yml")))
  expect_true(file.exists(file.path(tmp_dir, "report.qmd")))
  expect_true(file.exists(file.path(tmp_dir, "output", "figures_registry.json")))

  # Verificar que el registro empiece como lista vacía
  reg <- jsonlite::fromJSON(file.path(tmp_dir, "output", "figures_registry.json"))
  expect_equal(length(reg), 0)
})
