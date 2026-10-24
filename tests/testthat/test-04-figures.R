test_that("export_iteration genera artefactos y actualiza el registro con versionado", {
  tmp_proj <- tempfile(pattern = "proj_fig_")
  init_agent_project(tmp_proj)
  on.exit(unlink(tmp_proj, recursive = TRUE), add = TRUE)

  style_path <- file.path(tmp_proj, "style.json")
  out_dir <- file.path(tmp_proj, "output")
  reg_path <- file.path(out_dir, "figures_registry.json")

  # Crear plot simple
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg, wt)) +
    ggplot2::geom_point() +
    theme_from_json(style_path)

  # Version 1 (draft)
  res1 <- export_iteration(
    plot = p,
    id = "fig_test",
    caption = "Primera version",
    sources_id = "mtcars",
    summary_data = data.frame(media_mpg = mean(mtcars$mpg)),
    output_dir = out_dir,
    style_path = style_path,
    final = FALSE
  )

  expect_equal(res1$version, 1)
  expect_equal(res1$status, "draft")
  expect_true(file.exists(res1$artifacts$png))
  expect_true(file.exists(res1$artifacts$data_json))
  expect_null(res1$artifacts$pdf)

  # Version 2 (final)
  res2 <- export_iteration(
    plot = p,
    id = "fig_test",
    caption = "Segunda version final",
    sources_id = c("mtcars", "censo"),
    summary_data = data.frame(media_mpg = mean(mtcars$mpg), max_mpg = max(mtcars$mpg)),
    output_dir = out_dir,
    style_path = style_path,
    final = TRUE
  )

  expect_equal(res2$version, 2)
  expect_equal(res2$status, "final")
  expect_true(file.exists(res2$artifacts$png))
  expect_true(file.exists(res2$artifacts$pdf))

  # Verificar registro completo
  reg <- get_figure_registry(reg_path)
  expect_equal(length(reg), 2)

  # Verificar get_figure_entry
  latest_entry <- get_figure_entry("fig_test", "latest", reg_path)
  expect_equal(latest_entry$version, 2)

  v1_entry <- get_figure_entry("fig_test", version = 1, reg_path)
  expect_equal(v1_entry$version, 1)

  # Verificar include_figure
  md <- include_figure("fig_test", version = "latest", registry_path = reg_path, style_path = style_path)
  expect_true(grepl("Figura fig_test \\(v2\\)", md))
  expect_true(grepl("Segunda version final", md))
  expect_true(grepl("Fuente: mtcars, censo\\.", md))

  # 1. Probar list_figures
  lf <- list_figures(reg_path)
  expect_s3_class(lf, "data.frame")
  expect_equal(nrow(lf), 2)
  expect_equal(lf$figure_id, c("fig_test", "fig_test"))
  expect_equal(lf$version, c(1, 2))
  expect_true(all(lf$has_png))
  expect_true(all(lf$has_data))

  # 2. Probar get_figure_data
  fd <- get_figure_data("fig_test", version = 2, registry_path = reg_path)
  expect_true(fd$success)
  expect_equal(fd$figure_id, "fig_test")
  expect_equal(fd$version, 2)
  expect_equal(fd$data$media_mpg, mean(mtcars$mpg), tolerance = 1e-4)

  # 3. Probar update_figure_metadata
  upd <- update_figure_metadata("fig_test", version = 1, caption = "Caption modificado", status = "reviewed", registry_path = reg_path)
  expect_true(upd$success)
  expect_equal(upd$entry$caption, "Caption modificado")
  expect_equal(upd$entry$status, "reviewed")

  # 4. Probar delete_figure_version
  del <- delete_figure_version("fig_test", version = 1, delete_files = TRUE, registry_path = reg_path)
  expect_true(del$success)
  expect_equal(del$deleted_version, 1)
  expect_equal(del$remaining_entries, 1)

  # Verificar que el registro solo tiene version 2 ahora
  lf_after <- list_figures(reg_path)
  expect_equal(nrow(lf_after), 1)
  expect_equal(lf_after$version, 2)
})
