test_that("read_style y get_preferred_crs leen correctamente la configuracion", {
  style_path <- system.file("templates", "style.json", package = "mapagen")
  if (style_path == "") style_path <- file.path("inst", "templates", "style.json")

  cfg <- read_style(style_path)
  expect_type(cfg, "list")
  expect_equal(cfg$project_metadata$language, "es")

  crs_proj <- get_preferred_crs(style_path, "projected")
  crs_geo  <- get_preferred_crs(style_path, "geographic")
  expect_equal(crs_proj, "EPSG:6362")
  expect_equal(crs_geo, "EPSG:4326")
})

test_that("theme_from_json genera un tema valido de ggplot2", {
  style_path <- system.file("templates", "style.json", package = "mapagen")
  if (style_path == "") style_path <- file.path("inst", "templates", "style.json")

  th <- theme_from_json(style_path)
  expect_s3_class(th, "theme")

  # Probar integracion basica con ggplot2
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg, wt)) +
    ggplot2::geom_point() +
    th

  expect_s3_class(p, "ggplot")
})

test_that("get_palette y escalas devuelven colores esperados", {
  style_path <- system.file("templates", "style.json", package = "mapagen")
  if (style_path == "") style_path <- file.path("inst", "templates", "style.json")

  pal <- get_palette("primary", style_path)
  expect_true(length(pal) >= 3)
  expect_true(grepl("^#", pal[1]))

  s_fill <- scale_fill_mapagen("primary", style_path)
  expect_s3_class(s_fill, "ScaleDiscrete")
})

test_that("get_project_rules devuelve directrices en formatos list, json y markdown", {
  style_path <- system.file("templates", "style.json", package = "mapagen")
  if (style_path == "") style_path <- file.path("inst", "templates", "style.json")

  concept_path <- system.file("templates", "project_concept.md", package = "mapagen")
  if (concept_path == "") concept_path <- file.path("inst", "templates", "project_concept.md")

  # Formato list
  rules_l <- get_project_rules(style_path, concept_path, format = "list")
  expect_type(rules_l, "list")
  expect_equal(rules_l$language, "es")
  expect_equal(rules_l$crs$preferred_projected, "EPSG:6362")
  expect_true(!is.null(rules_l$conventions$metric_calculations))

  # Formato json
  rules_j <- get_project_rules(style_path, concept_path, format = "json")
  expect_type(rules_j, "character")
  expect_true(grepl("EPSG:6362", rules_j))

  # Formato markdown
  rules_m <- get_project_rules(style_path, concept_path, format = "markdown")
  expect_type(rules_m, "character")
  expect_true(grepl("Directrices del Proyecto mapagen", rules_m))
})
