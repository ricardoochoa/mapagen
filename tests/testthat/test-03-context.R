test_that("scan_tabular y extract_column_schema funcionan correctamente", {
  tmp_csv <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp_csv), add = TRUE)

  df <- data.frame(
    id = 1:5,
    nombre = c("A", "B", "C", "D", "E"),
    valor = c(10.5, 20.1, NA, 40.2, 50.0)
  )
  readr::write_csv(df, tmp_csv)

  res <- scan_tabular(tmp_csv, "csv")
  expect_equal(res$type, "tabular")
  expect_equal(length(res$attributes_schema), 3)

  col_valor <- Filter(function(x) identical(x$column, "valor"), res$attributes_schema)[[1]]
  expect_equal(col_valor$na_percentage, 20)
})

test_that("scan_vector procesa correctamente un objeto spatial sf", {
  tmp_gpkg <- tempfile(fileext = ".gpkg")
  on.exit(unlink(tmp_gpkg), add = TRUE)

  # Crear sf simple
  pts <- sf::st_sfc(sf::st_point(c(0, 0)), sf::st_point(c(1, 1)), crs = 4326)
  sf_df <- sf::st_sf(id = 1:2, categoria = c("Urbano", "Rural"), geom = pts)
  sf::st_write(sf_df, tmp_gpkg, quiet = TRUE)

  res <- scan_vector(tmp_gpkg)
  expect_equal(res$type, "vectorial")
  expect_equal(res$total_layers, 1)

  layer <- res$layers[[1]]
  expect_equal(layer$spatial_metadata$crs_epsg, 4326)
  expect_false(layer$spatial_metadata$is_projected)
  expect_equal(layer$spatial_metadata$units, "grados decimales (geografico)")
  expect_equal(length(layer$attributes_schema), 2)
})

test_that("build_context y summary_context generan catalogo y sintesis", {
  tmp_dir <- tempfile(pattern = "data_test_")
  dir.create(tmp_dir, recursive = TRUE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  # Escribir un archivo tabular
  readr::write_csv(data.frame(x = 1:3, y = 4:6), file.path(tmp_dir, "tabla1.csv"))

  out_json <- file.path(tmp_dir, "context_metadata.json")
  catalog <- build_context(dir_path = tmp_dir, output_file = out_json, include_non_supported = FALSE)

  expect_true(file.exists(out_json))
  expect_equal(catalog$total_files, 1)

  # Resumen
  summary_txt <- summary_context(out_json)
  expect_true(grepl("tabla1.csv", summary_txt))
  expect_true(grepl("Tabular", summary_txt))
})
