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

  # Resumen markdown
  summary_txt <- summary_context(out_json, format = "markdown")
  expect_true(grepl("tabla1.csv", summary_txt))
  expect_true(grepl("Tabular", summary_txt))

  # Resumen list y json
  summary_lst <- summary_context(out_json, format = "list")
  expect_type(summary_lst, "list")
  expect_equal(summary_lst$total_files, 1)

  summary_j <- summary_context(out_json, format = "json")
  expect_type(summary_j, "character")
  expect_true(grepl("tabla1.csv", summary_j))

  # Probar list_project_data
  lp <- list_project_data(tmp_dir)
  expect_s3_class(lp, "data.frame")
  expect_equal(nrow(lp), 1)
  expect_equal(lp$file_name[1], "tabla1.csv")
  expect_equal(lp$type[1], "tabular")
})

test_that("escaneos soportan objetos directamente en memoria", {
  df <- data.frame(a = 1:5, b = letters[1:5])
  res_tab <- scan_tabular(df)
  expect_true(res_tab$success)
  expect_equal(res_tab$type, "tabular")
  expect_equal(res_tab$rows_sample, 5)

  pts <- sf::st_sfc(sf::st_point(c(0, 0)), sf::st_point(c(1, 1)), crs = 3857)
  sf_df <- sf::st_sf(id = 1:2, geom = pts)
  res_vec <- scan_vector(sf_df)
  expect_true(res_vec$success)
  expect_equal(res_vec$type, "vectorial")
  expect_true(res_vec$layers[[1]]$spatial_metadata$is_projected)
})

test_that("scan_text maneja correctamente archivos con codificacion no UTF-8 (latin1 y UTF-16)", {
  tmp_latin1 <- tempfile(fileext = ".txt")
  tmp_utf16 <- tempfile(fileext = ".txt")
  on.exit(unlink(c(tmp_latin1, tmp_utf16)), add = TRUE)

  # Archivo en Latin-1 con caracteres especiales en espanol
  texto_latin1 <- iconv("Informacion geografica y metadatos: Cancun, Yucatan, Mexico, erosion costera", to = "latin1")
  writeBin(charToRaw(texto_latin1), tmp_latin1)

  res_latin1 <- scan_text(tmp_latin1)
  expect_true(res_latin1$success)
  expect_true(validUTF8(res_latin1$content))
  expect_true(grepl("Cancun|Cancún", res_latin1$content))

  # Archivo en UTF-16LE con BOM
  bom_le <- as.raw(c(0xff, 0xfe))
  texto_utf16 <- iconv("Distribucion de manglares y humedales", to = "UTF-16LE")
  writeBin(c(bom_le, charToRaw(texto_utf16)), tmp_utf16)

  res_utf16 <- scan_text(tmp_utf16)
  expect_true(res_utf16$success)
  expect_true(validUTF8(res_utf16$content))
})
