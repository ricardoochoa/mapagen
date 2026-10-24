test_that("check_crs_match y check_is_projected funcionan con objetos sf", {
  pts_geo <- sf::st_sfc(sf::st_point(c(-99.1332, 19.4326)), crs = 4326)
  pts_proj <- sf::st_sfc(sf::st_point(c(500000, 2000000)), crs = 3857)

  sf_geo <- sf::st_sf(id = 1, geom = pts_geo)
  sf_proj <- sf::st_sf(id = 1, geom = pts_proj)

  # Coincidencia de CRS
  res_same <- check_crs_match(sf_geo, sf_geo)
  expect_true(res_same$success)
  expect_true(res_same$crs_match)

  res_diff <- check_crs_match(sf_geo, sf_proj)
  expect_true(res_diff$success)
  expect_false(res_diff$crs_match)
  expect_true(grepl("mismatch", res_diff$message, ignore.case = TRUE))

  # Verificacion de CRS proyectado
  p_geo <- check_is_projected(sf_geo)
  expect_true(p_geo$success)
  expect_false(p_geo$is_projected)
  expect_true(grepl("WARNING", p_geo$message))

  p_proj <- check_is_projected(sf_proj)
  expect_true(p_proj$success)
  expect_true(p_proj$is_projected)
  expect_true(grepl("projected", p_proj$message, ignore.case = TRUE))
})

test_that("validate_geometries detecta geometrias validas e invalidas", {
  # Geometria valida
  poly_valid <- sf::st_polygon(list(matrix(c(0,0, 1,0, 1,1, 0,1, 0,0), ncol = 2, byrow = TRUE)))
  sf_valid <- sf::st_sf(id = 1, geom = sf::st_sfc(poly_valid, crs = 4326))

  v_ok <- validate_geometries(sf_valid)
  expect_true(v_ok$success)
  expect_true(v_ok$valid)
  expect_equal(v_ok$invalid_count, 0)
  expect_equal(v_ok$empty_count, 0)

  # Geometria auto-intersectada (invalid "bowtie")
  poly_invalid <- sf::st_polygon(list(matrix(c(0,0, 1,1, 1,0, 0,1, 0,0), ncol = 2, byrow = TRUE)))
  sf_invalid <- sf::st_sf(id = 1, geom = sf::st_sfc(poly_invalid, crs = 4326))

  v_bad <- validate_geometries(sf_invalid)
  expect_true(v_bad$success)
  expect_false(v_bad$valid)
  expect_equal(v_bad$invalid_count, 1)
  expect_true(!is.null(v_bad$suggestion))
  expect_true(grepl("st_make_valid", v_bad$suggestion))
})

test_that("check_spatial_overlap evalua interseccion de extensiones", {
  poly_a <- sf::st_polygon(list(matrix(c(0,0, 10,0, 10,10, 0,10, 0,0), ncol = 2, byrow = TRUE)))
  sf_a <- sf::st_sf(id = "A", geom = sf::st_sfc(poly_a, crs = 3857))

  # Se solapa parcialmente con A
  poly_b <- sf::st_polygon(list(matrix(c(5,5, 15,5, 15,15, 5,15, 5,5), ncol = 2, byrow = TRUE)))
  sf_b <- sf::st_sf(id = "B", geom = sf::st_sfc(poly_b, crs = 3857))

  # No se solapa con A
  poly_c <- sf::st_polygon(list(matrix(c(100,100, 110,100, 110,110, 100,110, 100,100), ncol = 2, byrow = TRUE)))
  sf_c <- sf::st_sf(id = "C", geom = sf::st_sfc(poly_c, crs = 3857))

  ov_ab <- check_spatial_overlap(sf_a, sf_b)
  expect_true(ov_ab$success)
  expect_true(ov_ab$overlap)

  ov_ac <- check_spatial_overlap(sf_a, sf_c)
  expect_true(ov_ac$success)
  expect_false(ov_ac$overlap)
  expect_true(grepl("WARNING", ov_ac$message))
})

test_that("lint_map analiza correctamente un objeto ggplot", {
  p_empty <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg, wt)) + ggplot2::geom_point()

  lm_empty <- lint_map(p_empty)
  expect_true(lm_empty$success)
  expect_false(lm_empty$is_compliant)
  expect_false(lm_empty$has_scalebar)
  expect_false(lm_empty$has_north_arrow)
  expect_false(lm_empty$has_title)
  expect_true(length(lm_empty$suggestions) > 0)

  # Con titulo y caption
  p_labs <- p_empty + ggplot2::labs(title = "Titulo Mapa", caption = "Fuente: Censo")
  lm_labs <- lint_map(p_labs)
  expect_true(lm_labs$has_title)
  expect_true(lm_labs$has_caption)
})
