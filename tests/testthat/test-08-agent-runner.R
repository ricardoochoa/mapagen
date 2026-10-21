test_that("build_repair_prompt genera un prompt estructurado con diagnostico y codigo", {
  err <- "Error in st_transform: cannot transform sfc_POLYGON to target CRS"
  code <- "p <- ggplot(data) + geom_sf()"

  prompt <- build_repair_prompt(err, code)

  expect_true(grepl("\\[ERROR\\]: Error in st_transform", prompt))
  expect_true(grepl("geom_sf\\(\\)", prompt))
  expect_true(grepl("theme_from_json", prompt))
  expect_true(grepl("export_iteration", prompt))
})

test_that("run_spatial_script ejecuta codigo valido y captura errores de forma segura", {
  tmp_proj <- tempfile(pattern = "proj_runner_")
  dir.create(tmp_proj, recursive = TRUE)
  on.exit(unlink(tmp_proj, recursive = TRUE), add = TRUE)

  # 1. Codigo valido
  good_code <- "x <- 10 * 2\nwriteLines(as.character(x), 'resultado.txt')"
  res_good <- run_spatial_script(good_code, proj_dir = tmp_proj, timeout = 10)

  expect_true(res_good$success)
  expect_null(res_good$error)
  expect_true(file.exists(file.path(tmp_proj, "resultado.txt")))

  # 2. Codigo con error de runtime
  bad_code <- "stop('Error simulado en proyeccion CRS')"
  res_bad <- run_spatial_script(bad_code, proj_dir = tmp_proj, timeout = 10)

  expect_false(res_bad$success)
  expect_true(grepl("Error simulado en proyeccion CRS", res_bad$error))
})

test_that("run_with_self_healing retorna exito directo si el codigo no tiene errores", {
  tmp_proj <- tempfile(pattern = "proj_heal_")
  dir.create(tmp_proj, recursive = TRUE)
  on.exit(unlink(tmp_proj, recursive = TRUE), add = TRUE)

  good_code <- "y <- 42"
  res <- run_with_self_healing(
    chat = NULL,
    code = good_code,
    proj_dir = tmp_proj,
    max_retries = 2
  )

  expect_true(res$success)
  expect_equal(res$attempts, 1)
  expect_null(res$error)
})

test_that("run_with_self_healing autorrepara el script mediante mock de chat", {
  tmp_proj <- tempfile(pattern = "proj_heal_mock_")
  dir.create(tmp_proj, recursive = TRUE)
  on.exit(unlink(tmp_proj, recursive = TRUE), add = TRUE)

  # Primer script tiene error intencional
  failing_code <- "stop('Error: variable continua pasada a scale_fill_manual')"
  repaired_code <- "output_val <- 100\nwriteLines(as.character(output_val), 'fixed.txt')"

  # Mock de chat de ellmer
  mock_chat <- list(
    chat = function(prompt) {
      paste0(
        "Analice el error y aqui esta la correccion:\n\n",
        "```r\n",
        repaired_code,
        "\n```\n"
      )
    }
  )

  retry_logged <- FALSE
  code_gen_logged <- FALSE

  res <- run_with_self_healing(
    chat = mock_chat,
    code = failing_code,
    proj_dir = tmp_proj,
    max_retries = 2,
    on_retry = function(attempt, err) {
      retry_logged <<- TRUE
    },
    on_code_generated = function(new_code, full_resp) {
      code_gen_logged <<- TRUE
    }
  )

  expect_true(res$success)
  expect_equal(res$attempts, 2)
  expect_true(retry_logged)
  expect_true(code_gen_logged)
  expect_true(file.exists(file.path(tmp_proj, "fixed.txt")))
})

test_that("run_with_self_healing respeta max_retries si el error persiste", {
  tmp_proj <- tempfile(pattern = "proj_heal_fail_")
  dir.create(tmp_proj, recursive = TRUE)
  on.exit(unlink(tmp_proj, recursive = TRUE), add = TRUE)

  persistent_failing_code <- "stop('Fallo irrecuperable en capa')"

  # Mock de chat que retorna codigo que sigue fallando
  mock_chat <- list(
    chat = function(prompt) {
      "```r\nstop('Sigue fallando')\n```"
    }
  )

  retry_count <- 0
  res <- run_with_self_healing(
    chat = mock_chat,
    code = persistent_failing_code,
    proj_dir = tmp_proj,
    max_retries = 2,
    on_retry = function(attempt, err) {
      retry_count <<- retry_count + 1
    }
  )

  expect_false(res$success)
  expect_equal(retry_count, 2)
  expect_true(grepl("Sigue fallando", res$error))
})
