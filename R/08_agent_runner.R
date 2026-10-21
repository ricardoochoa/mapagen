#' Ejecuta de forma aislada un script de R en un subproceso con callr
#'
#' Escribe el codigo R a un archivo temporal y lo ejecuta dentro del directorio
#' del proyecto de forma segura con un limite de tiempo para evitar bloqueos.
#'
#' @param code Cadena de texto con el codigo R ejecutable.
#' @param proj_dir Directorio raiz del proyecto. Por defecto `" ."`.
#' @param timeout Tiempo maximo de ejecucion en segundos (por defecto 60).
#'
#' @return Lista con el resultado: `success` (logico), `error` (mensaje o NULL),
#'   `image_path` (ruta a la imagen PNG mas reciente o NULL), `code` y `log`.
#' @export
run_spatial_script <- function(code, proj_dir = ".", timeout = 60) {
  if (!requireNamespace("callr", quietly = TRUE)) {
    stop("El paquete 'callr' es necesario para la ejecucion aislada.", call. = FALSE)
  }

  proj_dir <- normalizePath(proj_dir, mustWork = TRUE)
  temp_script <- file.path(proj_dir, "scripts", "temp_agent_exec.R")
  dir.create(dirname(temp_script), recursive = TRUE, showWarnings = FALSE)
  writeLines(code, temp_script)

  # Registrar timestamp antes de ejecutar para buscar imagenes nuevas
  t0 <- Sys.time()

  log_file <- tempfile(pattern = "exec_log_", fileext = ".txt")
  on.exit(if (file.exists(log_file)) unlink(log_file), add = TRUE)

  exec_res <- tryCatch({
    out <- callr::r_safe(
      function(script_path, wd) {
        setwd(wd)
        source(script_path, local = FALSE)
      },
      args = list(script_path = temp_script, wd = proj_dir),
      timeout = timeout,
      show = FALSE,
      stdout = log_file,
      stderr = log_file
    )
    log_content <- if (file.exists(log_file)) paste(readLines(log_file, warn = FALSE), collapse = "\n") else ""
    list(
      success = TRUE,
      error = NULL,
      log = log_content
    )
  }, error = function(e) {
    log_content <- if (file.exists(log_file)) paste(readLines(log_file, warn = FALSE), collapse = "\n") else ""
    list(
      success = FALSE,
      error = conditionMessage(e),
      log = log_content
    )
  })

  latest_png <- NULL
  if (exec_res$success) {
    fig_dir <- file.path(proj_dir, "output", "figures")
    if (dir.exists(fig_dir)) {
      figs <- list.files(fig_dir, pattern = "\\.png$", full.names = TRUE)
      if (length(figs) > 0) {
        info <- file.info(figs)
        # Buscar imagenes creadas o modificadas desde poco antes de t0
        latest_png <- figs[which.max(info$mtime)]
      }
    }
  }

  list(
    success = exec_res$success,
    error = exec_res$error,
    image_path = latest_png,
    code = code,
    log = exec_res$log
  )
}

#' Construye el prompt de autorreparacion para el LLM a partir de un error de runtime
#'
#' @param error_msg Mensaje de error capturado de la ejecucion.
#' @param code Codigo R que provoco el error.
#'
#' @return Cadena de texto formateada para el prompt de reparacion.
#' @export
build_repair_prompt <- function(error_msg, code) {
  sprintf(
    paste0(
      "La ejecucion del codigo R anterior genero el siguiente error de runtime:\n\n",
      "```\n[ERROR]: %s\n```\n\n",
      "Codigo ejecutado previamente:\n```r\n%s\n```\n\n",
      "Por favor analiza la causa raiz del error (por ejemplo: verificacion de proyecciones o geometrias en sf, compatibilidad de tipos de variables en escalas de ggplot2, o nombres de columnas segun el catalogo de datos) y proporciona el codigo R completo y corregido dentro de un unico bloque ```r ... ```. Incluye siempre la invocacion a mapagen::theme_from_json() y mapagen::export_iteration()."
    ),
    error_msg,
    code
  )
}

#' Ejecuta codigo R con un bucle autonomo de autorreparacion (Self-Healing)
#'
#' Intenta ejecutar el script con `run_spatial_script()`. Si ocurre un fallo en runtime
#' (por ejemplo en `sf` o `ggplot2`), envia automaticamente el mensaje de error y el codigo
#' previo al agente LLM (`ellmer::Chat`), extrae el codigo corregido y reintenta la ejecucion
#' hasta un numero maximo de intentos.
#'
#' @param chat Objeto `ellmer::Chat` con la sesion activa.
#' @param code Codigo R inicial a ejecutar.
#' @param proj_dir Directorio raiz del proyecto.
#' @param max_retries Numero maximo de reintentos permitidos (por defecto 2).
#' @param timeout Tiempo limite por intento en segundos.
#' @param on_retry Funcion callback opcional invocada al fallar un intento: `function(attempt, error_msg)`.
#' @param on_code_generated Funcion callback opcional invocada cuando el LLM genera nuevo codigo: `function(new_code, full_text)`.
#'
#' @return Lista con el resultado final: `success`, `error`, `image_path`, `code`, `attempts` y `log`.
#' @export
run_with_self_healing <- function(chat,
                                  code,
                                  proj_dir = ".",
                                  max_retries = 2,
                                  timeout = 60,
                                  on_retry = NULL,
                                  on_code_generated = NULL) {
  current_code <- code
  current_res <- run_spatial_script(current_code, proj_dir = proj_dir, timeout = timeout)

  if (current_res$success) {
    return(list(
      success = TRUE,
      error = NULL,
      image_path = current_res$image_path,
      code = current_code,
      attempts = 1,
      log = current_res$log
    ))
  }

  attempt <- 1
  while (attempt <= max_retries) {
    # Notificar via callback
    if (is.function(on_retry)) {
      tryCatch(on_retry(attempt, current_res$error), error = function(e) NULL)
    }

    # Formular prompt de reparacion
    repair_prompt <- build_repair_prompt(current_res$error, current_code)

    # Consultar al agente
    repair_response <- tryCatch({
      chat$chat(repair_prompt)
    }, error = function(e) {
      paste("Error al consultar el LLM durante autorreparacion:", e$message)
    })

    new_code <- extract_r_code(repair_response)

    if (is.function(on_code_generated)) {
      tryCatch(on_code_generated(new_code, repair_response), error = function(e) NULL)
    }

    if (!nzchar(new_code)) {
      # El LLM no proporciono codigo en el intento
      break
    }

    current_code <- new_code
    current_res <- run_spatial_script(current_code, proj_dir = proj_dir, timeout = timeout)

    if (current_res$success) {
      return(list(
        success = TRUE,
        error = NULL,
        image_path = current_res$image_path,
        code = current_code,
        attempts = attempt + 1,
        log = current_res$log
      ))
    }

    attempt <- attempt + 1
  }

  list(
    success = FALSE,
    error = current_res$error,
    image_path = NULL,
    code = current_code,
    attempts = attempt,
    log = current_res$log
  )
}
