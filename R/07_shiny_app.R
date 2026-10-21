#' Inicia la aplicacion interactiva de exploracion y copiloto geoespacial
#'
#' Despliega una interfaz web local Shiny con diseno moderno (`bslib`) y conversacional
#' (`shinychat`), conectada a un agente LLM (`ellmer`) y un bucle de ejecucion segura
#' y autorreparacion (self-healing) con `callr`.
#'
#' @param project_dir Directorio raiz del proyecto. Por defecto el actual `"."`.
#' @param port Puerto TCP opcional para servir la app.
#' @param host Direccion IP de escucha (por defecto `"127.0.0.1"`).
#' @param launch.browser Logico. Si es `TRUE`, abre el navegador del sistema automaticamente.
#' @param provider Proveedor de LLM a utilizar (`"gemini"`, `"claude"`, `"openai"`, `"ollama"`). Por defecto `"gemini"`.
#' @param model Nombre de modelo especifico opcional.
#' @param max_retries Numero maximo de intentos de autorreparacion por ejecucion (por defecto 2).
#'
#' @return Inicia el servidor reactivo de Shiny (bloqueante hasta cerrar).
#' @export
#'
#' @examples
#' \dontrun{
#' run_geo_agent()
#' }
run_geo_agent <- function(project_dir = ".",
                          port = NULL,
                          host = "127.0.0.1",
                          launch.browser = TRUE,
                          provider = "gemini",
                          model = NULL,
                          max_retries = 2) {
  required_pkgs <- c("shiny", "bslib", "shinychat", "ellmer", "promises", "callr")
  missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing_pkgs) > 0) {
    stop(
      sprintf("Los siguientes paquetes son necesarios para ejecutar el copiloto interactivo:\n  %s\nPor favor instalalos antes de continuar.",
              paste(missing_pkgs, collapse = ", ")),
      call. = FALSE
    )
  }

  if (identical(provider, "gemini") && !nzchar(Sys.getenv("GEMINI_API_KEY"))) {
    warning("No se detecto la variable de entorno 'GEMINI_API_KEY'. Configurala en tu archivo .Renviron para habilitar la comunicacion con Gemini.", call. = FALSE)
  }

  proj_dir <- normalizePath(project_dir, mustWork = TRUE)

  # UI de la aplicacion
  ui <- bslib::page_sidebar(
    title = "mapagen | Copiloto Geoespacial",
    theme = bslib::bs_theme(version = 5, preset = "bootstrap"),
    sidebar = bslib::sidebar(
      width = 460,
      shiny::div(
        class = "d-flex justify-content-between align-items-center mb-2",
        shiny::h6("Chat Agentico", class = "mb-0 fw-bold"),
        shiny::span(class = "badge bg-primary text-uppercase", provider)
      ),
      shinychat::chat_ui(
        id = "map_chat",
        messages = "**Hola! Soy tu Copiloto Geoespacial de mapagen.**\n\nTengo cargado el contexto de tu proyecto, las reglas cartograficas y el catalogo de datos. Pideme crear mapas coropleticos, filtrar capas, ajustar paletas o calcular metricas espaciales.",
        height = "72vh",
        fill = TRUE
      ),
      shiny::hr(class = "my-2"),
      shiny::div(
        class = "d-flex gap-2",
        shiny::actionButton(
          inputId = "btn_reset_chat",
          label = "Reiniciar Sesion",
          icon = shiny::icon("arrow-rotate-right"),
          class = "btn-outline-secondary btn-sm w-50"
        ),
        shiny::actionButton(
          inputId = "btn_export_script",
          label = "Eyectar a R (.R)",
          icon = shiny::icon("file-code"),
          class = "btn-outline-primary btn-sm w-50",
          disabled = TRUE
        )
      )
    ),
    bslib::navset_card_tab(
      id = "main_tabs",
      bslib::nav_panel(
        title = "Visualizacion",
        shiny::div(
          class = "p-3",
          shiny::uiOutput("render_status"),
          shiny::imageOutput("plot_display", height = "620px")
        )
      ),
      bslib::nav_panel(
        title = "Codigo R Generado",
        shiny::div(
          class = "p-3",
          shiny::p("Ultimo script ejecutable generado o reparado por el agente:"),
          shiny::verbatimTextOutput("code_display"),
          shiny::tags$hr(),
          shiny::h6("Log de Ejecucion en Sandbox (callr):"),
          shiny::verbatimTextOutput("log_display")
        )
      ),
      bslib::nav_panel(
        title = "Catalogo de Datos",
        shiny::div(
          class = "p-3",
          shiny::p("Capas vectoriales, raster y tablas detectadas en 'context_metadata.json':"),
          shiny::verbatimTextOutput("catalog_display")
        )
      ),
      bslib::nav_panel(
        title = "Auditoria de Estilo",
        shiny::div(
          class = "p-3",
          shiny::actionButton("btn_audit", "Ejecutar Auditoria Visual con Gemini Vision", class = "btn-info mb-3"),
          shiny::verbatimTextOutput("audit_display")
        )
      )
    )
  )

  # Server de la aplicacion
  server <- function(input, output, session) {
    catalog_path <- file.path(proj_dir, "context_metadata.json")
    concept_path <- file.path(proj_dir, "project_concept.md")
    style_path   <- file.path(proj_dir, "style.json")

    # Inicializar chat con el contexto del proyecto
    init_chat <- function() {
      geo_chat(
        provider = provider,
        model = model,
        concept_path = concept_path,
        style_path = style_path,
        context_path = catalog_path
      )
    }

    # Estado reactivo
    state <- shiny::reactiveValues(
      chat = init_chat(),
      current_code = "",
      latest_png = NULL,
      executing = FALSE,
      execution_log = "Sin ejecuciones previas.",
      audit_result = "Presiona el boton para auditar la imagen actual contra style.json.",
      status_ui = list(
        type = "info",
        message = "Listo para recibir instrucciones en lenguaje natural."
      )
    )

    # Cargar catalogo de datos
    output$catalog_display <- shiny::renderText({
      if (file.exists(catalog_path)) {
        summary_context(catalog_path)
      } else {
        "Aun no se ha generado 'context_metadata.json'. Ejecuta mapagen::build_context() primero."
      }
    })

    # Renderizar estado visual
    output$render_status <- shiny::renderUI({
      s <- state$status_ui
      color_class <- switch(s$type,
        success = "alert alert-success py-2 px-3 mb-3",
        error   = "alert alert-danger py-2 px-3 mb-3",
        warning = "alert alert-warning py-2 px-3 mb-3",
        info    = "alert alert-info py-2 px-3 mb-3",
        "alert alert-secondary py-2 px-3 mb-3"
      )
      shiny::div(class = color_class, shiny::HTML(s$message))
    })

    # Renderizar imagen de la figura actual
    output$plot_display <- shiny::renderImage({
      shiny::req(state$latest_png)
      list(
        src = state$latest_png,
        contentType = "image/png",
        width = "100%",
        alt = "Mapa generado por mapagen"
      )
    }, deleteFile = FALSE)

    # Observar entradas del chat de shinychat
    shiny::observeEvent(input$map_chat_user_input, {
      prompt <- input$map_chat_user_input
      shiny::req(is.character(prompt) && nzchar(trimws(prompt)))

      if (isTRUE(state$executing)) {
        shiny::showNotification("El agente ya esta procesando una consulta. Por favor espera a que termine.", type = "warning")
        return()
      }

      state$executing <- TRUE
      state$status_ui <- list(type = "info", message = "Generando respuesta del agente...")

      # Streaming asincrono hacia shinychat
      stream <- state$chat$stream_async(prompt)
      p <- shinychat::chat_append("map_chat", stream)

      promises::then(
        p,
        onFulfilled = function(full_text) {
          r_code <- extract_r_code(full_text)

          # Si no hay bloque de codigo R, es un dialogo puramente analitico/explicativo
          if (!nzchar(r_code)) {
            state$executing <- FALSE
            state$status_ui <- list(type = "info", message = "Respuesta analitica entregada.")
            return()
          }

          state$status_ui <- list(type = "warning", message = "Ejecutando script geoespacial en sandbox aislado...")
          state$current_code <- r_code
          output$code_display <- shiny::renderText({ r_code })

          # Ejecucion aislada con bucle autonomo de autorreparacion (Self-Healing)
          exec_res <- run_with_self_healing(
            chat = state$chat,
            code = r_code,
            proj_dir = proj_dir,
            max_retries = max_retries,
            on_retry = function(attempt, err) {
              shinychat::chat_append(
                "map_chat",
                sprintf("[AVISO] Fallo en ejecucion: `%s`\n\nIniciando autorreparacion con Gemini (Intento %d/%d)...", err, attempt, max_retries)
              )
              state$status_ui <- list(
                type = "warning",
                message = sprintf("Error detectado. Autorreparando con Gemini (Intento %d/%d)...", attempt, max_retries)
              )
            },
            on_code_generated = function(new_code, full_resp) {
              shinychat::chat_append("map_chat", full_resp)
              state$current_code <- new_code
              output$code_display <- shiny::renderText({ new_code })
            }
          )

          state$executing <- FALSE
          state$current_code <- exec_res$code
          state$execution_log <- if (is.null(exec_res$log)) "Sin mensajes de consola." else paste(exec_res$log, collapse = "\n")
          output$code_display <- shiny::renderText({ exec_res$code })
          output$log_display <- shiny::renderText({ state$execution_log })

          if (isTRUE(exec_res$success)) {
            state$latest_png <- exec_res$image_path
            state$status_ui <- list(
              type = "success",
              message = sprintf("<b>Renderizado exitoso</b> (intentos: %d): %s", exec_res$attempts, basename(exec_res$image_path %||% "figura"))
            )
            shiny::updateActionButton(session, "btn_export_script", disabled = FALSE)
          } else {
            state$status_ui <- list(
              type = "error",
              message = sprintf("<b>Error no recuperable</b> tras %d intentos: %s", exec_res$attempts, exec_res$error)
            )
            shinychat::chat_append(
              "map_chat",
              sprintf("[ERROR] Error no recuperable tras autorreparacion: `%s`\n\nPuedes revisar el codigo y la traza en la pestana 'Codigo R Generado' o guiar al agente con una nueva instruccion.", exec_res$error)
            )
          }
        },
        onRejected = function(err) {
          state$executing <- FALSE
          state$status_ui <- list(type = "error", message = paste("Error en la comunicacion con el LLM:", err$message))
          shiny::showNotification(paste("Error en LLM:", err$message), type = "error")
        }
      )
    })

    # Reiniciar sesion de chat
    shiny::observeEvent(input$btn_reset_chat, {
      state$chat <- init_chat()
      shinychat::chat_append("map_chat", "[SESION REINICIADA] El historial previo se ha limpiado y el contexto se ha recargado.")
      shiny::showNotification("Sesion de chat reiniciada.", type = "message")
    })

    # Eyectar script a RStudio
    shiny::observeEvent(input$btn_export_script, {
      shiny::req(state$current_code)
      timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
      script_file <- file.path(proj_dir, "scripts", paste0("analisis_", timestamp, ".R"))
      dir.create(dirname(script_file), recursive = TRUE, showWarnings = FALSE)
      writeLines(state$current_code, script_file)

      shiny::showNotification(sprintf("Script exportado a: %s", basename(script_file)), type = "message")

      if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
        rstudioapi::navigateToFile(script_file)
      }
    })

    # Auditoria visual con Gemini Vision
    shiny::observeEvent(input$btn_audit, {
      shiny::req(state$latest_png)
      shiny::showNotification("Ejecutando auditoria con Gemini Vision...", type = "message")
      tryCatch({
        audit_res <- audit_figure(
          id_or_path = state$latest_png,
          style_path = file.path(proj_dir, "style.json")
        )
        state$audit_result <- audit_res
        output$audit_display <- shiny::renderText({ audit_res })
      }, error = function(e) {
        state$audit_result <- paste("Error en auditoria:", e$message)
        output$audit_display <- shiny::renderText({ state$audit_result })
      })
    })
  }

  shiny::shinyApp(ui = ui, server = server, options = list(port = port, host = host, launch.browser = launch.browser))
}
