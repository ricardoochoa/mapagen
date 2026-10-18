#' Inicia la aplicacion interactiva de exploracion y copiloto geoespacial
#'
#' Despliega una interfaz web local Shiny con diseno moderno (`bslib`),
#' implementando el flujo de trabajo de 3 fases: interpretacion/planificacion,
#' ejecucion segura en segundo plano con `callr` y ejeccion de scripts a RStudio.
#'
#' @param project_dir Directorio raiz del proyecto. Por defecto el actual `"."`.
#' @param port Puerto TCP opcional para servir la app.
#' @param host Direccion IP de escucha (por defecto `"127.0.0.1"`).
#' @param launch.browser Lógico. Si es `TRUE`, abre el navegador del sistema automaticamente.
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
                          launch.browser = TRUE) {
  if (!requireNamespace("shiny", quietly = TRUE) || !requireNamespace("bslib", quietly = TRUE)) {
    stop("Los paquetes 'shiny' y 'bslib' son necesarios para ejecutar el asistente interactivo.", call. = FALSE)
  }

  proj_dir <- normalizePath(project_dir, mustWork = TRUE)

  # UI de la aplicacion
  ui <- bslib::page_navbar(
    title = "mapagen | Copiloto Geoespacial",
    theme = bslib::bs_theme(version = 5, preset = "bootstrap"),
    sidebar = bslib::sidebar(
      width = 400,
      shiny::h5("Asistente Conversacional"),
      shiny::textAreaInput(
        inputId = "user_prompt",
        label = "Instruccion en Lenguaje Natural:",
        placeholder = "Ej. Genera un mapa coropletico de marginacion por municipio aplicando la paleta institucional y calculando cuantiles...",
        rows = 4
      ),
      shiny::actionButton(
        inputId = "btn_plan",
        label = "1. Proponer Plan de Accion",
        class = "btn-primary w-100 mb-2"
      ),
      shiny::uiOutput("plan_box"),
      shiny::hr(),
      shiny::actionButton(
        inputId = "btn_run_code",
        label = "2. Aprobar y Generar Mapa",
        class = "btn-success w-100 mb-2",
        disabled = TRUE
      ),
      shiny::actionButton(
        inputId = "btn_export_script",
        label = "3. Eyectar a Script (.R)",
        class = "btn-outline-secondary w-100",
        disabled = TRUE
      )
    ),
    bslib::nav_panel(
      title = "Visualizacion",
      shiny::div(
        class = "p-3",
        shiny::uiOutput("render_status"),
        shiny::imageOutput("plot_display", height = "550px")
      )
    ),
    bslib::nav_panel(
      title = "Codigo R Generado",
      shiny::div(
        class = "p-3",
        shiny::p("Codigo reproducible generado por el modelo para evaluacion o edicion:"),
        shiny::verbatimTextOutput("code_display")
      )
    ),
    bslib::nav_panel(
      title = "Catalogo de Datos",
      shiny::div(
        class = "p-3",
        shiny::p("Capas y tablas detectadas en 'context_metadata.json':"),
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

  # Server de la aplicacion
  server <- function(input, output, session) {
    # Estado reactivo
    state <- shiny::reactiveValues(
      chat = NULL,
      current_plan = NULL,
      current_code = "",
      latest_png = NULL,
      executing = FALSE,
      audit_result = "Presiona el boton para auditar la imagen actual contra style.json."
    )

    # Cargar catalogo inicial
    catalog_path <- file.path(proj_dir, "context_metadata.json")
    output$catalog_display <- shiny::renderText({
      if (file.exists(catalog_path)) {
        summary_context(catalog_path)
      } else {
        "Aun no se ha generado 'context_metadata.json'. Ejecuta mapagen::build_context() primero."
      }
    })

    # Fase 1: Proponer Plan
    shiny::observeEvent(input$btn_plan, {
      req(input$user_prompt)
      shiny::showNotification("Consultando al agente para formular el plan...", type = "message")

      tryCatch({
        check_ellmer()
        state$chat <- geo_chat(
          concept_path = file.path(proj_dir, "project_concept.md"),
          style_path = file.path(proj_dir, "style.json"),
          context_path = catalog_path
        )

        plan_prompt <- sprintf(
          "El usuario solicita: '%s'. Formula un PLAN DE ACCION claro y conciso en 3-4 pasos, mencionando que capas de datos usaras, que operaciones espaciales (uniones, filtros, reproyecciones) se haran y que tipo de visualizacion propones. No incluyas codigo todavia.",
          input$user_prompt
        )

        plan_response <- state$chat$chat(plan_prompt)
        state$current_plan <- plan_response

        shiny::updateActionButton(session, "btn_run_code", disabled = FALSE)
      }, error = function(e) {
        shiny::showNotification(paste("Error al consultar el LLM:", e$message), type = "error")
      })
    })

    output$plan_box <- shiny::renderUI({
      req(state$current_plan)
      shiny::div(
        class = "card p-2 mt-2 bg-light border-primary",
        shiny::h6("Plan Propuesto:"),
        shiny::p(state$current_plan, style = "font-size: 0.88rem; white-space: pre-line;")
      )
    })

    # Fase 2: Aprobar, Generar y Ejecutar
    shiny::observeEvent(input$btn_run_code, {
      req(state$chat)
      shiny::showNotification("Traduciendo plan a codigo R y renderizando...", type = "message")

      tryCatch({
        code_prompt <- "Excelente. Ahora traduce el plan acordado a codigo R ejecutable estricto. Aplica mapagen::theme_from_json() y exporta la figura usando mapagen::export_iteration(p, id = 'mapa_shiny', caption = '...', summary_data = ...). Entrega el bloque ```r completo."
        code_res <- state$chat$chat(code_prompt)
        r_code <- extract_r_code(code_res)
        state$current_code <- r_code

        output$code_display <- shiny::renderText({ r_code })

        # Ejecucion segura en proceso aislado con callr
        if (requireNamespace("callr", quietly = TRUE)) {
          temp_script <- file.path(proj_dir, "scripts", "temp_agent_exec.R")
          dir.create(dirname(temp_script), recursive = TRUE, showWarnings = FALSE)
          writeLines(r_code, temp_script)

          output$render_status <- shiny::renderUI({
            shiny::p("Ejecutando script de forma segura en proceso aislado...", class = "text-primary")
          })

          res <- callr::r_safe(
            function(script_path, wd) {
              setwd(wd)
              source(script_path, local = FALSE)
            },
            args = list(script_path = temp_script, wd = proj_dir),
            timeout = 60,
            show = FALSE
          )

          # Buscar la imagen mas reciente en output/figures
          figs <- list.files(file.path(proj_dir, "output", "figures"), pattern = "\\.png$", full.names = TRUE)
          if (length(figs) > 0) {
            info <- file.info(figs)
            latest <- figs[which.max(info$mtime)]
            state$latest_png <- latest
            output$render_status <- shiny::renderUI({
              shiny::p(paste("Renderizado exitoso:", basename(latest)), class = "text-success fw-bold")
            })
          }
        }

        shiny::updateActionButton(session, "btn_export_script", disabled = FALSE)
      }, error = function(e) {
        output$render_status <- shiny::renderUI({
          shiny::p(paste("Error durante la ejecucion:", e$message), class = "text-danger fw-bold")
        })
        shiny::showNotification(paste("Error al ejecutar:", e$message), type = "error")
      })
    })

    # Mostrar imagen renderizada
    output$plot_display <- shiny::renderImage({
      req(state$latest_png)
      list(
        src = state$latest_png,
        contentType = "image/png",
        width = "100%",
        alt = "Mapa generado por mapagen"
      )
    }, deleteFile = FALSE)

    # Fase 3: Ejeccion a RStudio
    shiny::observeEvent(input$btn_export_script, {
      req(state$current_code)
      timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
      script_file <- file.path(proj_dir, "scripts", paste0("analisis_", timestamp, ".R"))
      dir.create(dirname(script_file), recursive = TRUE, showWarnings = FALSE)
      writeLines(state$current_code, script_file)

      shiny::showNotification(sprintf("Script exportado a: %s", basename(script_file)), type = "message")

      if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
        rstudioapi::navigateToFile(script_file)
      }
    })

    # Auditoria visual
    shiny::observeEvent(input$btn_audit, {
      req(state$latest_png)
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
