#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom bs4Dash dashboardPage dashboardHeader
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    h1("Graph Simulator", style="background-color: #23b5fe;
                                 color: white;
                                 padding: 20px 15px;
                                 margin: 0;
                                 margin-bottom: 10px"),
    fluidPage(
      tabsetPanel(
        tabPanel("Input",
                 h3("Load network graph"),
                 fileInput("upload", label = NULL),
                 plotly::plotlyOutput("network")
        ),
        tabPanel("Simulation",
                 numericInput(
                   "input_people",
                   label = "Set the number of people that should enter the network.",
                   value = 100,
                   min = 10,
                   max = 5000,
                   step = 1
                 ),
                 textOutput("error_people"),
                 actionButton(
                   "run",
                   label = "Compute simulation"
                 ),
                 h3("Simulation of the graph"),
                 textOutput("error_graph"),
                 plotly::plotlyOutput("simulation_plot")
        )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "graphapp"
    ),
    tags$style("#error_people, #error_graph { color: red; }"),
    shinyjs::useShinyjs()
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
