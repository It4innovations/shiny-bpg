#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinyjs
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  r <- reactiveValues(
   df_network = NULL
  )

  # Upload network ----
  observeEvent(input$upload,
  {
    output$error_people <- renderText("")
    output$error_graph <- renderText("")
    r$df_network <- utils::read.csv(input$upload$datapath, header = TRUE)
  })

  # Plotly ----
  output$network <- plotly::renderPlotly(
  {
    req(r$df_network)
    plot_network(r$df_network )
  })


  # Setup SIMULATION -----------------------------------------------------------
  r_simulation <- reactiveValues(
    continue = NULL,
    simulation = NULL,
    animation = NULL
  )

  # Observe if button was clicked ----
  observeEvent(input$run,{
    r_simulation$continue <- NULL
    r_simulation$simulation <- NULL
    r_simulation$animation <- NULL
    shinyjs::disable("run")
    print("Starting...")
    r_simulation$continue <- TRUE
  })

  # Simulation ----
  observeEvent(r_simulation$continue, {
    # Check input_people
    if (input$input_people > 5000 |
        input$input_people < 10)
    {
        # Output error if it is out of range
        output$error_people <- renderText("Input people should be integer between 10 and 5000!")
        shinyjs::enable("run")
        input_people <- NULL
    } else {
        input_people <- round(input$input_people)
    }

    print("Checking..")
    # Check network ----
    if (is.null(r$df_network)) {
        # output error if there is no graph loaded
        shinyjs::enable("run")
        output$error_graph <- renderText("The graph is not loaded!")
    } else {
        network_check <- TRUE
        if (check_network( r$df_network ) > 0)
        {
            network_check <- FALSE
            shinyjs::enable("run")
        }
    }

    req(input_people, r$df_network, network_check)
    output$error_people <- renderText("")
    output$error_graph <- renderText("")
    print("Simulating...")
    res <- simulate_flow(df_edges = r$df_network, input_people = input_people)
    r_simulation$simulation <- res[[1]]
  })

  observeEvent(r_simulation$simulation,
  {
    print("Animating...")
      r_simulation$animation <-
          animate_simulation(df_edges = r$df_network, r_simulation$simulation)
  })

  observeEvent(r_simulation$animation,{
    output$simulation_plot <- plotly::renderPlotly({r_simulation$animation})
    shinyjs::enable("run")
  })

}
