#' Run uncertainties app
#'
#' App which allows interactive plotting of emissions with uncertainties.
#'
#' @export
#'
#' @import shiny
run_app <- function(){

  # establish db connection
  con <- connect_to_edgar()

  ui <-  shinydashboardPlus::dashboardPage(

    options = list(sidebarExpandOnHover = TRUE),
    header = shinydashboardPlus::dashboardHeader(title = "uEDGAR"),
    sidebar = shinydashboardPlus::dashboardSidebar(minified = FALSE, collapsed = FALSE),

    body = shinydashboard::dashboardBody(
      fluidRow(
        shinydashboardPlus::box(title = "Select country", collapsible = TRUE,
            # shinyWidgets::pickerInput(
            #   inputId = "country",
            #   label = "Countries",
            #   choices = valid_countries
            # ),
            shinyWidgets::multiInput(
              inputId = "country",
              label = "Countries",
              choices = valid_countries, selected = valid_countries[1]
            ),
            textOutput("ISO3"),
            shinyWidgets::pickerInput(
              inputId = "sector",
              label = "Sector",
              choices = c("ALL", valid_sectors)
            ),
            shinyWidgets::pickerInput(
              inputId = "substance",
              label = "Substance",
              choices = c("CO2", "CH4", "N2O")
            )
        ),
        shinydashboardPlus::box(title = "Time range", collapsible = TRUE,
            sliderInput("year_range", label = NULL,
                        min = 1970, max = 2020, step = 1,
                        value = c(2000, 2020), sep = ""))
      ),
      shinydashboardPlus::box(title = "Plot", id = "plot_box", width = 12, footer = "",
          #textOutput("nodata_text"),
          plotly::plotlyOutput("ts_plot"))
    ),
    title = "uEDGAR"
  )

  server <- function(input, output) {

    # emi <- reactiveVal(NULL)
    #
    # # df of emissions
    # observe({
    #
    #   sector <- if(input$sector == "ALL") NULL else input$sector
    #
    #   emissions <- get_uncertain_emissions(
    #     con,
    #     substances = input$substance,
    #     years = input$year_range[1]:input$year_range[2],
    #     countries = input$country,
    #     sectors = sector
    #   )
    #
    #   emi(emissions)
    # })

    output$ts_plot <- plotly::renderPlotly({

      req(input$country)

      sector <- if(input$sector == "ALL") NULL else input$sector

      emissions <- get_uncertain_emissions(
        con,
        substances = input$substance,
        years = input$year_range[1]:input$year_range[2],
        countries = input$country,
        sectors = sector
      )

      iplot_time_series(emissions)
    })

    # output$nodata_text <- renderText({
    #   toString(input$country)
    #   # if(is.null(emi())){
    #   #   "No data available for this query..."
    #   # } else {
    #   #   ""
    #   # }
    # })

  }

  # Run the application
  shinyApp(ui = ui, server = server)

}
