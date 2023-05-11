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
            pickerInput(
              inputId = "country",
              label = "Countries",
              choices = valid_countries
            ),
            pickerInput(
              inputId = "sector",
              label = "Sector",
              choices = c("ALL", valid_sectors)
            )
        ),
        shinydashboardPlus::box(title = "Time range", collapsible = TRUE,
            sliderInput("year_range", label = NULL,
                        min = 1970, max = 2020, step = 1,
                        value = c(2000, 2020), sep = ""))
      ),
      shinydashboardPlus::box(title = "Plot", id = "plot_box", width = 12, footer = "",
          textOutput("message_text"),
          plotly::plotlyOutput("ts_plot"))
    ),
    title = "uEDGAR"
  )

  server <- function(input, output) {

    output$ts_plot <- plotly::renderPlotly({

      sector <- if(input$sector == "ALL") NULL else input$sector

      emi <- get_uncertain_emissions(
        con,
        substances = "CO2",
        years = input$year_range[1]:input$year_range[2],
        countries = input$country,
        sectors = sector)

      if(is.null(emi)){
        shinydashboardPlus::updateBox(
          "plot_box",
          action = "update",
          options = list(
            title = "No data available for this query."
          )
        )
        return(NULL)
      } else {
        shinydashboardPlus::updateBox(
          "plot_box",
          action = "update",
          options = list(
            title = "Plot"
          )
        )
      }

      plot_time_series(emi)

    })

  }

  # Run the application
  shinyApp(ui = ui, server = server)

}
