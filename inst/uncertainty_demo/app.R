library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)

countries <- c("Italy", "UK", "Germany", "Portugal")
flags <- c(
  "https://flagicons.lipis.dev/flags/4x3/it.svg",
  "https://flagicons.lipis.dev/flags/4x3/gb.svg",
  "https://flagicons.lipis.dev/flags/4x3/de.svg",
  "https://flagicons.lipis.dev/flags/4x3/pt.svg"
)

ui <-  dashboardPage(

  options = list(sidebarExpandOnHover = TRUE),
  header = dashboardHeader(title = "uEDGAR"),
  sidebar = dashboardSidebar(minified = FALSE, collapsed = FALSE),
  body = dashboardBody(
    box(title = "Select data", collapsible = TRUE,
        multiInput(
          inputId = "Id010",
          label = "Countries :",
          choices = NULL,
          choiceNames = lapply(seq_along(countries),
                               function(i) tagList(tags$img(src = flags[i],
                                                            width = 20,
                                                            height = 15), countries[i])),
          choiceValues = countries
        )),
    box(title = "Plot")
  ),
  controlbar = dashboardControlbar(),
  title = "uEDGAR"
)

server <- function(input, output) {


}

# Run the application
shinyApp(ui = ui, server = server)
