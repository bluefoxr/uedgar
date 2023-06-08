#' Plot time series of emissions
#'
#' Plots a time series of emissions, with confidence intervals. It takes a
#' data frame with expected columns "Year", "Emissions", "Emissions_Min",
#' "Emissions_Max" and "Country". If more than one country is present, it will
#' plot multiple traces.
#'
#' @param dt_emissions
#'
#' @return Plotly plot
#' @export
iplot_time_series <- function(dt_emissions){

  missing_cols <- setdiff(
    c("Year", "Emissions", "Emissions_Min", "Emissions_Max", "Country"),
    names(dt_emissions)
  )

  if(length(missing_cols) > 0){
    stop("Expected columns missing from input data frame: ", toString(missing_cols))
  }

  fig <- plotly::plot_ly()

  for(country in unique(dt_emissions[,Country])){

    dti <- dt_emissions[Country == country] |> as.data.frame()

    fig <- plotly::add_trace(fig, data = dti, x = ~Year, y = ~Emissions_Max, type = 'scatter', mode = 'lines',
                             line = list(color = 'transparent'),
                             showlegend = FALSE, name = 'Max')

    fig <- plotly::add_trace(fig, data = dti, x = ~Year, y = ~Emissions_Min, type = 'scatter', mode = 'lines',
                             fill = 'tonexty',  line = list(color = 'transparent'),
                             showlegend = TRUE, name = country)

    fig <- plotly::add_trace(fig, data = dti, x = ~Year, y = ~Emissions, type = 'scatter', mode = 'lines',
                             line = list(color='grey'), showlegend = FALSE,
                             name = country)

  }

  fig <- fig |> plotly::layout(title = NULL,
                        paper_bgcolor='rgb(255,255,255)', plot_bgcolor='rgb(229,229,229)',
                        xaxis = list(title = "Year",
                                     gridcolor = 'rgb(255,255,255)',
                                     showgrid = TRUE,
                                     showline = FALSE,
                                     showticklabels = TRUE,
                                     tickcolor = 'rgb(127,127,127)',
                                     ticks = 'outside',
                                     zeroline = FALSE),
                        yaxis = list(title = "Emissions",
                                     gridcolor = 'rgb(255,255,255)',
                                     showgrid = TRUE,
                                     showline = FALSE,
                                     showticklabels = TRUE,
                                     tickcolor = 'rgb(127,127,127)',
                                     ticks = 'outside',
                                     zeroline = FALSE))

  fig


}
