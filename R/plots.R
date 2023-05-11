#' Plot time series of emissions
#'
#' Plots a time series of emissions, with confidence intervals. It takes a
#' data frame with expected columns "Year", "Emissions", "Emissions_Min",
#' "Emissions_Max".
#'
#' @param dt_emissions
#'
#' @return Plotly plot
#' @export
plot_time_series <- function(dt_emissions){

  missing_cols <- setdiff(
    c("Year", "Emissions", "Emissions_Min", "Emissions_Max"),
    names(dt_emissions)
  )

  if(length(missing_cols) > 0){
    stop("Expected columns missing from input data frame: ", toString(missing_cols))
  }

  fig <- plotly::plot_ly(dt_emissions, x = ~Year, y = ~Emissions_Max, type = 'scatter', mode = 'lines',
                 line = list(color = 'transparent'),
                 showlegend = FALSE, name = 'Max')

  fig <- fig |> plotly::add_trace(y = ~Emissions_Min, type = 'scatter', mode = 'lines',
                           fill = 'tonexty', fillcolor='rgba(0,100,80,0.2)', line = list(color = 'transparent'),
                           showlegend = FALSE, name = 'Min')

  fig <- fig |> plotly::add_trace(x = ~Year, y = ~Emissions, type = 'scatter', mode = 'lines',
                           line = list(color='rgb(0,100,80)'),
                           name = 'Nominal')

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
