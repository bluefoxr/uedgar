plot_time_series <- function(dt_emissions){

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
