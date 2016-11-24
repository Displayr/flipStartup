

#' \code{TimeSeriesColumnChart}
#'
#' @description Plots a time series as columns, optionally smoothing the data.
#' @param x A vector containing values, where the names indicate the dates.
#' @param by The period that has been used to aggregate the data (day, week, month, quarter, year).
#' @param title The title of the chart.
#' @param ytitle The title to show on the y-axis.
#' @param series.name E.g., "churn".
#' @param smooth Smooth the data using \code{supmsu}.
#' @param tickformat Plotlytickformat \code{tickformat}.
#' @return A plotly plot.
#' @importFrom stats supsmu
#' @importFrom plotly config plot_ly
#' @export
TimeSeriesColumnChart <- function(x,
                                  by = "day",
                                  title = "",
                                  ytitle = "",
                                  series.name = "",
                                  tickformat = NULL,
                                  smooth = TRUE)
{
    period.names <- if(by == "year") x else PeriodNameToDate(names(x), by)
    # Creating the initial plot.
    p <- plot_ly(
        x = ~period.names,
        y = ~x,
        name = series.name,
        type = "bar")
    p <- config(p, displayModeBar = FALSE)
    # Smoothing.
    if (smooth)
    {
        y.fitted <- supsmu(period.names, x)$y
        p <- add_trace(p,
            x = period.names,
            y = y.fitted,
            name = "Fitted",
            type = "scatter",
            mode = "lines")
    }
    p <- layout(p,
                showlegend = FALSE,
                xaxis = list(title = "",
                             mode = "category",
                             showgrid = FALSE),
                yaxis = list(title = ytitle, tickformat = tickformat),
                title = title)
    p
}

