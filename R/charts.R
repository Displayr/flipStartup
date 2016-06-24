#' \code{TimeSeriesColumnChart}
#'
#' @description Plots a time series as columns, optionally smoothing the data.
#' @param x A vector containing values, where the names indicate the dates.
#' @param title The title of the chart.
#' @param ytitle The title to show on the y-axis.
#' @param series.name E.g., "churn".
#' @param smooth Smooth the data using \code{supmsu}.
#' @param ... Parameters to be passed to plotly.
#' @return A plotly plot.
##' @importFrom plotly plot_ly
#' @importFrom stats supsmu
#' @export
TimeSeriesColumnChart <- function(x, smooth = TRUE, title = "", ytitle = "",  series.name = "",  ...)
{
    period.names <- names(x)
    # Creating the initial plot.
    p <- plot_ly(
        x = period.names,
        y = x,
        name = series.name,
        type = "bar")
    # Smoothing.
    if (smooth)
    {
        # Fixing the dates.
        dates <- period.names
        n.dashes <- nchar(x[1]) - nchar(gsub("-", "", x[1]))
        if (n.dashes == 0) # yearly data
            dates <- paste0(dates,"-07-01")
        else if (n.dashes == 1) # montly or quarterly
            dates <- paste0(period.names,"-15")
        y.fitted <- supsmu(as.Date(dates), x)$y
        p <- add_trace(p,
                       x = period.names,
                       y = y.fitted,
                       name = "Fitted",
                       type = "line")
    }
    p <- layout(p,
                xaxis = list(title = "",
                            # range = range(dates),
                             showgrid = FALSE),
                yaxis = list(title = ytitle), title = title)
    p
}
