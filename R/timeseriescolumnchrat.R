

#' \code{TimeSeriesColumnChart}
#'
#' @description Plots a time series as columns, optionally smoothing the data.
#' @param x A vector containing values, where the names indicate the dates.
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
        ## Fixing the dates.
        #dates <- period.names
        #n.dashes <- nchar(x[1]) - nchar(gsub("-", "", x[1]))
        #if (n.dashes == 0) # yearly data
        #    dates <- paste0(dates,"-07-01")
        #else if (n.dashes == 1) # montly or quarterly
        #    dates <- paste0(period.names,"-15")
        #y.fitted <- supsmu(as.Date(dates), x)$y

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
                            # range = range(dates),
                             showgrid = FALSE),
                yaxis = list(title = ytitle, tickformat = tickformat),
                title = title)
    p
}

