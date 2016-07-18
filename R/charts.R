#' \code{LayerCake}
#'
#' @description Plots revenue by year and start year, as a stacked revenue chart.
#' @param data A \code{data.frame} that has the same variables as a \code{RevenueData} object.
#' @param title The title to show above the plot.
#' @return A plotly plot.
#' @importFrom flipStandardCharts Chart
#' @importFrom scales col_numeric
#' @export
LayerCake <- function(data, title = 'Revenue "layercake"')
{
    table <- Table(value ~ start.period + period, data = data, FUN = sum)
    names(dimnames(table)) <- c("Start", "Year")
    table <- t(table)
    #table <- table[nrow(table):1, ] # Reordering or legend
    k <- nrow(table)

    p <- Chart(table, type = "Stacked Area",
          title = title,
          colors = col_numeric("Blues", domain = NULL)(1:(k + 3))[-1:-3], legend.sort.order = "reversed")
    layout(p, y.title = "Revenue")
}



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
#' @importFrom stats supsmu
#' @importFrom plotly config plot_ly
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
    plotly::config(displayModeBar = FALSE)

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


#' \code{Heatmap}
#'
#' @description A heatmap of a matrix.
#' @param x A matrix.
#' @param title The title of the chart.
#' @return A plotly plot.
#' @importFrom flipFormat FormatAsReal
#' @importFrom plotly layout
#' @export
Heatmap <- function(x, title)
{
    if (missing(title))
        title = deparse(substitute(x))
    row.title <- names(dimnames(x))[1]
    if (is.null(row.names))
        row.names = ""
    column.title <- names(dimnames(x))[2]
    if (is.null(row.names))
        row.names = ""
    hover.text <- x#FormatAsReal(x, 2)
    hover.text <- paste(title, hover.text)
    hover.text <- matrix(hover.text, ncol = nrow(x))
    p <- plot_ly(z = x,
            x = rownames(x),
            y = colnames(x),
            text = hover.text,
            hoverinfo = "text",
            type = "heatmap",
            colorbar = list(title = title, min = -1))
    layout(p, xaxis = list(title = row.title),
               yaxis = list(title = column.title))
}

