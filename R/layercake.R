#' \code{LayerCake}
#'
#' @description Plots revenue by year and start year, as a stacked revenue chart.
#' @param data A \code{data.frame} that has the same variables as a \code{RevenueData} object.
#' @param title The title to show above the plot.
#' @return A plotly plot.
#' @importFrom flipStandardCharts Chart
#' @importFrom flipStatistics Table
#' @importFrom scales col_numeric
#' @export
LayerCake <- function(data, title = 'Revenue "layercake"')
{
    table <- Table(value ~ subscriber.from.period + period, data = data, FUN = sum)
    names(dimnames(table)) <- c("Start", "Year")
    k <- nrow(table)

    p <- Chart(table, type = "Stacked Area",
          title = title,
          colors = col_numeric("Blues", domain = NULL)(1:(k + 3))[-1:-3],
          legend.ascending = FALSE)
    layout(p, yaxis = list(title = "Revenue"))
}


