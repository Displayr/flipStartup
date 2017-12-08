#' \code{LayerCake}
#'
#' @description Plots revenue by year and start year, as a stacked revenue chart.
#' @param data A \code{data.frame} that has the same variables as a \code{RevenueData} object.
#' @param title The title to show above the plot.
#' @param as.table Return a table rather than a chart.
#' @return A plotly plot.
#' @importFrom flipStandardCharts Area
#' @importFrom flipStatistics Table
#' @importFrom scales col_numeric
#' @export
LayerCake <- function(data, title = 'Revenue "layercake"', as.table = FALSE)
{
    table <- Table(value ~ subscriber.from.period + from.period, data = data, FUN = sum)
    names(dimnames(table)) <- c("Start", "Year")
    if (as.table)
        return(table)
    k <- nrow(table)
    date.format <- switch(attr(data, "subscription.length"),
                          "year" = "%Y", "%b %Y")
    p <- Area(t(table), type = "Stacked Area",
          title = title,
          x.tick.format = date.format,
          colors = col_numeric("Blues", domain = NULL)(1:(k + 3))[-1:-3],
          legend.ascending = FALSE)
    layout(p$plotly.plot, yaxis = list(title = "Revenue"))
}
# 
# 
# X <- matrix(runif(9 * 5), 9, dimnames = list(from = 2008:2016, to = 2012:2016))
# 
#     flipStandardCharts::Chart(X, type = "Stacked Area",
#           title = "My chart",
#           colors = scales::col_numeric("Blues", domain = NULL)(1:(k + 3))[-1:-3],
#           legend.ascending = FALSE)
#     
# flipStandardCharts::Area(X, type = "Stacked Area",
#           title = "My chart",
#           colors = scales::col_numeric("Blues", domain = NULL)(1:(k + 3))[-1:-3],
#           legend.ascending = FALSE)
