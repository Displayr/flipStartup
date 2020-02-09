#' \code{RevenueByCohort}
#'
#' @description Revenue by year and start year, as a stacked revenue chart.
#' @param data A \code{data.frame} that has the same variables as a \code{RevenueData} object.
#' @param volume Weights the results by volume. FALSE doesn't do anything.
#' @param remove.last Remove the final period (as usually is incomplete).
#' @return A table.
#' @importFrom flipStandardCharts Area
#' @importFrom flipStatistics Table
#' @importFrom scales col_numeric
#' @export
RevenueByCohort <- function(data, volume = TRUE, remove.last = TRUE)
{
    table <- Table(value ~ subscriber.from.period + from.period, data = data, FUN = sum)
    names(dimnames(table)) <- c("Start", "Year")
    if (remove.last)
        table <- table[-nrow(table), -ncol(table)]
    attr(table, "date.format") <- switch(attr(data, "subscription.length"),
                                         "year" = "%Y", "%b %Y")
    class(table) <- c("RevenueByCohort", class(table))
    table
}


#' @export
plot.RevenueByCohort <- function(x, ...)
{
    k <- nrow(x)
    Area(t(x), type = "Stacked Area",
          y.title = "Revenue",
          x.tick.format = attr(table, "date.format"),
          colors = col_numeric("Blues", domain = NULL)(1:(k + 3))[-1:-3],
          legend.ascending = FALSE)$htmlwidget
}