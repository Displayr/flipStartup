#' \code{Customers}
#'
#' @description The number of customers.
#' @param data A \code{data.frame} that has the same variables as a \code{RevenueData} object.
#' @param by The time unit to plot. E.g., "month".
#' @return A named vector showing the number of customers.
#' @importFrom flipTime AsDate Period
#' @export
Customers <- function(data, volume = FALSE, by = "quarter", error.if.no.data = FALSE)
{
    data$from.period <- Period(data$from.renewal, by)
    x <- tapply(data$id, list(data$from.period), FUN = unique)
    n.per.period <- sapply(x, length)
    detail <- data.frame(rep(names(x), n.per.period),
                         unlist(x))
    colnames(detail) <- c(properCase(by), "Name")
    addAttributesAndClass(n.per.period, "Customers", by, detail)
}


#' @export
plot.Customers <- function(x, ...)
{
    columnChart(x, y.title = "Customers", ...)
}
