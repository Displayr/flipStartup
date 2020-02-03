#' \code{Customers}
#'
#' @description The number of customers.
#' @param data A \code{data.frame} that has the same variables as a \code{RevenueData} object.
#' @param by The time period to aggregate the dates by: 
#' \code{"year"}, \code{"quarter"}, \code{"month"}, \code{"week"}, 
#' and \code{"day"}.
#' @return A named vector showing the number of customers.
#' @importFrom flipTime AsDate Period
#' @export
Customers <- function(data, volume = FALSE, by = "quarter", error.if.no.data = FALSE)
{
    from <- AsDate(Period(attr(data, "start"), by))
    end <- AsDate(Period(attr(data, "end"), by))
    dts <- seq.Date(from, end, by = by)
    m <- matrix(dts, nrow(data), length(dts), byrow = TRUE)
    m <- sweep(m, 1, data$from, ">=") & sweep(m, 1, data$to, "<")  
    out <- apply(m, 2, function(x) nUnique(data$id[x]))
    #out <- colSums(m)
    names(out) <- Period(dts, by)
    detail <- data[data$observation == 0, 
                   c("id", "subscriber.from.period", "subscriber.to.period")]
    addAttributesAndClass(out, "Customers", by, detail)
}


#' @export
plot.Customers <- function(x, ...)
{
    columnChart(x, y.title = "Customers", ...)
}
