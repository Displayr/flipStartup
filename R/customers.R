#' \code{Customers}
#'
#' @description The number of customers.
#' @param data A \code{data.frame} that has the same variables as a \code{RevenueData} object.
#' @param by The time period to aggregate the dates by: 
#' \code{"year"}, \code{"quarter"}, \code{"month"}, \code{"week"}, 
#' and \code{"day"}.
#' @param ... Additional arguments to be passed to lower level functions.
#' @return A named vector showing the number of customers.
#' @importFrom flipTime AsDate Period
#' @importFrom lubridate floor_date
#' @export
Customers <- function(data, by = "quarter", ...)
{
    
    from <- floor_date(AsDate(attr(data, "start")), unit = by)
    end <- floor_date(AsDate(attr(data, "end")), unit = by)
    dts <- seq.Date(from, end, by = by)
    m <- matrix(dts, nrow(data), length(dts), byrow = TRUE)
    m <- sweep(m, 1, as.numeric(as.Date(data$from)), ">=") & sweep(m, 1, as.numeric(as.Date(data$to)), "<")  
    out <- apply(m, 2, function(x) nUnique(data$id[x]))
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
