#' \code{Revenue}
#'
#' @description Computes total amount of license revenue at points in time.
#' @param data A \code{data.frame} that has the same variables as a \code{RevenueData} object.
#' @param end The time past which revenue is ignored (where its \code{from} data is greater than this value.
#' Note that the default value, \code{Sys.time()} may not be in the same time zone as your other data, and this
#' can cause unexpected results.
#' @param by The time period to aggregate the dates by: 
#' \code{"year"}, \code{"quarter"}, \code{"month"}, \code{"week"}, 
#' and \code{"day"}.
#' @param ... Additional arguments to be passed to lower level functions.
#' @details Computed based on being a subscribed on the last second of the time period.
#' @return A vector showing the revenue at specific points in time.
#'
#' @export
Revenue <- function(data, end = Sys.time(), by, ...)
{
    out <- Subscribers(data, by = by, end = end, volume = TRUE, recurring = FALSE)
    detail <- data[c("id", "value", "from", "to")]
    addAttributesAndClass(out, "Revenue", by, detail)
}

#' @export
plot.Revenue <- function(x, ...)
{
    areaChart(x, y.title = "Revenue", ...)
}
