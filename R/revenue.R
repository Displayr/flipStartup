#' \code{Revenue}
#'
#' @description Computes total amount of license revenue at points in time.
#' @param data A \code{data.frame} that has the same variables as a \code{RevenueData} object.
#' @param end The time past which revenue is ignored (where its \code{from} data is greater than this value.
#' Note that the default value, \code{Sys.time()} may not be in the same time zone as your other data, and this
#' can cause unexpected results.
#' @param by The time unit for reporting the data. E.g., "month".
#' @details Computed based on being a subscribed on the last second of the time period.
#' @return A vector showing the revenue at specific points in time.
#'
#' @export
Revenue <- function(data, end = Sys.time(), by)
{
    result <- Subscribers(data, by = by, end = end, volume = TRUE, recurring = FALSE)
#    class(result) <- c("Revenue", class(result))
    result
}

