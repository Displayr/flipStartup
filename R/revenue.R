#' \code{Revenue}
#'
#' @description Computes acquisition, by cohort.
#' @param data A \code{data.frame} that has the same variables as a \code{RevenueData} object.
#' @param end The time past which revenue is ignored (where its \code{from} data is greater than this value.
#' Note that the default value, \code{Sys.time()} may not be in the same time zone as your other data, and this
#' can cause unexpected results.
#' @param by The time unit. E.g., "month".
#' @details Computed based on being a subscribed on the last second of the time period.
#' @return A vector showing number of subscribers over time.
#'
#' @importFrom lubridate '%within%' seconds
#' @export
Revenue <- function(data, end = Sys.time(), by = "month")
{
    result <- Subscribers(data, by = "month", end = end, volume = TRUE)
    class(result) <- c("Revenue", class(result))
    result
}

