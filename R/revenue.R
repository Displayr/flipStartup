#' \code{Revenue}
#'
#' @description Computes acquisition, by cohort.
#' @param data A \code{data.frame} that has the same variables as a \code{RevenueData} object.
#' @param by The time unit. E.g., "month".
#' @details Computed based on being a subscribed on the last second of the time period.
#' @return A vector showing number of subscribers over time.
#'
#' @importFrom lubridate '%within%' seconds
#' @export
Revenue <- function(data, by = "month")
{
    Subscribers(data, by = "month", volume = TRUE)
}

