#' \code{Subscribers}
#'
#' @description Computes acquisition, by cohort.
#' @param data A \code{data.frame} that has the same variables as a \code{RevenueData} object.
#' @param end The date on and after which which revenue is ignored.
#' Note that the default value, \code{Sys.time()} may not be in the same time zone as your other data, and this
#' can cause unexpected results.
#' @param by The time unit. E.g., "month".
#' @param volume The number of subscribers in terms of their value.
#' @details Computed based on being a subscribed on the last second of the time period.
#' @return A vector showing number of subscribers over time.
#'
#' @importFrom lubridate '%within%' floor_date
#' @importFrom flipTime Periods
#' @export
Subscribers <- function(data, end = Sys.time(),  by = "month", volume = FALSE)
{
    if (!volume)
        data <- data[data$observation == 1, ]
    start <- floor_date(min(data$from), unit = by)
    n <- interval(start, end) %/% Periods(1, by) + 1
    result <- rep(NA, n) + 1
    starts <- start + Periods(0:(n-1), by)
    names(result) <- starts
    count <- 0
    for (i in 1:n)
    {
        start <- starts[i]
        filt <- start >= data$from & start < data$to
        result[i] <- if(volume) sum(data$value[filt]) else sum(filt)
    }
    class(result) <- c("Subscribers", class(result))
    result
}
