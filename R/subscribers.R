#' \code{Subscribers}
#'
#' @description Computes acquisition, by cohort.
#' @param data A \code{data.frame} that has the same variables as a \code{RevenueData} object.
#' @param end The date on and after which which revenue is ignored.
#' Note that the default value, \code{Sys.time()} may not be in the same time zone as your other data, and this
#' can cause unexpected results.
#' @param by The time unit. E.g., "month".
#' @param volume The number of subscribers in terms of their value.
#' @param recurring If TRUE, and \code{volume} is also TRUE, computes the recurring revenue.
#' @details Computed based on being a subscribed on the last second of the time period.
#' @return A vector showing number of subscribers over time.
#'
#' @importFrom lubridate '%within%' floor_date ceiling_date
#' @importFrom flipTime Periods Period
#' @export
Subscribers <- function(data, end = Sys.time(),  by = "month", volume = FALSE, recurring = FALSE)
{
    end <- ceiling_date(end, unit = by)
    start <- floor_date(min(data$from), unit = by)
    n <- interval(start, end) %/% Periods(1, by) + 1
    result <- rep(NA, n) + 1
    starts <- start + Periods(0:(n-1), by)
    names(result) <- Period(starts, by)
    count <- 0
    value = if (recurring) data$recurring.value else data$value
    for (i in 1:n)
    {
        start <- starts[i]
        filt <- start >= data$from & start < data$to
        result[i] <- if(volume) sum(value[filt]) else length(unique(data$id[filt]))
    }
    #attr(result, "by") = by
    result
}
