#' #' \code{Expansion}
#' #'
#' #' @description Computes acquisition, by cohort.
#' #' @param data A \code{data.frame} that has the same variables as a \code{RevenueData} object.
#' #' @param by The time unit. E.g., "month".
#' #' @param volume The number of subscribers in terms of their value.
#' #' @details Computed based on being a subscribed on the last second of the time period.
#' #' @return A vector showing number of subscribers over time.
#' #'
#' #' @importFrom lubridate '%within%' seconds floor_date
#' #' @importFrom flipTime Periods
#' #' @export
#' Subscribers <- function(data, by = "month")
#' {
#'     
#'     if (!volume)
#'         data <- data[data$observation == 1, ]
#'     start <- floor_date(min(data$from), unit = by)
#'     n <- interval(start, end) %/% Periods(1, by) + 1
#'     result <- rep(NA, n) + 1
#'     starts <- start + Periods(0:(n-1), by)
#'     names(result) <- starts
#'     tenure.interval <- interval(data$subscriber.from, data$subscriber.to)
#'     count <- 0
#'     for (i in 1:n)
#'     {
#'         result[i] <- if(volume)
#'             {
#'                 start <- starts[i]
#'                 filt <- start >= data$from & start <= data$to
#'                 sum(data$value[start >= data$from & start <= data$to])
#'             }
#'             else
#'             {
#'                 start <- starts[i] + months(1) - seconds(1)
#'                 sum(start %within% tenure.interval)
#'             }
#' 
#'     }
#'     class(result) <- c("Subscribers", class(result))
#'     result
#' }
