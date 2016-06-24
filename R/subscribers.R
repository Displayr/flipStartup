#' \code{Subscribers}
#'
#' @description Computes acquisition, by cohort.
#' @param data A \code{data.frame} that has the same variables as a \code{RevenueData} object.
#' @param by The time unit. E.g., "month".
#' @details Computed based on being a subscribed on the last second of the time period.
#' @return A vector showing number of subscribers over time.
#'
#' @importFrom lubridate '%within%' seconds
#' @export
Subscribers <- function(data, by = "month")
{
    if (by != "month")
        stop("This function only works for 'by' of month.")
    data <- data[data$observation == 1, ]
    start <- min(data$from)
    end <- max(data$from)
    n <- interval(start, end) %/% months(1)
    result <- rep(NA, n)
    starts <- start + months(0:(n-1))
    names(result) <- starts
    for (i in 1:n)
    {
        start <- starts[i] + months(1) - seconds(1)
        result[i] <- sum(start %within% data$tenure.interval)
    }
    class(result) <- c("Subscribers", class(result))
    result
}


#' @importFrom plotly plot_ly
#' @export
plot.Subscribers <- function(x, ...)
{
    title <- "Subscribers"
    TimeSeriesColumnChart(x, smooth = FALSE, series.name = "Subscribers", ytitle = "Subscribers",  ...)
}
