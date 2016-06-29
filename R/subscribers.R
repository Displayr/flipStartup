#' \code{Subscribers}
#'
#' @description Computes acquisition, by cohort.
#' @param data A \code{data.frame} that has the same variables as a \code{RevenueData} object.
#' @param by The time unit. E.g., "month".
#' @param volume The number of subscribers in terms of their value.
#' @details Computed based on being a subscribed on the last second of the time period.
#' @return A vector showing number of subscribers over time.
#'
#' @importFrom lubridate '%within%' seconds floor_date
#' @export
Subscribers <- function(data, by = "month", volume = FALSE)
{
    if (by != "month")
        stop("This function only works for 'by' of month.")
    if (!volume)
        data <- data[data$observation == 1, ]
    start <- min(data$from)
    end <- min(max(data$from), floor_date(Sys.time(), by))
    n <- interval(start, end) %/% months(1) + 1
    result <- rep(NA, n)
    starts <- start + months(0:(n-1))
    names(result) <- starts
    for (i in 1:n)
    {
        result[i] <- if(volume)
            {
                start <- starts[i]
                sum(data$value[start >= data$from & start < data$to])
            }
            else
            {
                start <- starts[i] + months(1) - seconds(1)
                sum(start %within% data$tenure.interval)
            }

    }
    class(result) <- c("Subscribers", class(result))
    result
}


#' @importFrom plotly plot_ly
#' @export
plot.Subscribers <- function(x, ...)
{
    title <- if("Revenue" %in% class(x)) "Revenue" else "Subscribers"
    TimeSeriesColumnChart(x, smooth = FALSE, series.name = title, ytitle = title,  ...)
}
