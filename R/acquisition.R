#' \code{Acquisition}
#'
#' @description Computes acquisition, by cohort.
#' @param data A \code{data.frame} that has the same variables as a \code{RevenueData} object.
#' @param remove.last Remove the final period (as usually is incomplete).
#' @param volume Weights the results by volume.
#' @details Where subscribers suspends their purchasing for a period, but purchases again later, the subscriber
#' is asumed to have been retained during the period where the account was suspended.
#' @return A \code{\link{list}} containing the following elements:
#'   \item{id}{The \code{id} values of subscribers to churn.}
#'   \item{base}{The number of subscribers to renew or churn in the time period.}
#'   \item{counts}{Number of acquisitions by period (or in $ if \code{volume} is \code{TRUE}}.
#'   \item{rates}{The percentage to churn (weighted if the counts are weighted)}.
#'
#' @importFrom flipStatistics Table
#' @export
Acquisition <- function(data, remove.last = TRUE, volume = FALSE)
{
    #filtering out data where there are no start dates.
    # data <- data[data$to <= max(data$from), ]
     data <- data[data$observation == 1,]
    if (remove.last)
        data <- data[data$to.period < max(data$to.period), ]
    idag <- aggregate(id ~ start.period, data = data, FUN = unique)
    id <- idag[, 2]
    names(id) <- idag[, 1]
    counts <- if (volume)
        Table(value ~ start.period, data = data, FUN = sum)
    else
        Table( ~ start.period, data = data)
    result <-list(volume = volume, id = id, counts = counts)
    class(result) <- c("Acquisition", class(result))
    result
}

#' @importFrom plotly plot_ly
#' @export
plot.Acquisition <- function(x, ...)
{
    rates <- x$counts
    period.names <- names(rates)
    title <- if(x$volume) "Acquisition (% volume)" else "Acquisition (subscribers)"
    TimeSeriesColumnChart(rates, smooth = TRUE, series.name = "Acquisition", ytitle = title,  ...)
}

