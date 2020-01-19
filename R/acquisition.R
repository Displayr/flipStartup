#' \code{Acquisition}
#'
#' @description Computes acquisition, by cohort.
#' @param data A \code{data.frame} that has the same variables as a \code{RevenueData} object.
#' @param subset An optional vector specifying a subset of observations to be used in the fitting process.#' @details Where subscribers suspends their purchasing for a period, but purchases again later, the subscriber
#' @param remove.last Remove the final period (as usually is incomplete).
#' @param volume Weights the results by volume.
#' @param number.periods The number of periods of data that are treated as being 'acquisitions'. By default, this is 1, 
#' which means that only sales in the initial period are counted as acquisitions. If set to 12, for example, it would
#' means that the first 12 periods are treated as acquisitions (e.g., the first year, if the data has been setup as monthly).
#' is assumed to have been retained during the period where the account was suspended.
#' @return A \code{\link{list}} containing the following elements:
#'   \item{id}{The \code{id} values of subscribers to churn.}
#'   \item{base}{The number of subscribers to renew or churn in the time period.}
#'   \item{counts}{Number of acquisitions by period (or in $ if \code{volume} is \code{TRUE}}.
#'   \item{rates}{The percentage to churn (weighted if the counts are weighted)}.
#'
#' @importFrom flipStatistics Table
#' @export
Acquisition <- function(data, subset, remove.last = FALSE, volume = FALSE, number.periods = 1)
{
    if (!missing(subset))
        data <- subset(data, subset)
    if(remove.last)
        data <- removeLast(data)
    data <- data[data$observation <= number.periods, ]
    x <- quantityByTime(data, volume, "subscriber.from.period")
    attr(x, "volume") <- volume
    id <- idByPeriod(data, "subscriber.from.period")
    attr(x, "detail") <- sapply(id, paste, collapse = ", ")
    attr(x, "subscription.length") <- attr(data, "subscription.length")
    class(x) <- c("Acquisition", class(x))
    x
}

idByPeriod <- function(data, time)
{
    data$time <- data[, time]
    if (nrow(data) == 0)
        return(NULL)
    idag <- aggregate(id ~ time, data = data, FUN = unique)
    id <- idag[, 2]
    names(id) <- idag[, 1]
    id
}   

quantityByTime <- function(data, volume, time)
{
    if (volume)
        Table(value ~ subscriber.from.period, data = data, FUN = sum)
    else
        Table( ~ subscriber.from.period, data = data)
}

#' @export
YLim.Acquisition <- function(x, ...)
{
    range(x)
}

#' @export
plot.Acquisition <- function(x, ...)
{
    y.title <- if(attr(x, "volume")) "New revenue" else "New subscribers" 
    columnChart(x, y.title = y.title, ...)
}
