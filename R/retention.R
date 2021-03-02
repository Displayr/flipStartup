#' \code{Retention}
#'
#' @description Computes retention, by cohort.
#' @param data A \code{data.frame} that has the same variables as a \code{RevenueData} object.
#' @details Where subscribers suspends their purchasing for a period, but purchases again later, the subscriber
#' is asumed to have been retained during the period where the account was suspended. Where
#' a subscriber for some reason had a subscription length that was different to the specified.
#' @return A \code{\link{list}} containing the following elements:
#'   \item{counts}{The number of subscribers per \code{period} by \code{subscriber.from.period}}
#'   \item{index}{The percentage (proportion * 100) of subscribers to remain subscribers}
#'   \item{retention}{Retention per \code{period} by \code{subscriber.from.period}}
#'   \item{retention.volume}{Same as \code{retention}, except weighted by
#'    the volumne (revenue) in the previous subscription renewal.}
#'   \item{average.retention}{Average subscriber retention.}
#'   \item{churn}{subscriber churn (1 - average.retention).}
#'   \item{average.life.span}{Estimated average subscriber lifespan (1 / churn) subscriber retention.}
#'   \item{average.volume}{Retention, weighted by subscriber value in the preceeding period.}
#'   \item{churn.volume}{Churn, weighted by subscriber value in the preceeding period.}
#' @importFrom flipTime AsDate CompleteListPeriodNames Period Periods
#' @importFrom lubridate as_date
#' @importFrom verbs Sum
#' @export
Retention <- function(data)
{
    subscription.length <- attr(data, "subscription.length")
    data <- removeIncompleteSubscriptions(data)
    data.id <- data[data$observation == 1, ]
    data.id$final.end <- as_date(data.id$subscriber.to)
    last.period <- as_date(max(AsDate(data.id$subscriber.from.period,
                                      on.parse.failure = "silent")))
    data.id$final.end[data.id$final.end > last.period] <- last.period
    data.id$final.end <- Period(data.id$final.end, subscription.length)
    max.from <- max(AsDate(data$from.period, on.parse.failure = "silent"))
    final.period <- Period(max.from + Periods(1, subscription.length), by = subscription.length)
    period.names <- unique(c(unique(data$from.period), final.period))
    periods <- CompleteListPeriodNames(period.names, subscription.length)
    n.periods <- length(periods)
    retention.rate.volume <-
        matrix(NA, n.periods, n.periods, dimnames = list(subscriber.from.period = periods, period = periods))
    names(dimnames(retention.rate.volume)) <- c("Commenced", subscription.length)
    n.subscriptions <- n.retained <- retention.rate <- retention.rate.volume
    total <- 0
    total.lost <- 0
    total.by.period <- rep(0, n.periods)
    names(total.by.period) <- periods
    loss.by.period <- total.by.period
    # Computing the volumetric retention rate
    for (r in 1:(n.periods))
    {
        start.period <- periods[r]
        starters <- data$subscriber.from.period == start.period
        for (c in r:(n.periods))
        {
            period <- periods[c]
            base <- starters & data$from.period == period
            revenue <- data$value[base]
            churn <- data$churn[base]
            ids <- data$id[base]
            revenue.base <- Sum(revenue)
            revenue.lost <- Sum(revenue[churn])
            n.subscriptions[r, c] <- n.subscribers <- length(unique(ids))
            n.retained[r, c] <- retained <-  n.subscribers - length(unique(ids[churn]))
            retention.rate[r, c] <- retained / n.subscribers
            retention.rate.volume[r, c] <- (revenue.base - revenue.lost) / revenue.base
            total <- total + revenue.base
            total.lost <- total.lost + revenue.lost
            loss.by.period[r] <- revenue.lost + loss.by.period[r]
            total.by.period[r] <- revenue.base + total.by.period[r]
        }
    }
    average.retention.rate <- Sum(retention.rate * n.subscriptions) / Sum(n.subscriptions)
    retention.rate.by.period <- apply(retention.rate * n.subscriptions, 2, Sum) / apply(n.subscriptions, 2, Sum)
    retention.rate.volume.by.period <- 1 - loss.by.period / total.by.period
    churn <- 1 - average.retention.rate
    average.retention.rate.volume = (total - total.lost) / total
    churn.volume = 1 - average.retention.rate.volume
    average.lifespan <- 1 / churn
    average.lifespan.volume <- 1 / churn.volume
    churn.first.period <- diag(1 - retention.rate)
    mean.churn.first.period <- mean(churn.first.period, na.rm = TRUE)
    estimated.volume.churn.by.period <- churn.volume * (churn.first.period / mean.churn.first.period)
    list(n.subscriptions = n.subscriptions,
         n.retained = n.retained,
         retention.rate = retention.rate,
         retention.rate.volume = retention.rate.volume,
         average.retention.rate = average.retention.rate,
         average.retention.rate.volume = average.retention.rate.volume,
         retention.rate.by.period = retention.rate.by.period,
         retention.rate.volume.by.period = retention.rate.volume.by.period,
         churn = churn,
         churn.volume = churn.volume,
         churn.first.period = churn.first.period,
         average.lifespan = average.lifespan,
         average.lifespan.volume = average.lifespan.volume)
}


