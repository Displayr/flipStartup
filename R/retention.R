#' \code{Retention}
#'
#' @description Computes retention, by cohort.
#' @param data A \code{data.frame} that has the same variables as a \code{RevenueData} object.
#' @param subscription.length The length of the subscription: \code{"day"},
#' \code{"week"}, \code{"month"}, \code{"quarter"}, or \code{"year"}.
#' @param remove.last Remove the final period (as usually is incomplete).
#' @details Where subscribers suspends their purchasing for a period, but purchases again later, the subscriber
#' is asumed to have been retained during the period where the account was suspended. Where
#' a subscriber for some reason had a subscription length that was different to the specified
#' \code{"subscription.length"}, their revenue-weighted retention will be misattributed.
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
#' @importFrom lubridate as_date
#' @export
Retention <- function(data, subscription.length = "year", remove.last = TRUE)
{
    by <- attr(data, "by")
    data.id <- data[data$observation == 1, ]
    data.id$final.end <- as_date(data.id$subscriber.to)
    last.period <- as_date(max(PeriodNameToDate(data.id$subscriber.from.period, by)))
    data.id$final.end[data.id$final.end > last.period] <- last.period
    data.id$final.end <- Period(data.id$final.end, by)
#    counts <- xtabs(~ subscriber.from.period + final.end, data = data.id)
    #print("a7")
    period.names <- unique(c(unique(data$from.period), unique(data$to.period)))
    #print(period.names)
    periods <- CompleteListPeriodNames(period.names, by)
     #<- Period(dates, by)
    #print("a8")
  #  counts <- FillInMatrix(counts, dates, dates, 0)
    #print("a9")
 #   counts <- counts[, ncol(counts):1]
    #print("b")
   # counts <- t(apply(counts, 1, cumsum))
#    counts <- counts[,ncol(counts):1]
 #   periods <- sort(unique(unlist(dimnames(counts))))
#print("dog")
#print(periods)
    n.periods <- length(periods)
    #counts.1 <- matrix(NA, n.periods, n.periods, dimnames = list(start.period = periods, period = periods))
    #counts.1[match(rownames(counts), periods), match(colnames(counts), periods)] <- counts
    #print("c")
   # counts.1[Triangle(counts.1, "lower left")] <- NA
   # counts <- counts.1
    # Filling in periods where no churn occurred
#     for (r in 1:n.periods)
#         for (c in (n.periods - 1):min(n.periods - 1, r))
#             if (is.na(counts[r,c]) & r <= c)
#                 counts[r, c] <- counts[r, c + 1]
#     if (remove.last){
#         counts <- counts[-nrow(counts), -ncol(counts)]
#         periods <- periods[-n.periods]
#         n.periods <- n.periods - 1
#
#     }
# #print("d")
    # Volume-based retention
    #periods.per.subscription <- round(DaysPerPeriod(subscription.length) / DaysPerPeriod(by), 0)
#print(periods.per.subscription)
    retention.rate.volume <-
        matrix(NA, n.periods, n.periods, dimnames = list(subscriber.from.period = periods, period = periods))
    names(dimnames(retention.rate.volume)) <- c("Commenced", by)
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
            revenue.base <- sum(revenue, na.rm = TRUE)
            revenue.lost <- sum(revenue[churn], na.rm = TRUE)
            n.subscriptions[r, c] <- n.subscribers <- length(unique(ids))
            #n.retained[r, c] <-  sum(base)
            n.retained[r, c] <- retained <-  n.subscribers - length(unique(ids[churn]))
            retention.rate[r, c] <- retained / n.subscribers
#            print(c(r, c, n.subscribers, sum(churn)))
            retention.rate.volume[r, c] <- (revenue.base - revenue.lost) / revenue.base
            total <- total + revenue.base
            total.lost <- total.lost + revenue.lost
            loss.by.period[r] <- revenue.lost + loss.by.period[r]
            total.by.period[r] <- revenue.base + total.by.period[r]
        }
    }
    average.retention.rate <- sum(retention.rate * n.subscriptions, na.rm = TRUE) / sum(n.subscriptions, na.rm = TRUE)
    retention.rate.by.period <- apply(retention.rate * n.subscriptions, 2, sum, na.rm = TRUE) / apply(n.subscriptions, 2, sum, na.rm = TRUE)
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
         #index = IndexDiagonal(n.subscriptions),
         average.retention.rate = average.retention.rate,
         average.retention.rate.volume = average.retention.rate.volume,
         retention.rate.by.period = retention.rate.by.period,
         retention.rate.volume.by.period = retention.rate.volume.by.period,
         #estimated.volume.retention.by.period = estimated.volume.retention.by.period,
         #average.retention = average.retention,
         churn = churn,
         churn.volume = churn.volume,
         churn.first.period = churn.first.period,
         average.lifespan = average.lifespan,
         average.lifespan.volume = average.lifespan.volume)
         #average.volume = average.volume,
    #    $ $)
}


