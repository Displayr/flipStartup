#' \code{Retention}
#'
#' @description Computes retention, by cohort.
#' @param data A \code{data.frame} that has the same variables as a \code{RevenueData} object.
#' @param by The time unit to plot. E.g., "month".
#' @param ... Other arguments.
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
#' @export
Retention <- function(data, by, ...)
{
    final.period <- Period(attr(data, "end"), by = by)
    data <- removeIncompleteSubscriptions(data)
    if (nrow(data) == 0)
        return(NULL)
    data.id <- data[data$observation == 1, ]
    #data.id$final.end <- as_date(data.id$subscriber.to)
    #last.period <- as_date(max(AsDate(data.id$subscriber.from.period,
    #                                  on.parse.failure = "silent")))
    #data.id$final.end[data.id$final.end > last.period] <- last.period
    data.id$final.end <- Period(data.id$final.end, by)
    data$from.renewal.period <- Period(data$from.renewal, by)
    data$to.renewal.period <- Period(data$to.renewal, by)
    data$subscriber.from.period <- Period(data$subscriber.from, by)
    
    #max.from <- max(AsDate(data$from.renewal.period, on.parse.failure = "silent"))
#    final.period <- Period(max.from + Periods(1, by), by = by)
    period.names <- unique(c(unique(data$from.renewal.period), final.period))
    periods <- CompleteListPeriodNames(period.names, by)
    n.periods <- length(periods)
    retention.rate.volume <-
        matrix(NA, n.periods, n.periods, dimnames = list(subscriber.from.period = periods, period = periods))
    names(dimnames(retention.rate.volume)) <- c("Commenced", by)
    n.subscriptions <- n.retained <- retention.rate <- retention.rate.volume
    total <- 0
    total.lost <- 0
    total.by.period <- rep(0, n.periods)
    names(total.by.period) <- periods
    loss.by.period <- total.by.period
    detail <- data.frame(Cohort = rep(periods, n.periods),
                         "Period Ending" = rep(periods, rep(n.periods, n.periods)),
                         ID = "",
                         stringsAsFactors = FALSE)
        # Computing the volumetric retention rate
    # Should replace with better formulas, such as: Table(id~subscriber.from.period + to.renewal.period, data = data, FUN = function(x) length(unique(x)))
    
    for (cohort in 1:(n.periods - 1)) # Looping through cohorts
    {
        start.period <- periods[cohort]
        starters <- data$subscriber.from.period == start.period
        for (c in cohort:n.periods) # Looping through periods in cohort
        {
            period <- periods[c]
            
            base <- starters & data$to.renewal.period == period
            revenue <- data$value[base]
            churn <- data$churn[base]
            ids <- data$id[base]
            if (length(churn) > 0 & sum(churn) > 0)
                detail$ID[(cohort - 1) * n.periods + c] <- paste(ids[churn], collapse = ", ")
            revenue.base <- sum(revenue, na.rm = TRUE)
            revenue.lost <- sum(revenue[churn], na.rm = TRUE)
            n.subscribers <- length(unique(ids))
            if (n.subscribers > 0)
            {
                n.churned <- length(unique(ids[churn]))
                # if (period == "2010-01")
                # {
                #         z = sort(unique(ids))
                #         print(as.matrix(z))
                #     
                # }
                # if (period == 2010)#& cohort == n.periods - 1)
                # {
                #     #     #stop(data$id[base])
                #     #     
                # }
                n.subscriptions[cohort, c] <- n.subscribers
                n.retained[cohort, c] <- retained <- n.subscribers - n.churned
                retention.rate[cohort, c] <- retained / n.subscribers
                retention.rate.volume[cohort, c] <- (revenue.base - revenue.lost) / revenue.base
                total <- total + revenue.base
                total.lost <- total.lost + revenue.lost
                loss.by.period[c] <- revenue.lost + loss.by.period[cohort]
                total.by.period[c] <- revenue.base + total.by.period[cohort]
            }
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
    list(detail = detail, 
         n.subscriptions = n.subscriptions,
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


