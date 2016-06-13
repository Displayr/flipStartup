#' \code{Retention}
#'
#' @description Computes retention, by cohort.
#' @param data A \code{data.frame} that has the same variables as a \code{RevenueData} object.
#' @param remove.last Remove the final period (as usually is incomplete).
#' @details Where subscribers suspends their purchasing for a period, but purchases again later, the subscriber
#' is asumed to have been retained during the period where the account was suspended.
#' @return A \code{\link{list}} containing the following elements:
#'   \item{counts}{The number of subscribers per \code{period} by \code{start.period}}
#'   \item{index}{The percentage (proportion * 100) of subscribers to remain subscribers}
#'   \item{retention}{Retention by per \code{period} by \code{start.period}}
#'   \item{average}{Average subscriber retention.}
#'   \item{churn}{subscriber churn (1 - retention).}
#'   \item{average.life.span}{Estimated average subscriber lifespan (1 / churn) subscriber retention.}
#'   \item{average.volume}{Retention, weighted by subscriber value in the preceeding year.}
#'   \item{churn.volume}{Churn, weighted by subscriber value in the preceeding year.}
#'
#' @export
Retention <- function(data, remove.last = TRUE)
{
    data.id <- data[data$observation == 1, ]
    #data.id$

    counts <- xtabs(~ start.period + last.from.period, data = data.id)
    counts <- counts[, ncol(counts):1]
    counts <- t(apply(counts, 1, cumsum))
    counts <- counts[,ncol(counts):1]
    periods <- sort(unique(unlist(dimnames(counts))))
    k <- length(periods)
    counts.1 <- matrix(NA, k, k, dimnames = list(start.period = periods, period = periods))
    counts.1[match(rownames(counts), periods), match(colnames(counts), periods)] <- counts
    counts.1[Triangle(counts.1, "lower left")] <- NA
    counts <- counts.1
    # Filling in periods where no churn occurred
    for (r in 1:k)
        for (c in (k - 1):min(k - 1, r))
            if (is.na(counts[r,c]))
                counts[r, c] <- counts[r, c + 1]
    if (remove.last){
        counts <- counts[-nrow(counts), -ncol(counts)]
        periods <- periods[-k]
        k <- k - 1

    }
    # Volume-based retention

    volume <- matrix(NA, k, k, dimnames = list(start.period = periods, period = periods))
    total <- 0
    total.lost <- 0
    for (r in 1:(k - 1))
        for (c in (r + 1):k)
        {
            start.period <- periods[r]
            period <- periods[c]
            previous.period <- periods[c - 1]
            base <- data$start.period == start.period & data$period == previous.period
            #id.base <- unique(data$id[base])
            retained <- data$start.period == start.period & data$period == period
            id.retained <- unique(data$id[retained])
            revenue.base <- sum(data$value[base], na.rm = TRUE)
            revenue.lost <- sum(data$value[base & !(data$id %in% id.retained)], na.rm = TRUE)
            volume[r, c] <- (revenue.base - revenue.lost) / revenue.base
            total <- total + revenue.base
            total.lost <- total.lost + revenue.lost
        }
    # Computing overall statistics.
    #k <- ncol(counts)
    retention <- counts[, -1] / counts[, -k]
    average <- sum(retention * counts[, -k], na.rm = TRUE) / sum(counts[, -k], na.rm = TRUE)
    churn <- 1 - average
    average.volume = (total - total.lost) / total
    churn.volume = 1 - average.volume
    average.lifespan <- 1 / churn
    list(counts = counts,
         index = IndexDiagonal(counts),
         volume = volume,
         retention = retention,
         average = average,
         churn = churn,
         average.life.span =average.lifespan,
         average.volume = average.volume,
         churn.volume = churn.volume)
}


