#' \code{Churn}
#'
#' @description Computes retention, by cohort.
#' @param data A \code{data.frame} that has the same variables as a \code{RevenueData} object.
#' @param remove.last Remove the final period (as usually is incomplete).
#' @param volume Weights the results by volume.
#' @details Where subscribers suspends their purchasing for a period, but purchases again later, the subscriber
#' is asumed to have been retained during the period where the account was suspended.
#' @return A \code{\link{list}} containing the following elements:
#'   \item{id}{The \code{id} values of subscribers to churn.}
#'   \item{base}{The number of subscribers to renew or churn in the time period.}
#'   \item{counts}{A table where the first column contains the number of people not
#'   to churn in a period, and the second contains the number that have churned. Where \code{volume}
#'   is \code{TRUE} the data is volume-weighted}.
#'   \item{rates}{The percentage to churn (weighted if the counts are weighted)}.
#' @importFrom flipTime PeriodNameToDate
#' @export
Churn <- function(data, remove.last = TRUE, volume = FALSE)
{

    by <- attr(data, "by")
    data <- data[data$to <= max(data$from), ] #filtering out data post final period in data
    to.period <- PeriodNameToDate(data$to.period, by)
    if (remove.last)
        data <- data[to.period < max(to.period), ]
    idag <- aggregate(id ~ subscriber.to.period, data = data[data$churned,], FUN = unique)
    id <- idag[, 2]
    names(id) <- idag[, 1]
    counts <- if (volume)
        Table(value ~ churn + to.period, data = data, FUN = sum)
    else
        Table( ~ churn + to.period, data = data)
    base <- table(data$to.period)
    rates <- prop.table(counts, 2)[2, ]
    result <-list(id = id, base = base, counts = t(counts), rates = rates, by = by)
    class(result) <- c("Churn", class(result))
    result
}
