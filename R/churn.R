#' \code{Churn}
#'
#' @description Computes retention, by cohort.
#' @param data A \code{data.frame} that has the same variables as a \code{RevenueData} object.
#' @param volume Weights the results by volume.
#' @details Where subscribers suspends their purchasing for a period, but purchases again later, the subscriber
#' is included in the churn. Churn is show for all periods that have data in \code{data$to.period}, even if
#' they occur in the future.
#' @return A \code{\link{list}} containing the following elements:
#'   \item{id}{The \code{id} values of subscribers to churn.}
#'   \item{base}{The number of subscribers to renew or churn in the time period.}
#'   \item{counts}{A table where the first column contains the number of people not
#'   to churn in a period, and the second contains the number that have churned. Where \code{volume}
#'   is \code{TRUE} the data is volume-weighted}.
#'   \item{rates}{The percentage to churn (weighted if the counts are weighted)}.
#' @importFrom flipTime AsDate
#' @export
Churn <- function(data, volume = FALSE)
{

    by <- attr(data, "subscription.length")
    to.period <- AsDate(data$to.period, on.parse.failure = "silent")
    data <- removeIncompleteSubscriptions(data)
    # Retaining only the initial invoice in a period
    idag <- aggregate(id ~ to.period, data = data[data$churn,], FUN = unique)
    id <- idag[, 2]
    names(id) <- idag[, 1]
    counts <- if (volume)
            Table(value ~ churn + to.period, data = data, FUN = sum)
        else
        {
            Table( ~ churn + to.period, data = data[data$observation.within.period == 1,])
            #apply(xt, c(1, 2), max)
        }
    base <- table(data$to.period[data$observation.within.period == 1])
    rates <- prop.table(counts, 2)[2, ]
    result <-list(id = id, base = base, counts = t(counts), rates = rates, by = by)
    class(result) <- c("Churn", class(result))
    result
}
