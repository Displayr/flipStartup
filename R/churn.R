#' \code{Churn}
#'
#' @description Computes retention, by cohort.
#' @param data A \code{data.frame} that has the same variables as a \code{RevenueData} object.
#' @param volume Weights the results by volume.
#' @param error.if.no.data If TRUE and the data contains no valid cases, an error is thrown.
#' @details Where subscribers suspends their purchasing for a period, 
#' but purchases again later, the subscriber
#' is included in the churn. Churn is show for all periods that have 
#' data in \code{data$to.period}, even if they occur in the future.
#' @return A named vector showing churn.
#' @importFrom flipTime AsDate
#' @export
Churn <- function(data, volume = FALSE, error.if.no.data = FALSE)
{
    by <- attr(data, "subscription.length")
    to.period <- AsDate(data$to.period, on.parse.failure = "silent")
    data <- removeIncompleteSubscriptions(data)
    if (nrow(data) == 0)
    {
        if (error.if.no.data)
            stop("No subscriptions have had an opportunity to churn.")
        return(NULL)
    }
    # Retaining only the initial invoice in a period
    dat <- data[data$churn,, drop = FALSE]
    
    counts <- churnCountsByTime(data, volume)
    
    #base <- table(data$to.period[data$observation.within.period == 1])
    out <-  prop.table(counts, 2)[2, ] 
    #result <-list(id = id, base = base, counts = t(counts), rates = rates, by = by, volume = volume)
    class(out) <- c("Churn", class(out))
    id <- idByPeriod(dat, time = "to.period")    
    attr(out, "detail") <- sapply(id, paste, collapse = ",")
    attr(out, "volume") <- volume
    out
}

#' @export
plot.Churn <- function(x, ...)
{
    y.title <- if (attr(x, "volume")) "Churn rate ($)" else "Churn rate (customers)"
    columnChart(x, y.title = y.title, y.tick.format = "%")
}

churnCountsByTime <- function(data, volume)
{
    counts <- if (volume) Table(value ~ churn + to.period, data = data, FUN = sum)
    else Table( ~ churn + to.period, data = data[data$observation.within.period == 1,])
    if (nrow(counts) == 1)
    {
        counts <-if (rownames(counts) == "TRUE")
            rbind(counts, "FALSE" = 0)[2:1, , drop = FALSE]
        else
            rbind(counts, "TRUE" = 0)
    }
    counts    
}

