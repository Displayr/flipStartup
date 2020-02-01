#' \code{Churn}
#'
#' @description Computes retention, by cohort.
#' @param data A \code{data.frame} that has the same variables as a \code{RevenueData} object.
#' @param volume Weights the results by volume.
#' @param by The time unit to plot. E.g., "month".
#' @param error.if.no.data If TRUE and the data contains no valid cases, an error is thrown.
#' @details Where subscribers suspends their purchasing for a period, 
#' but purchases again later, the subscriber
#' is included in the churn. Churn is show for all periods that have 
#' data in \code{data$to.period}, even if they occur in the future.
#' @return A named vector showing churn.
#' @importFrom flipTime AsDate
#' @export
Churn <- function(data, volume = FALSE, by = "quarter", error.if.no.data = FALSE)
{
    to.period <- AsDate(data$to.period, on.parse.failure = "silent")
    data <- removeIncompleteSubscriptions(data)
    if (nrow(data) == 0)
    {
        if (error.if.no.data)
            stop("No subscriptions have had an opportunity to churn.")
        return(NULL)
    }
    counts <- churnCountsByTime(data, volume)
#    print(rbind(counts, colSums(counts)))
    out <-  prop.table(counts, 2)[2, ] 
    class(out) <- c("Churn", class(out))
    dat <- data[data$churn,, drop = FALSE]
    id <- idByPeriod(dat, time = "to.renewal.period")    
    attr(out, "detail") <- sapply(id, paste, collapse = ",")
    attr(out, "volume") <- volume
    attr(out, "by") <- by
    out
}

#' \code{CustomerChurn}
#' @param data A \code{data.frame} that has the same variables as a \code{RevenueData} object.
#' @param ... Other parameters
#' @return A named vector showing churn.
#' @importFrom flipTime AsDate
#' @export
CustomerChurn <- function(data, by = "quarter", ...)
{
    Churn(data, volume = FALSE, by = by, error.if.no.data = FALSE)
}



#' \code{RecurringRevenueChurn}
#' @param data A \code{data.frame} that has the same variables as a \code{RevenueData} object.
#' @param ... Other parameters
#' @return A named vector showing churn.
#' @importFrom flipTime AsDate
#' @export
RecurringRevenueChurn <- function(data, by = "quarter", ...)
{
    Churn(data, volume = TRUE, by = by, error.if.no.data = FALSE)
}


#' @export
print.Churn <- function(x, ...)
{
    attr(x, "detail") <- NULL
    attr(x, "volume") <- NULL
    class(x) <- class(x)[-1]
    print(x)
}

#' @export
plot.Churn <- function(x, ...)
{
    y.title <- if (attr(x, "volume")) "Churn rate ($)" else "Churn rate (customers)"
    columnChart(x, y.title = y.title, y.tick.format = "%", ...)
}

churnCountsByTime <- function(data, volume)
{
#    data <- data[data$observation.within.period == 1,]
#    z = sort(data$id[data$to.renewal.period == 2010])
#    print(as.matrix(z))
    counts <- if (volume) Table(value ~ churn + to.renewal.period, data = data, FUN = sum)
    else Table(id ~ churn + to.renewal.period, data = data, FUN = function(x) length(unique(x)))
    if (nrow(counts) == 1)
    {
        counts <-if (rownames(counts) == "TRUE")
            rbind(counts, "FALSE" = 0)[2:1, , drop = FALSE]
        else
            rbind(counts, "TRUE" = 0)
    }
    counts    
}

