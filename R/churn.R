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
#' @importFrom flipTime AsDate Period
#' @export
Churn <- function(data, volume = FALSE, by = "quarter", error.if.no.data = FALSE)
{
    updatePeriods(data, by)
    if (nrow(data) == 0)
    {
        if (error.if.no.data)
            stop("No subscriptions have had an opportunity to churn.")
        return(NULL)
    }
    counts <- churnCountsByTime(data, volume)
    out <-  prop.table(counts, 2)[2, ]
    if (length(out) == 1) # Dealing with scalars losing names
        names(out) <- colnames(counts)
    dat <- data[data$churn,, drop = FALSE]
    detail <- idByPeriod(dat, time = "to.renewal.period")#sapply(id, paste, collapse = ",")
    out <- addAttributesAndClass(out, "Churn", by, detail)
    attr(out, "volume") <- volume
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


#' #' @export
#' print.Churn <- function(x, ...)
#' {
#'     printWithoutAttributes(x)
#' }

#' @export
plot.Churn <- function(x, ...)
{
    y.title <- if (attr(x, "volume")) "Recurring Revenue Churn Rate" else "Customer Churn Rate"
    columnChart(x, y.title = y.title, y.tick.format = "%", ...)
}

churnCountsByTime <- function(data, volume)
{
    counts <- if (volume) {
        v <- Table(recurring.value ~ churn + to.renewal.period, data = data, FUN = sum)
        v[round(v, 7) == 0] <- 0 # Dealing with numeric precision issue that can lead to negative churn
        v
    }
    else Table(id ~ churn + to.renewal.period, data = data, FUN = nUnique)
    if (nrow(counts) == 1)
    {
        counts <-if (rownames(counts) == "TRUE")
            rbind(counts, "FALSE" = 0)[2:1, , drop = FALSE]
        else
            rbind(counts, "TRUE" = 0)
    }
#    print(counts)
    counts
}

