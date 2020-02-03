#' \code{InitialChurn}
#'
#' @description Computes the churn rate in the initial time period
#' @param churn.cohort A \code{mattrix} showing churn by cohort.
#' @param remove.last Remove the final period (as usually is incomplete).
#' @param volume Weights the results by volume. Does nothing in this case.
#' @param by The time period to aggregate the dates by: 
#' \code{"year"}, \code{"quarter"}, \code{"month"}, \code{"week"}, 
#' and \code{"day"}.
#' @param ... Other arguments.
#' @return A \code{\link{matrix}}
#' @export
InitialChurn <- function(churn.cohort, remove.last = TRUE, volume = FALSE, by, ...)
{
    # if (remove.last)
    #     x.matrix <- x.matrix[, -ncol(x.matrix)]
    # k <- nrow(churn.cohort)
    # 
    # 
    # 
    churn <- selectChurnData(churn.cohort, attr(churn.cohort, "subscription.length"), by)#[diag(k)[, k:1] == 1]
    #names(churn) <- colnames(churn.cohort)[-1]
    detail <- attr(churn.cohort, "detail")
    detail <- detail[detail[, 2] == 1, -2] # Filtering for people in their first period
    churn <- addAttributesAndClass(churn, "InitialChurn", by, detail)
    class(churn) <- c("InitialChurn", class(churn))
    attr(churn, "subscription.length") <- attr(data, "subscription.length")
    #attr(churn, "n.subscriptions") <- attr(data, "n.subscriptions")
    attr(churn, "volume") <- volume
    churn
}

#' @importFrom flipTime AsDate Periods 
selectChurnData <- function(x, subscription.length, by)
{
    n.subscribers <- attr(x, "n.subscribers")
    row.names <- rownames(x)
    row.dates <- AsDate(row.names)
    col.dates <- AsDate(colnames(x))
    initial.possible.renewal <- row.dates + Periods(1, subscription.length) 
    m <- match(initial.possible.renewal, col.dates)
    k <- length(row.dates)
    churn <- rep(NA, k)
    base <- rep(0, k)
    names(base) <- names(churn) <- Period(initial.possible.renewal, by)
    not.missing <-!is.na(m) 
    lookups <- cbind(1:k, m)[not.missing, ]
    churn[not.missing] <- x[lookups]
    base[not.missing] <- n.subscribers[lookups]
    attr(churn, "n.subscribers") <- base
    churn
}

#' InitialCustomerChurn
#' 
#' @description Computes the churn rate for people in their first completed
#' renewal period.
#' @param data A \code{data.frame} that has the same variables as a \code{RevenueData} object.
#' @param by The time period to aggregate the dates by: 
#' \code{"year"}, \code{"quarter"}, \code{"month"}, \code{"week"}, 
#' and \code{"day"}.
#' @param ... Other arguments.
#' @details For example, if the subscriptions are annual, computes 
#' proportion of people in 2012 that churned in 2013. Customers that had
#' an incomplete year that falls within a calendar year will be excluded
#' from this metric (e.g., a license from 1 January to 1 December 2012  will
#' not appear in this calculation, but their renewal year may instead). 
#' This calculation is taken from the one-off diagonal of 
#' \code{CustomerChurnByCohort}.
#' @export
InitialCustomerChurn <- function(data, by, ...)
{
    churn.cohort <- CustomerChurnByCohort(data, TRUE, by)
    InitialChurn(churn.cohort, remove.last = TRUE, by = by)
}

#' InitialRecurringRevenueChurn
#' 
#' @description Computes the recurrent revenue churn rate for people in their first completed
#' renewal period.
#' @param data A \code{data.frame} that has the same variables as a \code{RevenueData} object.
#' @param by The time period to aggregate the dates by: 
#' \code{"year"}, \code{"quarter"}, \code{"month"}, \code{"week"}, 
#' and \code{"day"}.
#' @param ... Other arguments.
#' 
#' @details For example, if the subscriptions are annual, computes 
#' proportion of people in 2012 that churned in 2013. Customers that had
#' an incomplete year that falls within a calendar year will be excluded
#' from this metric (e.g., a license from 1 January to 1 December 2012  will
#' not appear in this calculation, but their renewal year may instead). 
#' This calculation is taken from the one-off diagonal of \code{RecurringRevenueChurnByCohort}.
#' 
#' @export
InitialRecurringRevenueChurn <- function(data, by, ...)
{
    churn.cohort <- RecurringRevenueChurnByCohort(data, TRUE, by)
    InitialChurn(churn.cohort, remove.last = TRUE, by = by)
}

#' @export
plot.InitialChurn <- function(x, ...)
{
    smooth <- if (length(x) < 4) "None" else "Friedman's super smoother"
    y.title <- if(attr(x, "volume")) "Year 1 Churn rate ($)" else "Year 1 Churn rate (customers)"
    columnChart(x, fit.type = smooth, y.title = y.title, y.tick.format = "%", ...)
}
