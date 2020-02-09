#' \code{CustomerChurnByCohort}
#'
#' @description Computes the customer churn by time period and time-based cohort
#' @param data A \code{data.frame} that has the same variables as a \code{RevenueData} object.
#' @param remove.last Remove the final period (as usually is incomplete).
#' @param by The time period to aggregate the dates by: 
#' \code{"year"}, \code{"quarter"}, \code{"month"}, \code{"week"}, 
#' and \code{"day"}.
#' @param ... Additional arguments to be passed to lower level functions.
#' @return A \code{\link{matrix}} 
#' @importFrom flipTime Period
#' @export
CustomerChurnByCohort <- function(data, remove.last = FALSE, by, ...)
{
    data <- updateDataForPeriodAndChurn(data, by)
    if (nrow(data) == 0)
        return(NULL)
    n.subscribers <- subscribersByCohortAndPeriod(data, by, remove.last)
    n.churn <- calculateNChurn(data, n.subscribers)
    rate <- churnRate(data, n.churn, n.subscribers, n.subscribers, by)
    attr(rate, "volume") <- FALSE
    rate
}


calculateNChurn <- function(data, n.subscribers)
{
    churn.data <- data[data$churn, ]
    if (nrow(churn.data) == 0)
        return(matrix(0, nrow(n.subscribers), ncol(n.subscribers),
                      dimnames = dimnames(n.subscribers)))
    Table(id ~ subscriber.from.period + to.renewal.period, data = churn.data, FUN = nUnique)
}

#' \code{RecurringRevenueChurnByCohort}
#'
#' @description Computes the recurring revenue churn by time period and time-based cohort
#' @param data A \code{data.frame} that has the same variables as a \code{RevenueData} object.
#' @param remove.last Remove the final period (as usually is incomplete).
#' @param by The time period to aggregate the dates by: 
#' \code{"year"}, \code{"quarter"}, \code{"month"}, \code{"week"}, 
#' and \code{"day"}.
#' @param ... Additional arguments to be passed to lower level functions.
#' @return A \code{\link{matrix}} 
#' @importFrom flipTime Period
#' @export
RecurringRevenueChurnByCohort <- function(data, remove.last = FALSE, by, ...)
{
    data <- updateDataForPeriodAndChurn(data, by)
    if (nrow(data) == 0)
        return(NULL)
    n.subscribers <- subscribersByCohortAndPeriod(data, by, remove.last)
    total <- Table(recurring.value ~ subscriber.from.period + to.renewal.period, 
                   data = data, 
                   FUN = sum)
    lost <- Table(recurring.value ~ subscriber.from.period + to.renewal.period, 
                  data = data[data$churn, ], 
                  FUN = sum)
    rate <- churnRate(data, lost, total, n.subscribers, by)
    attr(rate, "volume") <- TRUE
    rate
}

updateDataForPeriodAndChurn <- function(data, by)
{
    data <- removeIncompleteSubscriptions(data)
    updatePeriods(data, by)
}

#' @importFrom flipTime Period
updatePeriods <- function(data, by)
{
    data$subscriber.from.period <- Period(data$subscriber.from, by)
    data$to.renewal.period <- Period(data$to.renewal, by)
    data
}

#' @importFrom flipTime AsDate
subscribersByCohortAndPeriod <- function(data, by, remove.last)
{
    n.subscribers <- Table(id ~ subscriber.from.period + to.renewal.period, 
                           data = data, 
                           FUN = nUnique)
    all.periods <- unique(unlist(dimnames(n.subscribers)))
    dt <- CompleteListPeriodNames(all.periods, by)
    n.subscribers <- FillInMatrix(n.subscribers, dt, dt, value = 0)
    keep <- periodsToKeep(dt, attr(data, "start"), attr(data, "end"), remove.last)
    n.subscribers <- n.subscribers[, keep, drop = FALSE]
    names(dimnames(n.subscribers)) <- c("Commenced", properCase(by))
    n.subscribers
}

#' @importFrom flipTime AsDate
churnRate <- function(data, numerator, denominator, n.subscribers, by)
{
    dt <- rownames(n.subscribers)
    num <- FillInMatrix(numerator, dt, dt, value = 0)
    den <- FillInMatrix(denominator, dt, dt, value = 0)
    rate <- num / den
    names(rate) <- names(n.subscribers)
    rate <- rate[rownames(n.subscribers), colnames(n.subscribers), drop = FALSE]
    detail <- churnByCohortDetail(data, by)
    rate <- addAttributesAndClass(rate, "ChurnByCohort", by, detail)
    attr(rate, "n.subscribers") <- n.subscribers
    attr(rate, "subscription.length") <- attr(data, "subscription.length")
    dimnames(rate) <- dimnames(n.subscribers)
    rate
}

churnByCohortDetail <- function(data, by)
{
    if(sum(data$churn) == 0)
        return(NULL)
    detail <- aggregate(recurring.value ~ subscriber.from.period + period.counter + id, data, sum, subset = data$churn)
    colnames(detail) <- c("Recurring Revenue", "Commenced", properCase(by))
    detail
}

#' @export
print.ChurnByCohort <- function(x, ...)
{
    printWithoutAttributes(x)
}


#' @importFrom plotly plot_ly layout `%>%`
#' @importFrom flipFormat FormatAsPercent
#' @export
plot.ChurnByCohort <- function(x, ...)
{
    by <- properCase(attr(x, "by"))
    churn.type <- if(attr(x, "volume")) "Recurring Revenue " else "Customer "
    n <- c(attr(x, "n.subscribers"))
    hover.text <- matrix(paste0("Commenced: ", rownames(x), "<br>",
                                by, ": <br>", colnames(x), "<br>",
                                churn.type, "Churn Rate: ", FormatAsPercent(x, decimals = 1), "<br>",
                                "Base: ", n), nrow(x))#, "<extra></extra>")
    plot_ly(
        x = colnames(x),
        y = rownames(x),
        z = x, 
        colors = colorRamp(max(x), list(...)$y.max), 
        text = hover.text,
        hoverinfo = "text",
        type = "heatmap", 
        showscale = FALSE
    ) %>% config(displayModeBar = FALSE)
}