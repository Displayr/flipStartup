#' \code{CustomerChurnByCohort}
#'
#' @description Computes the customer churn by time period and time-based cohort
#' @param data A \code{data.frame} that has the same variables as a \code{RevenueData} object.
#' @param remove.last Remove the final period (as usually is incomplete).
#' @param by The time unit to plot. E.g., "month".
#' @param ... Other arguments.
#' @return A \code{\link{matrix}} 
#' @importFrom flipTime Period
#' @export
CustomerChurnByCohort <- function(data, remove.last = FALSE, by, ...)
{
    data <- updateDataForPeriodAndChurn(data, by)
    if (nrow(data) == 0)
        return(NULL)
    n.subscribers <- subscribersByCohortAndPeriod(data, by)
    n.churn <- Table(id ~ subscriber.from.period + to.renewal.period, 
                     data = data[data$churn, ], 
                     FUN = nUnique)
    rate <- churnRate(data, n.churn, n.subscribers, n.subscribers, by, remove.last)
    attr(rate, "volume") <- FALSE
    rate
}

#' \code{RecurringRevenueChurnByCohort}
#'
#' @description Computes the recurring revenue churn by time period and time-based cohort
#' @param data A \code{data.frame} that has the same variables as a \code{RevenueData} object.
#' @param remove.last Remove the final period (as usually is incomplete).
#' @param by The time unit to plot. E.g., "month".
#' @param ... Other arguments.
#' @return A \code{\link{matrix}} 
#' @importFrom flipTime Period
#' @export
RecurringRevenueChurnByCohort <- function(data, remove.last = FALSE, by, ...)
{
    data <- updateDataForPeriodAndChurn(data, by)
    if (nrow(data) == 0)
        return(NULL)
    n.subscribers <- subscribersByCohortAndPeriod(data, by)
    total <- Table(recurring.value ~ subscriber.from.period + to.renewal.period, 
                   data = data, 
                   FUN = sum)
    lost <- Table(recurring.value ~ subscriber.from.period + to.renewal.period, 
                  data = data[data$churn, ], 
                  FUN = sum)
    rate <- churnRate(data, lost, total, n.subscribers, by, remove.last)
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
subscribersByCohortAndPeriod <- function(data, by)
{
    n.subscribers <- Table(id ~ subscriber.from.period + to.renewal.period, 
                           data = data, 
                           FUN = nUnique)
    all.periods <- unique(unlist(dimnames(n.subscribers)))
    dt <- CompleteListPeriodNames(all.periods, by)
    n.subscribers <- FillInMatrix(n.subscribers, dt, dt, value = 0)
    names(dimnames(n.subscribers)) <- c("Commenced", properCase(by))
    n.subscribers
}

#' @importFrom flipTime AsDate
churnRate <- function(data, numerator, denominator, n.subscribers, by, remove.last)
{
    dt <- rownames(n.subscribers)
    num <- FillInMatrix(numerator, dt, dt, value = 0)
    den <- FillInMatrix(denominator, dt, dt, value = 0)
# print(colSums(num, na.rm = TRUE))
# print(colSums(den - num , na.rm = TRUE))
# print(colSums(num, na.rm = TRUE) / colSums(den, na.rm = TRUE))

    rate <- num / den
    names(rate) <- names(n.subscribers)
    if (remove.last)
        rate <- rate[-nrow(rate), -ncol(rate)]
    detail <- aggregate(recurring.value ~ subscriber.from.period + id, data, sum, subset = churn)
    rate <- addAttributesAndClass(rate, "ChurnByCohort", by, detail)
    attr(rate, "n.subscribers") <- n.subscribers
    dimnames(rate) <- dimnames(n.subscribers)
    rate
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