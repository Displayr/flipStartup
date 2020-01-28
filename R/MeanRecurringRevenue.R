#' \code{MeanRecurringRevenueByCohort}
#'
#' @description Computes recurring revenue, by cohort.
#' @param data A \code{data.frame} that has the same variables as a \code{RevenueData} object.
#' @param days.to.count The number of days after the initial commencing to include in the Recurring Revenue
#' calculation.
#' @param by The time period to show value by 
#' @return A matrix
#' @importFrom flipTime AsDate Period
#' @importFrom flipStatistics Table
#' @export
MeanRecurringRevenue <- function(data, days.to.count, by, ...)
{
    start <- attr(data, "start")
    end <- attr(data, "end") - days.to.count
    data <- data[data$from <= data$subscriber.from + days.to.count, ]
    data$subscriber.from.period <- Period(data$subscriber.from, by)
    counts <- Table(id ~ subscriber.from.period, data = data, FUN = function(x) {length(unique(x))})
    value <- Table(recurring.value ~ subscriber.from.period, data = data, FUN = sum)
    out <- value / counts
    dates <- AsDate(names(out))
    out <- out[dates >= start & dates <= end]
    attr(out, "days.to.count") <- days.to.count
    attr(out, "detail") <- data[, c("subscriber.from", "from", "id", "recurring.value")]
    class(out) <- c("MeanRecurringRevenue", class(out))
    out
}



#' \code{MeanRecurringRevenueByYearCohort}
#'
#' @description Computes annual recurring revenue committed on the first day that the licenses start.
#' @param data A \code{data.frame} that has the same variables as a \code{RevenueData} object.
#' @param by The time period to show value by 
#' @param ... Don't use. There to prevent errors when unnecessary parameters are passed
#' @return A matrix
#' @export
MeanRecurringRevenueInitial <- function(data, by,  ...)
{
    MeanRecurringRevenue(data, by,  days.to.count = 0)
}        

#' \code{MeanRecurringRevenue30Days}
#'
#' @description Computes annual recurring revenue by day 30  (from the date of the first license start)
#' @param data A \code{data.frame} that has the same variables as a \code{RevenueData} object.
#' @param by The time period to show value by 
#' @param ... Don't use. There to prevent errors when unnecessary parameters are passed
#' @return A matrix
#' @export
MeanRecurringRevenue30Days <- function(data, by,  ...)
{
    MeanRecurringRevenue(data, by,  days.to.count = 30, ...)
}        

#' \code{MeanRecurringRevenue90Days}
#'
#' @description Computes annual recurring revenue by day 90  (from the date of the first license start)
#' @param data A \code{data.frame} that has the same variables as a \code{RevenueData} object.
#' @param by The time period to show value by 
#' @param ... Don't use. There to prevent errors when unnecessary parameters are passed
#' @return A matrix
#' @export
MeanRecurringRevenue90Days <- function(data, by,  ...)
{
    MeanRecurringRevenue(data, by,  days.to.count = 90, ...)
}        

#' \code{MeanRecurringRevenue180Days}
#'
#' @description Computes annual recurring revenue by day 180  (from the date of the first license start)
#' @param data A \code{data.frame} that has the same variables as a \code{RevenueData} object.
#' @param by The time period to show value by 
#' @param ... Don't use. There to prevent errors when unnecessary parameters are passed
#' @return A matrix
#' @export
MeanRecurringRevenue180Days <- function(data, by,  ...)
{
    MeanRecurringRevenue(data, by,  days.to.count = 180, ...)
}        

#' \code{MeanRecurringRevenue365Days}
#'
#' @description Computes annual recurring revenue by day 365 (from the date of the first license start)
#' @param data A \code{data.frame} that has the same variables as a \code{RevenueData} object.
#' @param by The time period to show value by 
#' @param ... Don't use. There to prevent errors when unnecessary parameters are passed
#' @return A matrix
#' @export
MeanRecurringRevenue180Days <- function(data, by,  ...)
{
    MeanRecurringRevenue(data, by,  days.to.count = 365, ...)
}        

#' \code{MeanRecurringRevenue2Years}
#'
#' @description Computes annual recurring revenue by day 730 (from the date of the first license start)
#' @param data A \code{data.frame} that has the same variables as a \code{RevenueData} object.
#' @param by The time period to show value by 
#' @param ... Don't use. There to prevent errors when unnecessary parameters are passed
#' @return A matrix
#' @export
MeanRecurringRevenue2Years <- function(data, by,  ...)
{
    MeanRecurringRevenue(data, by,  days.to.count = 730, ...)
}        

#' @export
print.MeanRecurringRevenue <- function(x, ...)
{
    attr(x, "detail") <- NULL
    attr(x, "volume") <- NULL
    attr(out, "days.to.count") <- NULL
    class(x) <- class(x)[-1]
    print(x)
}

#' @export
plot.MeanRecurringRevenue <- function(x, ...)
{
    days <- attr(x, "days.to.count")
    y.title <- ""
    if (days > 0)
    {
        time <- if (days <= 360) paste(days, "days") else  paste(round(days/365.25), "years")
        y.title <- paste0("Recurring revenue per new customer (first", time, ")")
    }
    columnChart(x, y.title = y.title, y.tick.format = "$")
}
