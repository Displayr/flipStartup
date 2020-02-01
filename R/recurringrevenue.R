#' \code{RecurringRevenue}
#'
#' @description Computes recurring revenue, by cohort.
#' @param data A \code{data.frame} that has the same variables as a \code{RevenueData} object.
#' @param end The date on and after which which revenue is ignored.
#' Note that the default value, \code{Sys.time()} may not be in the same time zone as your other data, and this
#' can cause unexpected results.
#' @param by The time unit to plot. E.g., "month".
#' @details Computed based on being a subscribed on the last second of the time period. 
#' Partial revenue is multipled out. For exmaple, if with yearly data, if a customer has an annual license for $1,000
#' from 1 February to 1 February of the next year, and a second license which starts on 1 January of the last year
#' with a prorated price of $1000/12, then the recurring revenue is $2,000.
#' @return A vector showing the recurring revenue by time points.
#'
#' @export
RecurringRevenue <- function(data, end = Sys.time(), by = "day", ...)#attr(data, "subscription.length"))
{
    x <- Subscribers(data, by = by, end = end, volume = TRUE, recurring = TRUE)
    detail <- data[, c("id", "value", "from", "to")]
    addAttributesAndClass(x, "RecurringRevenue", by, detail)
}


addAttributesAndClass <- function(x, class.name, by, detail)
{
    attr(x, "by") <- by
    attr(x, "detail") <- detail
    class(x) <- c(class.name, class(x))
    x    
}    

#' @export
plot.RecurringRevenue <- function(x, ...)
{
    areaChart(x, y.title = "Annual Recurring Revenue", ...)
}
