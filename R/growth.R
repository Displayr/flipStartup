#' Growth
#'
#' @description Sales and sales growth.
#' @param x A vector showing some metric with scale properties
#' @param start The data at which the analysis should start.
#' @param end The data at which the analysis should end.
#' @param by The time period to aggregate the dates by: 
#' \code{"year"}, \code{"quarter"}, \code{"month"}, \code{"week"}, 
#' and \code{"day"}.
#' @param remove.last Remove the final period (as usually is incomplete).
#' @return A \code{\link{list}} containing the following elements:
#' \item{revenue}{The revenue by \code{period}}.
#' \item{growth}{The revenue growth, as a proportion, by \code{period}}.
#'
#' @importFrom flipTime AsDate
#' @export
Growth <- function(x, start, end, by, remove.last = TRUE)
{
    k <- length(x)
    g <- x[-1] / x[-k] - 1
    g[!is.finite(g)] <- NA
    g <- removeStartEndLastVector(g, start, end, remove.last)
    addAttributesAndClass(g, "Growth", by, detail = x)
}


#' \code{RecurringRevenueGrowth}
#'
#' @description Computes retention, by cohort.
#' @param data A \code{data.frame} that has the same variables as a \code{RevenueData} object.
#' @param by The time period to aggregate the dates by: 
#' \code{"year"}, \code{"quarter"}, \code{"month"}, \code{"week"}, 
#' and \code{"day"}.
#' @param ... Additional arguments to be passed to lower level functions.
#' @details Where subscribers suspends their purchasing for a period, 
#' but purchases again later, the subscriber
#' is included in the churn. Churn is show for all periods that have 
#' data in \code{data$to.period}, even if they occur in the future.
#' @return A named vector showing churn.
#' @importFrom flipTime AsDate Period
#' @export
RecurringRevenueGrowth <- function(data, by, ...)
{
    x <- RecurringRevenue(data, by)
    g <- Growth(x, attr(data, "start"), attr(data, "start"), by, remove.last = TRUE)
    attr(g, "y.title") <- "Growth in Recurring Revenue"
    g
}

#' \code{RecurringRevenueGrowth}
#'
#' @description Computes retention, by cohort.
#' @param data A \code{data.frame} that has the same variables as a \code{RevenueData} object.
#' @param by The time period to aggregate the dates by: 
#' \code{"year"}, \code{"quarter"}, \code{"month"}, \code{"week"}, 
#' and \code{"day"}.
#' @param ... Additional arguments to be passed to lower level functions.
#' @details Where subscribers suspends their purchasing for a period, 
#' but purchases again later, the subscriber
#' is included in the churn. Churn is show for all periods that have 
#' data in \code{data$to.period}, even if they occur in the future.
#' @return A named vector showing churn.
#' @importFrom flipTime AsDate Period
#' @export
CustomerGrowth <- function(data, by, ...)
{
    x <- Customers(data,  by)
    g <- Growth(x, attr(data, "start"), attr(data, "start"), by, remove.last = TRUE)
    attr(g, "y.title") <- "Growth in Customers"
    g
}

print.Growth <- function(x, ...)
{
    printWithoutAttributes(x)
}

#' @export
plot.Growth <- function(x, ...)
{
    if (sum(!is.na(x)) == 0)
        return(NULL)
    columnChart(x,  y.tick.format = "%", y.title = attr(x, "y.title"), ...)
}

