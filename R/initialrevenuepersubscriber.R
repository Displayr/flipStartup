#' \code{InitialRevenuePerSubscriber}
#'
#' @description Computes mean revenue for subscripbers in their first time period.
#' @param data A \code{data.frame} that has the same variables as a \code{RevenueData} object.
#' @param remove.last Remove the final period (as usually is incomplete).
#' @param volume Weights the results by volume. Does nothing in this case.
#' @return A \code{\link{matrix}} 
#' @export
InitialRevenuePerSubscriber <- function(data, remove.last = FALSE, volume = FALSE)
{
    x <- RevenuePerSubscriberByCohortByTime(data, remove.last)[, 1]
    class(x) <- c("InitialRevenuePerSubscriber", class(x))
    attr(x, "subscription.length") <- attr(data, "subscription.length")
    x
}


#' @export
plot.InitialRevenuePerSubscriber <- function(x, ...)
{
    columnChart(x, y.title = "Initial Revenue Per Subscriber", ...)
}
