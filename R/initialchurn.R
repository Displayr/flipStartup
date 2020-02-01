#' \code{InitialChurn}
#'
#' @description Computes the churn rate in the initial time period
#' @param data A \code{data.frame} that has the same variables as a \code{RevenueData} object.
#' @param remove.last Remove the final period (as usually is incomplete).
#' @param volume Weights the results by volume. Does nothing in this case.
#' @param by The time unit to plot. E.g., "month".
#' @param ... Other arguments.
#' @return A \code{\link{matrix}} 
#' @export
InitialChurn <- function(data, remove.last = TRUE, volume = FALSE, by, ...)
{
    x.matrix <- ChurnByCohort(data, remove.last , volume, by)
    # if (remove.last)
    #     x.matrix <- x.matrix[, -ncol(x.matrix)]
    k <- nrow(x.matrix)
    x <- diag(x.matrix[, -1])#[diag(k)[, k:1] == 1]
    names(x) <- colnames(x.matrix)[-1]
    class(x) <- c("InitialChurn", class(x))
    attr(x, "subscription.length") <- attr(data, "subscription.length")
    attr(x, "n.subscriptions") <- attr(data, "n.subscriptions")
    attr(x, "volume") <- volume
    x
}

#' InitialCustomerChurn
#' 
#' @export
InitialCustomerChurn <- function(data, by, ...)
{
    InitialChurn(data, remove.last = TRUE, volume = FALSE, by = by)
}

#' InitialRecurringRevenueChurn
#' 
#' @export
InitialRecurringRevenueChurn <- function(data, by, ...)
{
    InitialChurn(data, remove.last = TRUE, volume = TRUE, by = by)
}


#' @export
plot.InitialChurn <- function(x, ...)
{
    y.title <- if(attr(x, "volume")) "Year 1 Churn rate ($)" else "Year 1 Churn rate (customers)"
    columnChart(x, y.title = y.title, y.tick.format = "%", ...)
}
