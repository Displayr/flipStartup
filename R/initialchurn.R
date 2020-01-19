#' \code{InitialChurn}
#'
#' @description Computes the churn rate in the initial time period
#' @param data A \code{data.frame} that has the same variables as a \code{RevenueData} object.
#' @param remove.last Remove the final period (as usually is incomplete).
#' @param volume Weights the results by volume. Does nothing in this case.
#' @return A \code{\link{matrix}} 
#' @export
InitialChurn <- function(data, remove.last = TRUE, volume = FALSE)
{
    x.matrix <- ChurnByCohort(data, remove.last, volume)
    k <- nrow(x.matrix)
    x <- diag(x.matrix)#[diag(k)[, k:1] == 1]
    names(x) <- rownames(x.matrix)
    class(x) <- c("InitialChurn", class(x))
    attr(x, "subscription.length") <- attr(data, "subscription.length")
    attr(x, "n.subscriptions") <- attr(data, "n.subscriptions")
    attr(x, "volume") <- volume
    x
}

#' @export
plot.InitialChurn <- function(x, ...)
{
    y.title <- if(attr(x, "volume")) "Churn rate ($)" else "Churn rate (customers)"
    columnChart(x, y.title = y.title, ...)
}
