#' \code{ChurnByCohort}
#'
#' @description Computes the churn by time period and time-based cohort
#' @param data A \code{data.frame} that has the same variables as a \code{RevenueData} object.
#' @param remove.last Remove the final period (as usually is incomplete).
#' @param volume Weights the results by volume. Does nothing in this case.
#' @return A \code{\link{matrix}} 
#' @export
ChurnByCohort <- function(data, remove.last = TRUE, volume = FALSE)
{
    x <- 1 - Retention(data)[[ if (volume) "retention.rate.volume" else "retention.rate"]]
    class(x) <- c("ChurnByCohort", class(x))
    attr(x, "subscription.length") <- attr(data, "subscription.length")
    attr(x, "n.subscriptions") <- attr(data, "n.subscriptions")
    if (remove.last)
        x <- x[-nrow(x), -ncol(x)]
    x
}

#' @importFrom plotly plot_ly layout
#' @export
plot.ChurnByCohort <- function(x, ...)
{
    colorscale <- list(...)$colorscale
    colorscale <- colorscale[match(c(x), colorscale[, 1]), ]
    by <- properCase(attr(x, "subscription.length"))
    n <- c(attr(x, "n.subscriptions"))
    plot_ly(
        x = colnames(x),
        y = rownames(x),
        z = x, 
        colorscale = colorscale,
        hovertemplate = paste0("Commenced: %{y}<br>",by,": %{x}<br>Churn: %{z:%}<br>Base: %{n}<extra></extra>"),
        type = "heatmap", 
        showscale = FALSE
    )
}
