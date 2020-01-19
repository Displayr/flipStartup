#' \code{RevenuePerSubscriberByCohortByTime}
#'
#' @description Computes mean revenue by time period and time since the subscriber joined.
#' @param data A \code{data.frame} that has the same variables as a \code{RevenueData} object.
#' @param remove.last Remove the final period (as usually is incomplete).
#' @param volume Weights the results by volume. Does nothing in this case.
#' @return A \code{\link{matrix}} 
#' @export
RevenuePerSubscriberByCohortByTime <- function(data, remove.last = FALSE, volume = FALSE)
{
    x <- Lifetime(data)$revenue.per.subscriber
    if (remove.last)
        x <- x[-nrow(x), -ncol(x)]
    class(x) <- c("RevenuePerSubscriberByCohortByTime", class(x))
    attr(x, "subscription.length") <- attr(data, "subscription.length")
    x
}


#' @importFrom plotly plot_ly layout
#' @export
plot.RevenuePerSubscriberByCohortByTime <- function(x, ...)
{
    colorscale <- list(...)$colorscale
    colorscale <- colorscale[match(c(x), colorscale[, 1]), ]
    by <- attr(x, "subscription.length")
    by <- paste0(toupper(substr(by, 1, 1)), tolower(substring(by, 2))) # capitalizing first letter
    plot_ly(
        x = colnames(x),
        y = rownames(x),
        z = x, 
        colorscale = colorscale,
        hovertemplate = paste0("Commenced: %{y}<br>",by,"s since starting: %{x}<br>Revenue: %{z:$.0f}<extra></extra>"),
        colors = "Blues",
        type = "heatmap", 
        showscale = FALSE
    )
}
