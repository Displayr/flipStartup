#' \code{ConnectedDotPlot}
#'
#' @description Plots the subscription periods as a dotplot, with lines showing when subscriptions occurred
#' @param data A \code{data.frame} that has the same variables as a \code{RevenueData} object.
#' @importFrom plotly plot_ly
#'
#' @export
ConnectedDotPlot <- function(name, from, to)
{
    dat <- data.frame(name, from, to)
    n <- nrow(dat)
    # Adding the dots
    suppressWarnings(p <- plot_ly(dat, x = ~from, y = ~name, mode = "markers", type = "scatter", marker = list(color = "blue")))#, marker = list(color = "blue")))
    p <- add_trace(p, x = ~to,  y = ~name, mode = "markers", type = "scatter", marker = list(color = "blue"))
    temp.x <- rep(from, 3)
    temp.x[(1:n)*3-2] <- from
    temp.x[(1:n)*3-1] <- to
    temp.y <- rep(name, each = 3)
    temp.y[(1:n)*3] <- NA
    p <- add_trace(p, x = temp.x,  y = temp.y, mode = "lines", type = "scatter", line = list(color = "blue"))
    p <- config(p, displayModeBar = FALSE)
    p <- layout(p,
        title = "Subscriptions", #,
        xaxis = list(title = ""),
        yaxis = list(title = "", showticklabels = FALSE),
        margin = list(l = 0),
        showlegend = FALSE)
    p
}
