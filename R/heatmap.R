
#' \code{Heatmap}
#'
#' @description A heatmap of a matrix.
#' @param x A matrix.
#' @param title The title of the chart.
#' @return A plotly plot.
#' @importFrom flipFormat FormatAsReal
#' @importFrom plotly layout config
#' @export
Heatmap <- function(x, title)
{
    if (missing(title))
        title = deparse(substitute(x))
    row.title <- names(dimnames(x))[1]
    if (is.null(row.names))
        row.names = ""
    column.title <- names(dimnames(x))[2]
    if (is.null(row.names))
        row.names = ""
    hover.text <- x#FormatAsReal(x, 2)
    hover.text <- paste(title, hover.text)
    hover.text <- matrix(hover.text, ncol = nrow(x))
    p <- plot_ly(z = x,
            x = rownames(x),
            y = colnames(x),
            text = hover.text,
            hoverinfo = "text",
            type = "heatmap",
            colorbar = list(title = title, min = -1))
    p <- config(p, displayModeBar = FALSE)
    layout(p, xaxis = list(title = row.title),
               yaxis = list(title = column.title))
}

