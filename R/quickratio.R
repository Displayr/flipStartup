#' QuickRatioPlot
#'
#' @param x A GrowthAccounting object.
#' @param supress.first The periods to suppress from the beginning of the data
#' @importFrom plotly plot_ly add_trace layout
#' @export
QuickRatioPlot <- function(x, supress.first = 1)
{
    growth.table <- x$Table
    if (supress.first != 0)
    {
        growth.table <- growth.table[-1:-supress.first, ]
    }
    growth.table$Period <- rownames(growth.table)
    growth.table$Benchmark = 4
    p <- plot_ly(x = rownames(growth.table), hoverinfo="x+text", text=sprintf("%.2f", growth.table$Quick.Ratio),
                 y = growth.table$Quick.Ratio, type="scatter", mode = "lines", name = "Quick Ratio<br>(Acquisition + Exp. + Res.) / (Con. + Churned)")#, marker = list(color = "green"))
    p <- add_trace(p = p, x = rownames(growth.table), y = growth.table$Benchmark,
                   hoverinfo="x+text", text=sprintf("%.2f", growth.table$Benchmark),
                   name = "4 is excellent for a business SaaS")#, marker = list(color = "blue"))
    p <- layout(p = p, xaxis = list(title = "Period"), yaxis = list(title = "Quick Ratio", range = c(0, max(growth.table$Quick.Ratio, 4))))
    p
}
