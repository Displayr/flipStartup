#
# library(plotly)
# y <- c(-7.1, -7, 16.9, .7, 42)
# bs <- c(y[1], sum(y[1:2]), sum(y[1:2]), sum(y[1:3]), sum(y[1:4]))
#
# annotation.y <- c(0, y[1], bs[-1:-2] + y[-1:-2])
#
# a <- list()
# for (i in 1:length(y))
# {
#     #    m <- y[i]
#     a[[i ]] <- list(
#         x = x[i],
#         y = annotation.y[i] + 2,
#         text = paste0(y[i], "%"),
#         textposition = "top middle",
#         #        xref = "x",
#         #        yref = "y",
#         showarrow = FALSE#,
#         #        arrowhead = 7,
#         #        ax = 20,
#         #        ay = -40
#     )
# }
#
# for (i in 1:length(y))
# {
#     #    m <- y[i]
#     a[[i+ length(y)]] <- list(
#         x = x[i],
#         y = y[i] ,
#         text = paste0(y[i], "%"),
#         textposition =  "middle",
#         #        xref = "x",
#         #        yref = "y",
#         showarrow = FALSE#,
#         #        arrowhead = 7,
#         #        ax = 20,
#         #        ay = -40
#     )
# }
#
# x <- c("Churned", "Contraction", "Expansion", "Resurrected", "New")
# p <- plot_ly(
#     x = x,
#     y = bs,
#     marker = list(color = "white"),
#     hoverinfo='none',
#     type = "bar")
# temp.y <- abs(y)
# p <- add_trace(
#     p,
#     x = x[1],
#     y = temp.y[1],
#     name = x[1],
#     type = "bar")
# p <- add_trace(
#     p,
#     x = x[2],
#     y = temp.y[2],
#     name = x[2],
#     type = "bar")
# p <- add_trace(
#     p,
#     x = x[3],
#     y = temp.y[3],
#     name = x[3],
#     type = "bar")
# p <- add_trace(
#     p,
#     x = x[4],
#     y = temp.y[4],
#     name = x[4],
#     type = "bar")
# p <- add_trace(
#     p,
#     x = x[5],
#     y = temp.y[5],
#     name = x[5],
#     type = "bar")
#
# layout(p, barmode = "stack", showlegend = FALSE, annotations = a, xaxis = list(title = ""), yaxis = list(title = "Net growth"))
#
#
#
#
#
#
#
#
#
#
