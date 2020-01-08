#' \code{PlotSubGroups}
#'
#' @description Creates a small multiple plot by sub-groups
#' @param data an R object, normally a data frame, possibly a matrix.
#' @param INDICES A character vecto indicating elements of data to be used as
#' variable for constructing sub-plots..
#' @param FUN A function to be applied to (usually data-frame) subsets of data. 
#' This needs to return an objet for which a plot method exist which returns
#' a plotly plot.
#' @param ... Further arguments to FUN.
#' @importFrom plotly add_annotations subplot
#' @return A plotly plot#
#' @export
PlotSubGroups <- function(data, INDICES, FUN, ...)
{
    plots <- by(data, data[, INDICES], FUN = function(x) plot(FUN(x, ...)))
    # Adding titles
    for (i in seq_along(plots))
        plots[[i]]  <- add_annotations(plots[[i]], 
                                     text = names(plots)[i],
                                     x = 0.5,
                                     y = 1,
                                     yref = "paper",
                                     xref = "paper",
                                     xanchor = "middle",
                                     yanchor = "top",
                                     showarrow = FALSE,
                                     font = list(size = 15))
    subplot(plots, shareX = TRUE, shareY = TRUE)
}


# 
# 
# subplots
# 
# out <- list(Overall = plot(Acquisition(x)),
#             Overall = plot(Acquisition(x, subset = x$Product == "Q")),
#             Overall = plot(Acquisition(x, subset = x$Product == "Displayr")))
# 
# plotly::subplot(out, shareX = TRUE, shareY = TRUE)
# 
# 
# 
# 
# 
# 
# 
# gg <- local({
#     k <- function(y)f(y)
#     f <- function(x) if(x) x*k(x-1) else 1
# })
# gg(10)
# sapply(1:5, gg)
# 
# 
