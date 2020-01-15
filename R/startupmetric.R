#' \code{StartupMetric}
#'
#' @description Creates a small multiple plot by sub-groups
#' @inherit RevenueData
#' @param profiling Separate analyses are conducted among each unique combination of these variables.
#' @FUN A function that calculates a metric
#' @as.plot If \code{TRUE}, outputs the metric as a plot
#' @importFrom plotly add_annotations subplot
#' @return A plotly plot#?
#' @export
StartupMetric <- function(value, 
                          from, 
                          to, 
                          start, 
                          end, 
                          id, 
                          subscription.length, 
                          subset,
                          profiling = NULL, 
                          trim.id = 50,
                          FUN = "Acquisition",
                          as.plot = TRUE)
{
    filters <- createFilters(profiling, subset)
    n.filters <- length(filters)
    out <- list()
    for (i in 1:n.filters)
    {
        rd <- RevenueData(value, from, to, start, end ,id, subscription.length, subset = filters[[i]], trim.id)
        out[[i]] <- do.call(FUN, list(rd))
    }
    if (as.plot)
        plotSubGroups(out)
    else out
}
    

createFilters <- function(profiling, subset = NULL)
{
    if (is.null(profiling))
        return(list(subset))
    subsets <- list()
    # Converting all variables to factors
    for (i in 1:NCOL(profiling))
    {
        p <- as.character(profiling[[i]])
        p[is.na(p)] <- "MISSING DATA"
        profiling[[i]] <- factor(p)
    }
    if (is.null(subset))
        subset <- TRUE
    n.profiling <- nrow(profiling)
    levs <- as.data.frame(sapply(profiling, levels))
    combs <- expand.grid(levs)
    n.combinations <- nrow(combs)
    for (i in 1:n.combinations)
    {
        filts <- combs[rep(i, n.profiling), , drop = FALSE]
        subsets[[i]] <- which(apply(profiling == filts, 1, all) & subset)
    }
    nms <- apply(combs,1, function(x) paste(as.character(x), collapse = " + "))
    nms <- paste0(nms, " n: ", sapply(nms, length))
    names(subsets) <- nms
    subsets
}


#' 
#' 
plotSubGroups <- function(data.to.be.plotted, ...)
{
    plots <- lapply(data.to.be.plotted, FUN = plot, ...)
    # Adding titles
    for (i in seq_along(plots))
        plots[[i]]  <- add_annotations(plots[[i]], 
                                     text = names(data.to.be.plotted)[i],
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
