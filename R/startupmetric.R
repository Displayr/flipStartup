#' \code{StartupMetric}
#'
#' @description Creates a small multiple plot by sub-groups
#' @inherit RevenueData
#' @param FUN A function that calculates a metric
#' @param output Whether to output as a Plot, Table, or List. 
#' @param profiling Separate analyses are conducted among each unique combination of these variables.
#' @importFrom plotly add_annotations subplot
#' @return A plotly plot#?
#' @export
StartupMetric <- function(FUN = "Acquisition",
                          output = c("Plot", "Table", "List")[1],
                          # parameters from RevenueData
                          value, from, to, start = min(from), end = max(from), id,
                          subscription.length = "year", subset = rep(TRUE, length(id)),
                          profiling = NULL, trim.id = 50)
{
    filters <- createFilters(profiling, subset = NULL, id)
    n.filters <- length(filters)
    out <- list()
    for (i in 1:n.filters)
    {
        rd <- RevenueData(value, from, to, start, end ,id, subscription.length, subset = filters[[i]], profiling = NULL, trim.id)
        out[[i]] <- do.call(FUN, list(rd))
    }
    names(out) <- names(filters)
    switch(output,
           Plot = plotSubGroups(out),
           Table = lapply(out, Tab),
           List = out)
}
    

createFilters <- function(profiling, subset, id)
{
    if (is.null(subset))
        subset <- rep(TRUE, length(id))
    if (is.null(profiling))
        return(list(subset))
    subsets <- list()
    # Converting all variables to factors
    for (i in 1:NCOL(profiling))
    {
        p <- trimws(as.character(profiling[[i]]))
        p[is.na(p)] <- "MISSING DATA"
        profiling[[i]] <- p
    }
    if (is.null(subset))
        subset <- TRUE
    n.profiling <- nrow(profiling)
    levs <- lapply(profiling, unique)
    combs <- expand.grid(levs)
    n.combinations <- nrow(combs)
    for (i in 1:n.combinations)
    {
        filts <- combs[rep(i, n.profiling), , drop = FALSE]
        f <- apply(profiling == filts, 1, all) & subset
        f[is.na(f)] <- FALSE
        subsets[[i]] <- f
    }
    nms <- apply(combs,1, function(x) paste(as.character(x), collapse = " + "))
    nms <- trimws(nms)
    nms <- paste0(nms, "\nn: ", sapply(subsets, function(x) length(unique(id[x]))))
    names(subsets) <- nms
    # Filtering out empty subsets
    subsets[sapply(subsets, function(x) length(x) > 0)]
}


plotSubGroups <- function(x, ...)
{
    n.plots <- length(x)
    if (n.plots == 1)
        return(print(plot(x[[1]])))
    plots <- lapply(x, FUN = plot, ...)
    # Adding titles
    for (i in seq_along(plots))
        plots[[i]]  <- add_annotations(plots[[i]], 
                                     text = names(x)[i],
                                     x = 0.5,
                                     y = 1,
                                     yref = "paper",
                                     xref = "paper",
                                     xanchor = "middle",
                                     yanchor = "top",
                                     showarrow = FALSE,
                                     font = list(size = 15))
    nr <- floor(sqrt(n.plots - 1))
    print(nr)
    print(subplot(plots, nrows = nr, shareX = TRUE, shareY = TRUE))
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
