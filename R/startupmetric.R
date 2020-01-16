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
    y.max <- 0
    for (i in 1:n.filters)
    {
        rd <- RevenueData(value, from, to, start, end ,id, subscription.length, subset = filters[[i]], profiling = NULL, trim.id)
        out[[i]] <- do.call(FUN, list(rd))
        y.max <- max(y.max, out[[i]]$counts) # not sure if this works for other FUN??
    }
    names(out) <- names(filters)
    switch(output,
           Plot = plotSubGroups(out,
                # need to specify bounds to ensure subplot share axis properly 
                x.bounds.minimum = format(min(from), "%Y-%m-%d"), # pass date as a string
                x.bounds.maximum = format(max(from), "%Y-%m-%d"),
                x.tick.format = "%b %Y",  # specify date format to help flipStandardChart figure out parsing
                y.bounds.maximum = y.max,
                y.bounds.minimum = 0, 
                opacity = 1.0),
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
    nr <- floor(sqrt(n.plots - 1))
    nc <- ceiling(n.plots/nr)
    pp <- subplot(plots, nrows = nr, shareX = TRUE, shareY = TRUE)

    # Adding titles
    annotations <- list()       
    titles.ypos <- rep((nr:1)/nr, each = nc)[1:n.plots]
    titles.xpos <- rep((1:nc - 0.5)/nc, nr)[1:n.plots]
    for (i in seq_along(plots))
    {
        annotations[[i]]  <- list(text = names(x)[i],
                                     x = titles.xpos[i],
                                     y = titles.ypos[i],
                                     yref = "paper",
                                     xref = "paper",
                                     xanchor = "center",
                                     yanchor = "top",
                                     showarrow = FALSE,
                                     font = list(size = 15))
    }
    pp <- layout(pp, annotations = annotations)
    print(pp)
}
