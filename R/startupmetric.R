#' \code{StartupMetric}
#'
#' @description Creates a small multiple plot by sub-groups
#' @inherit RevenueData
#' @param FUN A function that calculates a metric
#' @param output Whether to output as a Plot, Table, or List. 
#' @param volume If TRUE, computes a volumetric analysis (i.e, based on value rather than number of events or customers).
#' @param profiling Separate analyses are conducted among each unique combination of these variables.
#' @importFrom plotly add_annotations subplot
#' @return A plotly plot#?
#' @export
StartupMetric <- function(FUN = "Acquisition",
                          output = c("Plot", "Table", "List")[1],
                          volume = FALSE,
                          # parameters from RevenueData
                          value, from, to, start = min(from), end = max(from), id,
                          subscription.length = "year", subset = rep(TRUE, length(id)),
                          profiling = NULL, trim.id = 50)
{
    filters <- createFilters(profiling, subset = subset, id)
    n.filters <- length(filters)
    out <- vector("list", n.filters)
    y.max <- 0
    y.min <- 0
    for (i in 1:n.filters)
    {
        rd <- RevenueData(value, from, to, start, end ,id, subscription.length, subset = filters[[i]], profiling = NULL, trim.id)
        metric <- do.call(FUN, list(rd, volume = volume))
        if (!is.null(metric))
        {
            out[[i]] <- metric
            r <- YLim(metric)
            y.min <- min(y.min, r[1])
            y.max <- max(y.max, r[2])
        }
    }
    names(out) <- names(filters)
    out <- out[!sapply(out, is.null)] # Removing any empty strings
    switch(output,
           Plot = createPlots(out, start, end, y.min, y.max),
           Table = asMatrix(out),
           Detail = lapply(out, function(x) attr(x, "detail")))
}

#' @importFrom flipTime AsDate Period
asMatrix <- function(x)
{
    by <- attr(x[[1]], "subscription.length")
    rng <- sapply(x, function(x) names(x)[c(1, length(x))])
    mn <- min(AsDate(rng[1,]))
    mx <- max(AsDate(rng[2,]))
    dates <- Period(seq.Date(mn, mx, by = by), by)
    k <- length(x)
    m <- matrix(NA, length(dates), k, dimnames = list(dates, names(x)))
    for (i in 1:k)
        m[names(x[[i]]), i] <- x[[i]]
    m
}
    

#' @importFrom scales col_numeric rescale
#' @importFrom stats setNames
colorScale <- function(x)
{
    u <- unique(unlist(lapply(x, function(x) unique(c(x)))))
    vals <- unique(scales::rescale(u))
    cols <- col_numeric("Blues", domain = NULL)(vals)
    setNames(data.frame(vals, cols), NULL)
}


createPlots <- function(x, start, end, y.min, y.max)
{
    if (requiresHeatmap(x))
    {
        plotSubGroups(x, colorscale = colorScale(x))
    }
    else if ("RevenueByCohort" %in% class(x[[1]]))
        plotSubGroups(x)
    else plotSubGroups(x,
                  # need to specify bounds to ensure subplot share axis properly 
                  x.bounds.minimum = format(start, "%Y-%m-%d"), # pass date as a string
                  x.bounds.maximum = format(end, "%Y-%m-%d"),
                  x.tick.format = "%b %y",  # specify date format to help flipStandardChart figure out parsing
                  y.bounds.minimum = y.min, 
                  y.bounds.maximum = y.max,
                  opacity = 1.0)
}


plotSubGroups <- function(x, ...)
{
    n.plots <- length(x)
    # if (n.plots == 1)
    #     return(print(plot(x[[1]])))
    plots <- lapply(x, FUN = plot, ...)
    pp <-     if (length(plots) == 1) plots[[1]] else 
    {
        nr <- floor(sqrt(n.plots - 1))
        nc <- ceiling(n.plots/nr)
        
        pp <- subplot(plots, nrows = nr, shareY = nc > 1, shareX = nr > 1)#, shareX = TRUE, )
    
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
        layout(pp, annotations = annotations)
    }
    print(pp)
}


#' @export
Detail <- function(x)
{
    UseMethod("Tab", x)
}

#' @export
Detail.default <- function(x, ...)
{
    attr(x, "detail")
}
#' 
#' #' @export
#' Data <- function(x)
#' {
#'     UseMethod("Data", x)
#' }
#' 
#' #' @export
#' Data.default <- function(x, ...)
#' {
#'     attr(x, "detail")
#' }

#' #' @export
#' Detail.default <- function(x, ...)
#' {
#'     attr(x, "detail")
#'     sapply(x$id, paste, collapse = ", ")
#' }



#' @export
YLim <- function(x)
{
    UseMethod("YLim", x)
}

#' @export
YLim.default <- function(x, ...)
{
    range(x)
}



#' @importFrom flipStandardCharts Column
columnChart <- function(x,  ...)
{
    smooth <- if (length(x) < 4) "None" else "Friedman's super smoother"
    Column(x,  x.tick.angle = 0,
                fit.type = smooth, fit.ignore.last = TRUE,
                fit.line.type = "solid", fit.line.width = 2, 
                fit.line.colors = "#ED7D31", ...)$htmlwidget
}

#' @importFrom flipStandardCharts Column
areaChart <- function(x,  ...)
{
    Area(x,  x.tick.angle = 0,
         fit.ignore.last = TRUE,
         fit.line.type = "solid", fit.line.width = 2, 
         fit.line.colors = "#ED7D31", ...)$htmlwidget
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

requiresHeatmap <- function(x)
{
    required.for <- c("ChurnByCohort", "RevenuePerSubscriberByCohortByTime")
    any(required.for %in% class(x[[1]]))
}
