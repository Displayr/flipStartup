#' \code{RevenueMetric}
#'
#' @description Creates a small multiple plot by sub-groups
#' @inherit RevenueData
#' @param FUN A function that calculates a metric
#' @param output Whether to output as a Plot, Table, or List. 
#' @param profiling Separate analyses are conducted among each unique combination of these variables.
#' @importFrom plotly add_annotations subplot
#' @return A plotly plot#?
#' @export
RevenueMetric <- function(FUN = "Acquisition",
                          output = c("Plot", "Table", "List")[1],
                          # parameters from RevenueData
                          value, from, to, start = min(from), end = max(from), id,
                          subscription.length = "year", subset = rep(TRUE, length(id)),
                          profiling = NULL, trim.id = 50, ...)
{
    filters <- createFilters(profiling, subset = subset, id)
    n.filters <- length(filters)
    out <- vector("list", n.filters)
    y.max <- 0
    y.min <- 0
    for (i in 1:n.filters)
    {
        capture.output(rd <- RevenueData(value, from, to, start, end ,id, subscription.length, subset = filters[[i]], profiling = NULL, trim.id))
        if (!is.null(rd))
        {
            metric <- do.call(FUN, list(rd, ...))
            if (!is.null(metric))
            {
                out[[i]] <- metric
                r <- YLim(metric)
                y.min <- min(y.min, r[1])
                y.max <- max(y.max, r[2])
            }
        }
    }
    names(out) <- names(filters)
    out <- out[sapply(out, function(x) NROW(x) > 0 )] # Removing any empty strings
    switch(output,
           Plot = createPlots(out, start, end, y.min, y.max),
           Table = asMatrix(out),
           Detail = lapply(out, function(x) attr(x, "detail")))
}

#' @importFrom flipTime AsDate Period
asMatrix <- function(x)
{
    if (length(x) == 1)
        return(as.matrix(x[[1]]))
    by <- attr(x[[1]], "by")
    is.m <- is.matrix(x[[1]])
    rng <- if (is.m) sapply(x, function(x) colnames(x)[c(1, ncol(x))])
         else sapply(x, function(x) names(x)[c(1, length(x))])
    mn <- min(AsDate(rng[1,]))
    mx <- max(AsDate(rng[2,]))
    dates <- Period(seq.Date(mn, mx, by = by), by)
    if (is.m) stackMatrices(x, dates) else spliceVectors(x, dates)
}

spliceVectors <- function(x, dates)
{
    k <- length(x)
    m <- matrix(0, length(dates), k, dimnames = list(dates, names(x)))
    for (i in 1:k)
        m[names(x[[i]]), i] <- x[[i]]
    m
}

stackMatrices <- function(x, dates)
{
    nr <- sum(sapply(x, nrow))
    m <- matrix(NA, nr, length(dates), dimnames = list(1:nr, dates))
    counter <- 0
    for (i in 1:length(x))
    {
        t <- x[[i]]
        rows <- counter + 1:nrow(t)
        m[rows, colnames(t)] <- t
        nm <- strsplit(names(x)[i], split = "\n", fixed = TRUE)[[1]][1]
        rownames(m)[rows] <- paste(nm,rownames(t))
    }
    m    
}

createPlots <- function(x, start, end, y.min, y.max)
{
    if (requiresHeatmap(x))
    {
        plotSubGroups(x, y.max)
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
                fit.type = smooth,
           colors = "#3e7dcc",
           fit.ignore.last = TRUE,
#                fit.line.type = "solid", 
           fit.line.width = 4, 
           fit.line.type = "dot",
                fit.line.colors = "#f5c524",
           ...)$htmlwidget
}

#' @importFrom flipStandardCharts Column
areaChart <- function(x,  ...)
{
    Area(x,  x.tick.angle = 0,
         colors = "#3e7dcc",
         fit.ignore.last = TRUE,
         fit.line.type = "solid", fit.line.width = 2, 
         fit.line.colors = "#f5c524", ...)$htmlwidget
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


#' @importFrom scales colour_ramp
colorRamp <- function(local.y.max, global.y.max){
    global.color.ramp <- colour_ramp(c("white", "#3E7DCC"))
    if (is.null(global.y.max))
        return(global.color.ramp)
    colour_ramp(c("white", global.color.ramp(local.y.max / global.y.max)))
}

print.RevenueMetric <- function(x, ...)
{
    printWithoutAttributes(x)
}    

printWithoutAttributes <- function(x)
{
    for (a in c("detail", "volume", "by", "subscription.length", "n.subscriptions"))
        attr(x, a) <- NULL
    class(x) <- class(x)[-1:-2]
    print(x)
}

addAttributesAndClass <- function(x, class.name, by, detail)
{
    attr(x, "by") <- by
    attr(x, "detail") <- detail
    class(x) <- c(class.name, "RevenueMetric", class(x))
    x    
}    
