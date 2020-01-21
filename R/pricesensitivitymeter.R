#' Plots the Van Westendorp Price Sensitivity Meter
#'
#' @inheritParams flipStandardCharts::Line
#' @param x Input table containing survey responses in 4 columns
#'   At what price would you consider this product/brand to be 
#'   1) Very cheap, 2) Cheap, 3) Expensive, 4) Very expensive.
#' @param weights A numeric vector with length equal to the number of rows in \code{x}. They are applied
#'   whem computing the proportions of respondents for each question
#' @param resolution Numeric; controls the intervals (in terms of price) between which 
#'   "Proportion of respondents" is computed. If set to \code{NULL} (default), the proportion
#'   will be computed at the observed values, which may be irregularly spaced.
#' @param currency Character; Currency symbol to prepend to the intersection labels. These
#'   will also be used to set the default prefix to the x tick labels and hovertext.
#' @param intersection.show Logical; Whether to show labels to the intersection points of the lines.
#' @param intersection.arrow.color Color of the arrows to the intersection points.
#' @param intersection.arrow.size Size of the arrows to the intersection points.
#' @param intersection.arrow.width Width of the arrow heads.
#' @param intersection.arrow.length Scaling factor controlling length of arrows.
#' @param intersection.arrow.standoff Distance between arrowhead and intersection point.
#' @param intersection.label.font.color intersection.label font color as a named color in
#' character format (e.g. "black") or an a hex code.
#' @param intersection.label.font.family Character; intersection.label font family
#' @param intersection.label.font.size Integer; intersection label font size
#' @param intersection.label.wrap Logical; whether the intersection label text should be wrapped.
#' @param intersection.label.wrap.nchar Number of characters (approximately) in each
#' line of the intersection label when \code{intersection.label.wrap} \code{TRUE}.
#' @param font.units One of "px" of "pt". By default all font sizes are specified in terms of
#' pixels ("px"). But changing this to "pt" will mean that the font sizes will be in terms
#' points ("pt"), which will be consistent with font sizes in text boxes.
#' @param ... Other charting parameters passed to \code{\link[flipStandardCharts]{Line}}.
#' @importFrom grDevices rgb
#' @importFrom plotly layout config
#' @importFrom flipStandardCharts Line autoFormatLongLabels
#' @export

PriceSensitivityMeter <- function(x,
                                  weights = NULL,
                                  resolution = NULL,
                                  colors = c("#FF0000", "#FF0000", "#008000", "#008000"), 
                                  line.type = c("dot", "solid", "solid", "dot"),
                                  line.thickness = c(1, 2, 2, 1),
                                  currency = "$",
                                  global.font.family = "Arial",
                                  global.font.color = rgb(44, 44, 44, maxColorValue = 255),
                                  title.font.size = 16,
                                  subtitle.font.size = 12,
                                  footer.font.size = 8,
                                  legend.font.size = 10,
                                  hovertext.font.size = 11,
                                  y.title.font.size = 12,
                                  x.title.font.size = 12,
                                  y.tick.font.size = 10,
                                  x.tick.font.size = 10,
                                  data.label.font.size = 10,
                                  x.title = "Price",
                                  x.tick.prefix = currency,
                                  x.hovertext.format = ".2f",
                                  y.title = "Proportion of respondents",
                                  y.tick.format = "%",
                                  intersection.show = TRUE,
                                  intersection.arrow.color = global.font.color,
                                  intersection.arrow.size = 1.6,
                                  intersection.arrow.width = 0.7,
                                  intersection.arrow.length = 10,
                                  intersection.arrow.standoff = 3,
                                  intersection.label.font.family = global.font.family,
                                  intersection.label.font.color = global.font.color,
                                  intersection.label.font.size = 10,
                                  intersection.label.wrap = TRUE,
                                  intersection.label.wrap.nchar = 21,
                                  font.units = "px", 
                                  ...)
{
    x <- as.matrix(x)
    if (ncol(x) < 4)
        stop("Price sensitivity meter needs input data containing 4 columns: ",
             "'Very cheap', 'Cheap', 'Expensive', 'Very expensive'")
    if (length(weights) > 1 && any(is.na(weights)))
        stop("Weights contain missing values")
    
    # For the standard charts, the font size conversion happens inside flipChart::CChart
    if (tolower(font.units) %in% c("pt", "point", "points"))
    {
        fsc <- 1.3333
        title.font.size = round(fsc * title.font.size, 0)
        subtitle.font.size = round(fsc * subtitle.font.size, 0)
        footer.font.size = round(fsc * footer.font.size, 0)
        legend.font.size = round(fsc * legend.font.size, 0)
        hovertext.font.size = round(fsc * hovertext.font.size, 0)
        y.title.font.size = round(fsc * y.title.font.size, 0)
        x.title.font.size = round(fsc * x.title.font.size, 0)
        y.tick.font.size = round(fsc * y.tick.font.size, 0)
        x.tick.font.size = round(fsc * x.tick.font.size, 0)
        data.label.font.size = round(fsc * data.label.font.size, 0)
        intersection.label.font.size = round(fsc * intersection.label.font.size, 0)
    }
    rg <- range(x, na.rm = TRUE)
    xpts <- if (!is.null(resolution)) seq(from = rg[1], to = rg[2], by = resolution)
            else                      unique(sort(as.numeric(x)))    

    # Compute proportions - cannot use ecdf because we want '>=' not '>'
    psm.dat <- matrix(NA, nrow = length(xpts), ncol = 4,
                      dimnames = list(Price = xpts, c("Less than 'Very cheap'",
                                        "Less than 'Cheap'", "More than 'Expensive'", "More than 'Very expensive'")))
    psm.dat[,1] <- propLessorEqual(x[,1], xpts, weights)
    psm.dat[,2] <- propLessorEqual(x[,2], xpts, weights)
    psm.dat[,3] <- propGreatorEqual(x[,3], xpts, weights)
    psm.dat[,4] <- propGreatorEqual(x[,4], xpts, weights)
    
    pp <- Line(psm.dat, colors = colors, line.type = line.type, line.thickness = line.thickness,
               global.font.family = global.font.family, global.font.color = global.font.color,
               x.title = x.title, x.tick.prefix = x.tick.prefix, x.hovertext.format = x.hovertext.format,
               y.title = y.title, y.tick.format = y.tick.format, ...,
               title.font.size = title.font.size, subtitle.font.size = subtitle.font.size,
               footer.font.size = footer.font.size, legend.font.size = legend.font.size,
               hovertext.font.size = hovertext.font.size, data.label.font.size  = data.label.font.size,
               y.title.font.size = y.title.font.size, y.tick.font.size = y.tick.font.size,
               x.title.font.size = x.title.font.size, x.tick.font.size = x.tick.font.size)
    
    if (intersection.show)
    {
        intersect.pts <- matrix(NA, 4, 2)
        intersect.pts[1,] <- getIntersect(psm.dat[,1], psm.dat[,3], xpts)    
        intersect.pts[2,] <- getIntersect(psm.dat[,1], psm.dat[,4], xpts)    
        intersect.pts[3,] <- getIntersect(psm.dat[,2], psm.dat[,3], xpts)    
        intersect.pts[4,] <- getIntersect(psm.dat[,2], psm.dat[,4], xpts)    

        pp$htmlwidget <- layout(pp$htmlwidget,
                                annotations = list(xref = "x", yref = "y",
                                                   x = intersect.pts[,1],
                                                   y = intersect.pts[,2], 
                                                   arrowsize = intersection.arrow.size, arrowwidth = intersection.arrow.width,
                                                   arrowcolor = intersection.arrow.color, standoff = intersection.arrow.standoff,
                                                   axref = "pixel", ax = c(-10, 0, 0, 10) * intersection.arrow.length,
                                                   ayref = "pixel", ay = c(2, -5, 5, 2) * intersection.arrow.length,
                                                   font = list(family = intersection.label.font.family,
                                                               color = intersection.label.font.color, size = intersection.label.font.size),
                                                   text = autoFormatLongLabels(sprintf("%s %s%.2f", c("Point of marginal cheapness",
                                                                                                     "Optimal price point", "Indifference point price",
                                                                                                     "Point of marginal expensiveness"), 
                                                                               currency, intersect.pts[,1]), 
                                                                               wordwrap = intersection.label.wrap, intersection.label.wrap.nchar)))
        
        # allow labels to be movable - but turn off editing to other parts of the text
        pp$htmlwidget <- config(pp$htmlwidget, editable = TRUE, 
                                edits = list(annotationPosition = FALSE, annotationText = FALSE,
                                             axisTitleText = FALSE, titleText = FALSE))
    }
    attr(pp, "ChartData") <- psm.dat
    return(pp)
}

# Calculates proportion less than or equal to
# note that pts is sorted and contains all the values in vals
propLessorEqual <- function(vals, pts, wgts)
{
    if (length(wgts) == 0)
        wgts <- rep(1, length(vals))
    ord <- order(vals, na.last = NA)
    n <- length(ord)
    denom <- sum(wgts[ord])
    res <- rep(0, length(pts))
    
    j <- 1
    for (i in seq_along(pts))
    {
        while(j <= n && vals[ord[j]] <= pts[i])
        {
            if (!is.na(wgts[ord[j]]))
                res[i] <- res[i] + wgts[ord[j]]
            j <- j + 1
        }
    }
    res <- cumsum(res)/denom
}

# Calculates proportion more than or equal to
# note that pts is sorted and contains all the values in vals
propGreatorEqual <- function(vals, pts, wgts)
{
    if (length(wgts) == 0)
        wgts <- rep(1, length(vals))
    ord <- order(vals, decreasing = TRUE, na.last = NA)
    n <- length(ord)
    pts <- rev(pts)
    denom <- sum(wgts[ord])
    res <- rep(0, length(pts))

    j <- 1
    for (i in seq_along(pts))
    {
        while(j <= n && vals[ord[j]] >= pts[i])
        {
            res[i] <- res[i] + wgts[ord[j]]
            j <- j + 1
        }
    }
    res <- rev(cumsum(res)/denom)
}

getIntersect <- function(y1, y2, x)
{
    # Create function to interpolate
    tmp.f1 <- splinefun(x, y1)
    tmp.fd <- splinefun(x, y2 - y1)

    intersect <- uniroot(tmp.fd, range(x))
    x.pt <- intersect$root
    y.pt <- tmp.f1(x.pt)
    return(c(x.pt, y.pt))
}