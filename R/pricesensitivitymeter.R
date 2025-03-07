#' Plots the Van Westendorp Price Sensitivity Meter
#'
#' @inheritParams flipStandardCharts::Line
#' @param x Input table containing survey responses in 4 columns
#'   At what price would you consider this product/brand to be
#'   1) Very cheap, 2) Cheap, 3) Expensive, 4) Very expensive.
#' @param output Type of data to show in the chart. One of "Attitude of respondents",
#'   "Likelihood to buy" and "Revenue".
#' @param check.prices.ordered Check that prices supplied in the first 4 columns of \code{x}
#'  are supplied in increasing order. For backwards compatibility this is off by default.
#' @param likelihood.scale Used in NSM calculation to convert likelihood scale to probabiliy.
#'   Default scale assumes a 7 point scale.
#' @param weights A numeric vector with length equal to the number of rows in \code{x}.
#'   They are applied whem computing the proportions of respondents for each question
#' @param resolution Numeric; controls the intervals (in terms of price) between which
#'   "Proportion of respondents" is computed. For example, set to \code{0.1}, to
#'   evaluate proportions every 10 cents. By default, we use observed values.
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
#' @param intersection.label.decimals Integer; number of decimals to show on intersection label.
#' @param intersection.label.wrap Logical; whether the intersection label text should be wrapped.
#' @param intersection.label.wrap.nchar Number of characters (approximately) in each
#' line of the intersection label when \code{intersection.label.wrap} \code{TRUE}.
#' @param font.units One of "px" of "pt". By default all font sizes are specified in terms of
#' pixels ("px"). But changing this to "pt" will mean that the font sizes will be in terms
#' points ("pt"), which will be consistent with font sizes in text boxes.
#' @param ... Other charting parameters passed to \code{\link[flipStandardCharts]{Line}}.
#' @importFrom grDevices rgb
#' @importFrom plotly layout config add_trace
#' @importFrom flipStandardCharts Line autoFormatLongLabels
#' @importFrom flipU ConvertCommaSeparatedStringToVector StopForUserError
#' @importFrom verbs Sum SumEmptyHandling
#' @export

PriceSensitivityMeter <- function(x,
                                  check.prices.ordered = FALSE,
                                  weights = NULL,
                                  likelihood.scale = c(0.0, 0.1, 0.3, 0.5, 0.7),
                                  output = c("Attitude of respondents", "Likelihood to buy", "Revenue",
                                    "Likelihood to buy and Revenue")[1],
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
                                  hovertext.font.family = global.font.family,
                                  y.title.font.family = global.font.family,
                                  y.title.font.color = global.font.color,
                                  y.tick.font.family = global.font.family,
                                  y.tick.font.color = global.font.color,
                                  x.title = "Price",
                                  x.tick.prefix = currency,
                                  x.hovertext.format = ".2f",
                                  y.title = "",
                                  y.tick.format = "",
                                  intersection.show = TRUE,
                                  intersection.arrow.color = global.font.color,
                                  intersection.arrow.size = 0,
                                  intersection.arrow.width = 0.7,
                                  intersection.arrow.length = 10,
                                  intersection.arrow.standoff = 0,
                                  intersection.label.font.family = global.font.family,
                                  intersection.label.font.color = global.font.color,
                                  intersection.label.font.size = 10,
                                  intersection.label.decimals = 2,
                                  intersection.label.wrap = TRUE,
                                  intersection.label.wrap.nchar = 21,
                                  font.units = "px",
                                  ...)
{
    x <- as.matrix(x)
    if (output != "Attitude of respondents" && SumEmptyHandling(apply(x, 2, function(xx) any(!is.na(xx))), remove.missing = FALSE) < 6)
        StopForUserError("Data input must include price considered 'Too cheap', 'Cheap', 'Expensive', 'Too expensive' ",
                         "and likehood of buying when the price is 'Cheap' and 'Expensive'.")
    ind <- which(x < 0)
    if (length(ind) > 0)
    {
        warning("Negative prices have been ignored")
        x[ind] <- NA
    }
    if (ncol(x) < 4)
        StopForUserError("Price sensitivity meter needs input data containing 4 columns: ",
                         "'Very cheap', 'Cheap', 'Expensive', 'Very expensive'")
    if (length(weights) > 1 && length(weights) != nrow(x))
        StopForUserError("Weights should be the same length as the number of respondents.")
    if (length(weights) > 1 && any(is.na(weights)))
        StopForUserError("Weights contain missing values")
    ind.invalid <- which(x[,4] < x[,3] | x[,3] < x[,2] | x[,2] < x[,1])
    if (check.prices.ordered && length(ind.invalid) > 0)
    {
        if (length(ind.invalid) == NROW(x))
            StopForUserError("No data remaining after invalid observations were ignored.")
        warning(length(ind.invalid), " observations were not valid and ignored. ",
            "Prices for each respondent should be supplied in increasing order.")
        x <- x[-ind.invalid,]
        if (!is.null(weights))
            weights <- weights[-ind.invalid]
    }
    if (length(weights) > 1)
        weights <- weights/Sum(weights) * nrow(x)
    if (output != "Attitude of respondents" && !any(apply(x, 1, function(xx) SumEmptyHandling(!is.na(xx)) == 6)))
        StopForUserError("Data must include at least one valid observation containing all 6 values: ",
                         "Prices considered 'Too cheap', 'Cheap', 'Expensive', 'Too expensive' ",
                         "and likehood of buying when the price is 'Cheap' and 'Expensive'.")

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

    # Determine x-positions (price) to calculate proportions
    rg.raw <- range(x, na.rm = TRUE)
    if (is.null(resolution))
        xpts <- sort(unique(round(as.numeric(x), 2)))
    else
        xpts <- seq(from = rg.raw[1], to = rg.raw[2], by = resolution)

    # Compute proportions - cannot use ecdf because we want '>=' not '>'
    psm.dat <- matrix(NA, nrow = length(xpts), ncol = 4,
                      dimnames = list(Price = xpts, c("Too cheap", "Cheap",
                                        "Expensive", "Too expensive")))
    psm.dat[,1] <- propGreatorEqual(x[,1], xpts, weights)
    psm.dat[,2] <- propGreatorEqual(x[,2], xpts, weights)
    psm.dat[,3] <- propLessorEqual(x[,3], xpts, weights)
    psm.dat[,4] <- propLessorEqual(x[,4], xpts, weights)


    # NSM extension
    if (output != "Attitude of respondents" && ncol(x) >= 6 && !all(is.na(x[,5:6])))
    {
        max.likelihood.score <- max(x[,5:6], na.rm = TRUE)
        if (!is.numeric(likelihood.scale))
        {
            likelihood.scale <- suppressWarnings(as.numeric(ConvertCommaSeparatedStringToVector(likelihood.scale)))
            if (any(is.na(likelihood.scale)))
                StopForUserError("Likelhood scale is not valid")
            if (any(likelihood.scale < 0) || any(likelihood.scale > 1))
                StopForUserError("Likelihood scale should consist of values between 0 and 1.")
        }
        l.vals <- 1:length(likelihood.scale)
        if (any(!x[,5:6] %in% l.vals & !is.na(x[,5:6])))
            StopForUserError("Likelihood scores should consist of values in [", paste(l.vals, collapse=","), "].")

        nsmat <- matrix(NA, nrow(x), length(xpts))
        for (i in 1:nrow(x))
            nsmat[i,] <- interpolate_prob(x[i,], xpts, likelihood.scale,
                            if (is.null(weights)) 1 else weights[i])
        trial <- apply(nsmat, 2, mean, na.rm = TRUE)
        revenue <- xpts * trial
        psm.dat <- cbind(psm.dat, 'Likelihood to buy' = trial, 'Revenue' = revenue)
    }

    intersect.pts <- NULL
    intersect.label.format <- "%.0f%%"
    if (grepl("Likelihood", output, fixed = TRUE))
    {
        if (!any(nzchar(y.title)))
            y.title <- "Likelihood to buy"
        if (!any(nzchar(y.tick.format)))
            y.tick.format <- "%"
        plot.data <- psm.dat[,5,drop = FALSE]

        if (intersection.show)
        {
            ind.max.trial <- which.max(trial)
            intersect.pts <- matrix(c(xpts[ind.max.trial], trial[ind.max.trial]), 1, 2,
                dimnames = list("Optimal price", c("X", "Y")))
            intersect.ax <- 10 * intersection.arrow.length
            intersect.ay <- -2 * intersection.arrow.length
            intersect.label.format <- "%.0f%%"
        }

    } else if (output == "Revenue")
    {
        if (!any(nzchar(y.title)))
            y.title <- "Revenue"
        if (!any(nzchar(y.tick.format)))
            y.tick.format <- "$.2f"
        plot.data <- psm.dat[,6,drop = FALSE]

        if (intersection.show)
        {
            ind.max.revenue <- which.max(revenue)
            intersect.pts <- matrix(c(xpts[ind.max.revenue], revenue[ind.max.revenue]), 1, 2,
                dimnames = list("Optimal price", c("X", "Y")))
            intersect.ax <- 10 * intersection.arrow.length
            intersect.ay <- -2 * intersection.arrow.length
            intersect.label.format <- "$%.2f"
        }
    } else
    {
        if (!any(nzchar(y.title)))
            y.title <- "Proportion of respondents"
        if (!any(nzchar(y.tick.format)))
            y.tick.format <- "%"
        plot.data <- psm.dat[,1:4,drop = FALSE]

        if (intersection.show)
        {
            intersect.pts <- matrix(NA, 4, 2)
            intersect.pts[1,] <- getIntersect(psm.dat[,3], psm.dat[,1], xpts)
            intersect.pts[2,] <- getIntersect(psm.dat[,4], psm.dat[,1], xpts)
            intersect.pts[3,] <- getIntersect(psm.dat[,3], psm.dat[,2], xpts)
            intersect.pts[4,] <- getIntersect(psm.dat[,4], psm.dat[,2], xpts)
            rownames(intersect.pts) <- c("Point of marginal cheapness", "Optimal price point",
                                       "Indifference price point", "Point of marginal expensiveness")
            intersect.ax <- c(-10, 0, 0, 10) * intersection.arrow.length
            intersect.ay <- c(2, -5, -5, 2) * intersection.arrow.length
            ind.na <- which(is.na(intersect.pts[,1]) | is.na(intersect.pts[,2]))
            if (length(ind.na) > 0)
            {
                intersect.pts <- intersect.pts[-ind.na,, drop = FALSE]
                intersect.ax <- intersect.ax[-ind.na]
                intersect.ay <- intersect.ay[-ind.na]
                intersect.label.format <- "%.0f%%"
            }
        }
    }

    pp <- Line(plot.data, colors = colors,
               line.type = line.type, line.thickness = line.thickness,
               global.font.family = global.font.family, global.font.color = global.font.color,
               x.title = x.title, x.tick.prefix = x.tick.prefix, x.hovertext.format = x.hovertext.format,
               y.title = y.title, y.tick.format = y.tick.format, ...,
               title.font.size = title.font.size, subtitle.font.size = subtitle.font.size,
               footer.font.size = footer.font.size, legend.font.size = legend.font.size,
               hovertext.font.size = hovertext.font.size, data.label.font.size  = data.label.font.size,
               y.title.font.size = y.title.font.size, y.tick.font.size = y.tick.font.size,
               x.title.font.size = x.title.font.size, x.tick.font.size = x.tick.font.size,
               hovertext.font.family = hovertext.font.family, y.title.font.family = y.title.font.family,
               y.tick.font.family = y.tick.font.family, y.title.font.color = y.title.font.color,
               y.tick.font.color = y.tick.font.color)

    if (output == "Likelihood to buy and Revenue")
    {
        pp$htmlwidget <- add_trace(pp$htmlwidget, x = xpts, y = psm.dat[,6], yaxis = "y2",
            type = "scatter", mode = "lines", cliponaxis = FALSE, name = "Revenue",
            line = list(color = colors[2], width = line.thickness[2], dash = line.type[2]),
            hoverlabel = list(font = list(color = flipStandardCharts:::autoFontColor(colors[2]),
            size = hovertext.font.size, family = hovertext.font.family)))
        pp$htmlwidget <- layout(pp$htmlwidget,
            yaxis2 = list(side = "right", anchor = "y", range = c(0, 1.1 * max(revenue)),
                title = list(text = "Revenue", font = list(family = y.title.font.family,
                color = y.title.font.color, size = y.title.font.size), standoff = 20),
                tickformat = "$.2f", tickfont = list(family = y.tick.font.family,
                color = y.tick.font.family, size = y.tick.font.size),
                gridcolor = "transparent", layer = "below axis"), margin = list(r = 80))

        if (intersection.show)
        {
            ind.max.revenue <- which.max(revenue)
            intersect.pts <- rbind(intersect.pts, c(xpts[ind.max.revenue], revenue[ind.max.revenue]))
            rownames(intersect.pts) <- c("Price to maximise trial", "Price to maximise revenue")
            intersect.ax <- c(-10, 10) * intersection.arrow.length
            intersect.ay <- c(0,0) * intersection.arrow.length
            intersect.label.format <- c("%.0f%%")
        }
    }

    if (NROW(intersect.pts) > 0)
    {
        annot <- list()
        for (i in 1:NROW(intersect.pts))
        {
            tmp.font.color <- intersection.label.font.color
            if (output == "Likelihood to buy and Revenue")
                tmp.font.color <- colors[i]
            if (output == "Revenue" || (output == "Likelihood to buy and Revenue" && i == 2))
                tmp.ylab <- sprintf("$%.2f", intersect.pts[i,2])
            else
                tmp.ylab <- sprintf("%.0f%%", intersect.pts[i,2] * 100)

            annot[[i]] = list(xref = "x",
                            yref = if (output == "Likelihood to buy and Revenue" && i == 2) "y2" else "y",
                            x = intersect.pts[i,1], y = intersect.pts[i,2],
                            arrowsize = intersection.arrow.size, arrowwidth = intersection.arrow.width,
                            arrowcolor = intersection.arrow.color, standoff = intersection.arrow.standoff,
                            axref = "pixel", ax = intersect.ax[i],
                            ayref = "pixel", ay = intersect.ay[i],
                            font = list(family = intersection.label.font.family,
                            color = tmp.font.color, size = intersection.label.font.size),
                            text = autoFormatLongLabels(sprintf(paste0("%s %s%.",
                            intersection.label.decimals, "f", " (%s)"),
                            rownames(intersect.pts)[i], currency, intersect.pts[i,1], tmp.ylab),
                            wordwrap = intersection.label.wrap, intersection.label.wrap.nchar))
        }
        pp$htmlwidget <- layout(pp$htmlwidget, annotations = annot)
    }

    # allow labels to be movable - but turn off editing to other parts of the text
    pp$htmlwidget <- config(pp$htmlwidget, editable = TRUE,
                            edits = list(annotationPosition = FALSE, annotationText = FALSE,
                                         axisTitleText = FALSE, titleText = FALSE, legendText = FALSE))
    attr(pp, "ChartData") <- psm.dat
    return(pp)
}

# Calculates proportion less than or equal to
# note that pts is sorted and contains all the values in vals
#' @importFrom verbs Sum
propLessorEqual <- function(vals, pts, wgts)
{
    if (length(wgts) == 0)
        wgts <- rep(1, length(vals))
    ord <- order(vals, na.last = NA)
    n <- length(ord)
    denom <- Sum(wgts[ord], remove.missing = FALSE)
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
#' @importFrom verbs Sum
propGreatorEqual <- function(vals, pts, wgts)
{
    if (length(wgts) == 0)
        wgts <- rep(1, length(vals))
    ord <- order(vals, decreasing = TRUE, na.last = NA)
    n <- length(ord)
    pts <- rev(pts)
    denom <- Sum(wgts[ord], remove.missing = FALSE)
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

getIntersect <- function(y1, y2, x, y.min = 0, y.max = 1.0)
{
    if (!any(is.finite(y1)) || !any(is.finite(y2)) || !any(is.finite(x)))
        return(c(NA, NA))

    # We assume that curves start with y2 > y1 and end with y1 < y2
    diff <- y2 - y1
    ind0 <- max(which(diff >= 0))
    ind1 <- min(which(diff <= 0))
    if (diff[ind0] == 0)
        return(c(x[ind0], y1[ind0]))

    r <- diff[ind0]/(diff[ind0] - diff[ind1])
    x.delta <- r * (x[ind1] - x[ind0])
    x.mid <- x[ind0] + x.delta
    y.mid <- y1[ind0] + (y1[ind1] - y1[ind0])/(x[ind1] - x[ind0]) * x.delta
    return(c(x.mid, y.mid))
}


# For each respondent, interpolates the probability of buying
# at each value along the x-axis
interpolate_prob <- function(xx, prices, sc, ww)
{
    tmp.prob <- c(0, sc[xx[5]], sc[xx[6]], 0)
    tmp.price <- xx[1:4]
    if (any(is.na(xx)))
        return(rep(NA, length(prices)))

    # linear interpolation - applied piecewise
    mm <- diff(tmp.prob)/diff(tmp.price)
    .interpseg <- function(pp)
    {
        if (pp <= tmp.price[1])
            return(0)
        else if (pp <= tmp.price[2])
            return((mm[1] * (pp - tmp.price[1]) + tmp.prob[1]) * ww)
        else if (pp <= tmp.price[3])
            return((mm[2] * (pp - tmp.price[2]) + tmp.prob[2]) * ww)
        else if (pp <= tmp.price[4])
            return((mm[3] * (pp - tmp.price[3]) + tmp.prob[3]) * ww)
        else
            return(0)
    }
    return(sapply(prices, .interpseg))
}
