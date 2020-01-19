#' Growth
#'
#' @description Sales and sales growth.
#' @param data A \code{data.frame} that has the same variables as a \code{RevenueData} object.
#' @param volume Weights the results by volume.
#' @param remove.last Remove the final period (as usually is incomplete).
#' @return A \code{\link{list}} containing the following elements:
#' \item{revenue}{The revenue by \code{period}}.
#' \item{growth}{The revenue growth, as a proportion, by \code{period}}.
#'
#' @export
Growth <- function(data, volume = FALSE, remove.last = TRUE)
{
    if (remove.last)
        data <- removeLast(data)
    revenue <- quantityByTime(data, volume, "from.period")
    k <- length(revenue)
    growth <- revenue[-1] / revenue[-k] - 1
    out <- list(revenue = revenue, growth = growth, volume = volume)
    class(out) <- c("Growth", class(out))
    out
}


aggregateAsVector <- function(x)
{
    #print(x)
    result <- x[, 2]
    names(result) <- x[, 1]
    result
}


#' @export
YLim.Growth <- function(x, ...)
{
    range(x$growth)
}

#' @export
plot.Growth <- function(x, ...)
{
    y.title <- if(x$volume) "Revenue Growth (%)" else "Customer Growth (%)"
    columnChart(x$growth, y.title = y.title, ...)
}

