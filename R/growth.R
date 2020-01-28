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
#' @importFrom flipTime AsDate
#' @export
Growth <- function(data, volume = FALSE, remove.last = TRUE)
{
    start <- attr(data, "start")
    revenue <- quantityByTime(data, volume, "from.renewal.period")
    k <- length(revenue)
    growth <- revenue[-1] / revenue[-k] - 1
    if (remove.last)
        growth <- growth[-length(growth)]
    class(growth) <- c("Growth", class(out))
    growth <- growth[AsDate(names(growth)) >= start]
    attr(growth, "volume") <- volume
    class(growth) <- c("Growth", class(growth))
    growth
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
    range(x)
}

#' @export
plot.Growth <- function(x, ...)
{
    y.title <- if(attr(x, "volume")) "Revenue Growth (%)" else "Customer Growth (%)"
    columnChart(x,  y.tick.format= "%", y.title = y.title, ...)
}

