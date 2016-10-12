#' Growth
#'
#' @description Sales and sales growth.
#' @param data A \code{data.frame} that has the same variables as a \code{RevenueData} object.
#' @param remove.last Remove the final period (as usually is incomplete).
#' @return A \code{\link{list}} containing the following elements:
#' \item{revenue}{The revenue by \code{period}}.
#' \item{growth}{The revenue growth, as a proportion, by \code{period}}.
#'
#' @importFrom flipStatistics Table
#' @export
Growth <- function(data, remove.last = TRUE)
{
    revenue <- Table(value ~ period, data = data, FUN = sum)
    k <- length(revenue)
    if (remove.last)
    {
        revenue <- revenue[-k]
        k <- k - 1

    }
    growth <- revenue[-1] / revenue[-k] - 1
    list(revenue = revenue, growth = growth)
}

aggregateAsVector <- function(x)
{
    #print(x)
    result <- x[, 2]
    names(result) <- x[, 1]
    result
}
