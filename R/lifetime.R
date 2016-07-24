#' \code{LifetimeValue}
#'
#' @description The value that subscribers have historically provided the firm. This is sometimes
#' referred to as "lifetime value". \code{LifetimeValue} estimates the value of a subscriber
#' based on both historical value and likely future value.
#' @param data A \code{RevenueData} object.
#' @param remove.last Remove the final period (as usually is incomplete).
#' @return A \code{list} containing the following elements:
#' \item{total}{The total value of transactions, by \code{start.period} and \code{period.counter}.}
#' \item{mean}{The average value of transactions, where the base is the number of subscribers
#' in the \code{start.period}, by \code{start.period} and \code{period.counter}.}
#' \item{cumulative}{The cumulative means.}
#' \item{index}{The \code{cumulative} means divided by the mean from the first period.}
#'
#' @export
LifetimeValue <- function(data, remove.last = TRUE)
{
    ns <- Table(id ~ start.period, data = data, FUN = function(x) length(unique(x)))
    total <- Table(value ~ start.period + period.counter, data, sum)
    if (remove.last){
        k <- nrow(total)
        ns <- ns[-k]
        total <- total[-k, -k]
    }
    total[Triangle(total, position = "lower right")] <- NA
    names(dimnames(total)) <- c("Commenced","Year")
    value <- sweep(total, 1, ns, "/")
    index <- Index(value, STATS = value[, 1], remove = "lower right", remove.diag = FALSE)
    cumulative <- t(apply(value, 1, cumsum))
    churn <- 1 - Retention(data)$estimated.volume.retention.by.year
    #print(Diagonal(value, off = TRUE))
    future.revenue <- Diagonal(value, off = TRUE)/ churn
    #future.revenue <- ns * future.revenue
    lifetime.revenue <- Diagonal(cumulative, off = TRUE) + future.revenue
    lifetime.revenue.per.customer <- sum(lifetime.revenue * prop.table(ns), na.rm = TRUE)
    result <- list(total = total,
                   mean = value,
                   cumulative = cumulative,
                   index = index,
                   lifetime.revenue = lifetime.revenue,
                   lifetime.revenue.per.customer = lifetime.revenue.per.customer)
    class(result) <- c("LifetimeValue", class(result))
    result
}

#' CumulativeValuePlot
#'
#' Plots the cumulative value over time.
#' @param x A \code{LifetimeValue} object.
#' @import ggplot2
#' @importFrom scales dollar
#' @export
CumulativeValuePlot <- function(x)
{
    if (!is(x, "LifetimeValue"))
        stop("'x' must be a 'LifetimeValue' object.")
    #x <- x$cumulative
    #k <- nrow(x)
    # dat <- data.frame(Cumulative = as.numeric(x), Commenced = rownames(x), Year = rep(colnames(x), rep(k, k)))
    # dat <- dat[!is.na(data$Value), ]
    # print(dat)
    # p <- ggplot(dat, aes_string(x = "Year", y = "Cumulative", group = "Commenced")) +
    #      geom_line(aes_string(color = "Commenced")) +
    #      scale_y_continuous(labels = dollar) +
    #      geom_point(aes_string(color = "Commenced"))
    # p
}

