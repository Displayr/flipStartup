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
    print(dim(total))
    if (remove.last){
        k <- nrow(total)
        ns <- ns[-k]
        total <- total[-k, -k]
    }
    total[Triangle(total, position = "lower right")] <- NA
    value <- sweep(total, 1, ns, "/")
    index <- Index(value, STATS = value[, 1], remove = "lower right", remove.diag = FALSE)
    cumulative <- t(apply(value, 1, cumsum))
    churn <- 1 - Retention(data)$estimated.volume.retention.by.year
    #print(Diagonal(value, off = TRUE))
    future.revenue <- Diagonal(value, off = TRUE)/ churn
    #future.revenue <- ns * future.revenue
    lifetime.revenue <- Diagonal(cumulative, off = TRUE) + future.revenue
    lifetime.revenue.per.customer <- sum(lifetime.revenue * prop.table(ns))
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
    {
        stop("'x' must be a 'LifetimeValue' object.")
    }
    x <- x$cumulative
    k <- nrow(x)
    dat <- data.frame(value = as.numeric(x), cohort = rownames(x), period = rep(colnames(x), rep(k, k)))
    p <- ggplot(dat, aes_string(x = "period", y = "value", group = "cohort")) +
         geom_line(aes_string(color = "cohort")) +
         scale_y_continuous(labels = dollar) +
         geom_point(aes_string(color = "cohort"))
    p
    #dat
    # cohort.names <- rownames(x)
    # names(dat) <- cohort.names
    # dat <- dat[, -k]
    # dat$period <- colnames(x)
    # p <- ggplot(dat, aes(x = period, y = cohort.names[1], group = 1))
    # p + geom_line()
    # p


    #plot(colnames(x), result[, 1], type = "l")
    #
}


# library(ggplot2)
# df <- data.frame(dose=c("D0.5", "D1", "D2"),
#                  len=c(4.2, 10, 29.5))
# ggplot(data=df, aes(x=dose, y=len, group=1)) +
#     geom_line()+
#     geom_point()
