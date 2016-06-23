#' \code{Churn}
#'
#' @description Computes retention, by cohort.
#' @param data A \code{data.frame} that has the same variables as a \code{RevenueData} object.
#' @param remove.last Remove the final period (as usually is incomplete).
#' @param volume Weights the results by volume.
#' @details Where subscribers suspends their purchasing for a period, but purchases again later, the subscriber
#' is asumed to have been retained during the period where the account was suspended.
#' @return A \code{\link{list}} containing the following elements:
#'   \item{id}{The \code{id} values of subscribers to churn.}
#'   \item{base}{The number of subscribers to renew or churn in the time period.}
#'   \item{counts}{A table where the first column contains the number of people not
#'   to churn in a period, and the second contains the number that have churned. Where \code{volume}
#'   is \code{TRUE} the data is volume-weighted}.
#'   \item{rates}{The percentage to churn (weighted if the counts are weighted)}.
#'
#' @export
Churn <- function(data, remove.last = TRUE, volume = FALSE)
{
    #filtering out data where there are no start dates.
    data <- data[data$to <= max(data$from), ]
    if (remove.last)
        data <- data[data$to.period < max(data$to.period), ]
    idag <- aggregate(id ~ to.period, data = data[data$churned,], FUN = unique)
    id <- idag[, 2]
    names(id) <- idag[, 1]
    counts <- if (volume)
        Table(value ~ churn + to.period, data = data, FUN = sum)
    else
        Table( ~ churn + to.period, data = data)
    base <- table(data$to.period)
    rates <- prop.table(counts, 2)[2, ]
    result <-list(volume = volume, id = id, base = base, counts = t(counts), rates = rates)
    class(result) <- c("Churn", class(result))
    result
}

#' @importFrom plotly plot_ly
#' @export
plot.Churn <- function(x, ...)
{
    rates <- x$rates * 100
    base <- x$base[!is.na(rates)]
    rates <- rates[!is.na(rates)]
    period.names <- names(rates)
    p <- plot_ly(
        x = period.names,
        y = rates,
        name = "Churn",
        type = "bar")
    x.dates <- names(rates)
    n.dashes <- nchar(x.dates[1]) - nchar(gsub("-", "", x.dates[1]))
    if (n.dashes == 0) # yearly data
        x.dates <- as.Date(paste0(period.names,"-07-01"))
    else if (n.dashes == 1) # montly or quarterly
        x.dates <- as.Date(paste0(period.names,"-15"))
    else
        x.dates <- as.Date(period.names)
    y.fitted <- supsmu(x.dates, rates)$y
    p <- add_trace(p,
        x = period.names,
        y = y.fitted,
        #marker = list(color = colors[i]),
        name = "Fitted", #categories[eval(i)],
        type = "line")
    p <- layout(p,
           xaxis = list(title = "",
                        range = range(x.dates),# + c(-.5, .5)) else NULL
                        showgrid = FALSE),
           yaxis = list(title = if(x$volume) "Churn (% volume)" else "Churn (% subscribers)"))
    p
}

