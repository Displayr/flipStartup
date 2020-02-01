#' \code{ChurnByCohort}
#'
#' @description Computes the churn by time period and time-based cohort
#' @param data A \code{data.frame} that has the same variables as a \code{RevenueData} object.
#' @param remove.last Remove the final period (as usually is incomplete).
#' @param volume Weights the results by volume. Does nothing in this case.
#' @param by The time unit to plot. E.g., "month".
#' @param ... Other arguments.
#' @return A \code{\link{matrix}} 
#' @export
ChurnByCohort <- function(data, remove.last = FALSE, volume = FALSE, by, ...)
{
    retention <- Retention(data, by)
    x <- 1 - retention[[ if (volume) "retention.rate.volume" else "retention.rate"]]
    if (remove.last)
        x <- x[-nrow(x), -ncol(x)]
    x <- addAttributesAndClass(x, "ChurnByCohort", by, retention$detail)
    attr(x, "volume") <- volume
    x
}

#' \code{CustomerChurnByCohort}
#'
#' @description Computes the customer churn by time period and time-based cohort
#' @inherit ChurnByCohort
#' @export
CustomerChurnByCohort <- function(data, remove.last = FALSE, by, ...)
{
    ChurnByCohort(data, remove.last, volume = FALSE, by, ...)
}

#' \code{RecurringRevenueChurnByCohort}
#'
#' @description Computes the recurring revenue-weighted customer churn by time period and time-based cohort
#' @inherit ChurnByCohort
#' @export
RecurringRevenueChurnByCohort <- function(data, remove.last = FALSE, by, ...)
{
    ChurnByCohort(data, remove.last, volume = TRUE, by, ...)
}

#' @export
print.ChurnByCohort <- function(x, ...)
{
    printWithoutAttributes(x)
}


#' @importFrom plotly plot_ly layout `%>%`
#' @importFrom flipFormat FormatAsPercent
#' @export
plot.ChurnByCohort <- function(x, ...)
{
    by <- properCase(attr(x, "by"))
    churn.type <- if(attr(x, "volume")) "Recurring Revenue " else "Customer "
    n <- c(attr(x, "n.subscriptions"))
    hover.text <- matrix(paste0("Commenced: ", rownames(x), "<br>",
                        by, ": <br>", colnames(x), "<br>",
                        churn.type, "Churn Rate: ", FormatAsPercent(x, decimals = 1), "<br>",
                        "Base: ", n), nrow(x))#, "<extra></extra>")
    plot_ly(
        x = colnames(x),
        y = rownames(x),
        z = x, 
        colors = colorRamp(max(x), list(...)$y.max), 
        text = hover.text,
        hoverinfo = "text",
        type = "heatmap", 
        showscale = FALSE
    ) %>% config(displayModeBar = FALSE)
}
