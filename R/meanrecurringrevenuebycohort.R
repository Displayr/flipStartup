#' \code{MeanRecurringRevenueByCohort}
#'
#' @description Computes recurring revenue, by cohort.
#' @param data A \code{data.frame} that has the same variables as a \code{RevenueData} object.
#' @param cohort.by The time period used in defining the cohorts: "year", "quarter", "month", "week", day".
#' @param period.by The time period to show value by 
#' @return A matrix
#' @importFrom flipTime AsDate Period
#' @importFrom flipStatistics Table
#' @export
MeanRecurringRevenueByCohort <- function(data, cohort.by = "year")
{
    data$cohort <- Period(data$subscriber.from, cohort.by)
    period.by <- attr(data, "subscription.length")
    data$period <- Period(data$from, period.by)
    start <- attr(data, "start")
    counts <- Table(id ~ cohort + period.counter, data = data, FUN = nUnique)
    value <- Table(recurring.value ~ cohort + period.counter, data = data, FUN = sum)
    out <- value / counts
    attr(out, "cohort.by") = cohort.by
    attr(out, "subscription.length") = period.by
    attr(out, "detail") <- data[, c("cohort", "period", "id", "recurring.value")]
    class(out) <- c("MeanRecurringRevenueByCohort", class(out))
    out
}



#' \code{MeanRecurringRevenueByYearCohort}
#'
#' @description Computes recurring revenue, by cohort.
#' @param data A \code{data.frame} that has the same variables as a \code{RevenueData} object.
#' @param volume Does nothing.
#' @return A matrix
#' @export
MeanRecurringRevenueByYearCohort <- function(data, volume = FALSE)
{
    MeanRecurringRevenueByCohort(data, cohort.by = "year")
}        


#' \code{MeanRecurringRevenueByQuarterCohort}
#'
#' @description Computes recurring revenue, by cohort.
#' @param data A \code{data.frame} that has the same variables as a \code{RevenueData} object.
#' @param volume Does nothing.
#' @return A matrix
#' @export
MeanRecurringRevenueByQuarterCohort <- function(data, volume = FALSE)
{
    MeanRecurringRevenueByCohort(data, cohort.by = "quarter")
}        

#' \code{MeanRecurringRevenueByMonthCohort}
#'
#' @description Computes recurring revenue, by cohort.
#' @param data A \code{data.frame} that has the same variables as a \code{RevenueData} object.
#' @param volume Does nothing.
#' @return A matrix
#' @export
MeanRecurringRevenueByMonthCohort <- function(data, volume = FALSE)
{
    MeanRecurringRevenueByCohort(data, cohort.by = "month")
}        



#' @export
print.MeanRecurringRevenueByCohort <- function(x, ...)
{
    names(x) <- c("Customer since", attr(x, "subscription.length"))
    attr(x, "subscription.length") <- NULL
    attr(x, "n.subscriptions") <- NULL
    attr(x, "detail") <- NULL
    class(x) <- class(x)[-1]
    print(x)
}


#' @importFrom plotly plot_ly layout `%>%`
#' @importFrom flipFormat FormatAsPercent
#' @export
plot.MeanRecurringRevenueByCohort <- function(x, ...)
{
    heatmap(x, 
            row.title = "Commenced",
            column.title = properCase(attr(x, "subscription.length")), 
            series.hover = paste0("Mean deal size: $", FormatAsReal(x, decimals = 0)), 
            base = c(attr(x, "n.subscriptions")))
        
}


#' \code{heatmap}
#'
#' @description Internal heatmap function.
#' @param x A \code{matrix} 
#' @param row.title The row title; used in the hover tooltips. 
#' @param column.title The column title; used in the hover tooltips. 
#' @param series.hover The description of the value that appears in each cell;
#' used in the hover tooltips 
#' @param base The sample size; used in the hover tooltips 
#' @return A plotly heatmap
heatmap <- function(x, row.title, column.title, series.hover, base, ...)
{
    rn <- matrix(rownames(x), nrow(x), ncol(x), byrow = FALSE)
    cn <- matrix(colnames(x), nrow(x), ncol(x), byrow = TRUE)
    hover.text <- matrix(paste0(row.title, ": ", rn, "<br>",
                                column.title, ": <br>", cn, "<br>",
                                series.hover, "<br>",
                                "Base: ", base), nrow(x))
    plot_ly(x = colnames(x),
            y = rownames(x),
            z = x, 
            colors = colorRamp(max(x), list(...)$y.max), 
            text = hover.text,
            hoverinfo = "text",
            type = "heatmap", 
            showscale = FALSE) %>% config(displayModeBar = FALSE)
    
}