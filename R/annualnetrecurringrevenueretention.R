#' \code{AnnualNetRecurringRevenueRetention}
#'
#' @description Computes recurring revenue, by cohort.
#' @param data A \code{data.frame} that has the same variables as a \code{RevenueData} object.
#' @param by The time period used in defining the cohorts: "year", "quarter", "month", "week", day".
#' @param ... Additional arguments to be passed to lower level functions.
#' @return A matrix
#' @importFrom flipTime AsDate Period
#' @importFrom flipStatistics Table
#' @importFrom lubridate floor_date years
#' @export
AnnualNetRecurringRevenueRetention <- function(data, by = "year", ...)
{
    
    start <- floor_date(attr(data, "start"), by)
    end <- floor_date(attr(data, "end"), by)
    dts <- seq.Date(start, end, by)
    n <- length(dts)
    revenue.retention <- rep(NA, n)
    names(revenue.retention) <- Period(dts, by)
    from <- data$from
    to <- data$to
    id <- data$id
    rr <- data$recurring.value
    for (i in seq_along(dts))
    {
        dt <- dts[i]
        dt.year.ago <- dt - years(1)
        invoice.year.ago <- from <= dt.year.ago & to >= dt.year.ago
        ids.year.ago <- unique(id[invoice.year.ago])
        invoice.this.year <- from <= dt & to >= dt & id %in% ids.year.ago
        print(c(sum(rr[invoice.this.year]),sum(rr[invoice.year.ago])))
        print(dt)
        revenue.retention[i] <- sum(rr[invoice.this.year]) / sum(rr[invoice.year.ago]) - 1
    }
    detail <- data[, c("from", "to", "id", "recurring.value")]
    revenue.retention <- addAttributesAndClass(revenue.retention, "AnnualNetRecurringRevenueRetention", by, detail)
    revenue.retention
}

#' @export
plot.AnnualNetRecurringRevenueRetention <- function(x, ...)
{
    smooth <- if (length(x) < 4) "None" else "Friedman's super smoother"
    columnChart(x, 
                fit.type = smooth,
                fit.ignore.last = TRUE,
                y.title = "Annual Net Recurring Revenue Retetion", 
                y.tick.format = "%", ...)
}

