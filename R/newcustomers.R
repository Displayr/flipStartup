#' \code{NewCustomers}
#'
#' @description Computes NewCustomers, by cohort.
#' @param data A \code{data.frame} that has the same variables as a \code{RevenueData} object.
#' @param by The time period to aggregate the dates by: 
#' \code{"year"}, \code{"quarter"}, \code{"month"}, \code{"week"}, 
#' and \code{"day"}.
#' @return A \code{\link{list}} containing the following elements:
#'   \item{id}{The \code{id} values of subscribers to churn.}
#'   \item{base}{The number of subscribers to renew or churn in the time period.}
#'   \item{counts}{Number of NewCustomerss by period (or in $ if \code{volume} is \code{TRUE}}.
#'   \item{rates}{The percentage to churn (weighted if the counts are weighted)}.
#'
#' @importFrom flipStatistics Table
#' @importFrom flipTime Period
#' @export
NewCustomers <- function(data, by = "quarter", ...)
{
    data$subscriber.from.period <- Period(data$subscriber.from, by)
    x <- quantityByTime(data, FALSE, "subscriber.from.period", by)
    detail <- idByPeriod(data, "subscriber.from.period")#data[, "subscriber.from", "id"]#sapply(id, paste, collapse = ", ")
    addAttributesAndClass(x, "NewCustomers", by, detail)
}

print.NewCustomers <- function(x, ...)
{
    printWithoutAttributes(x, ...)
}



idByPeriod <- function(data, time)
{
    data$time <- data[, time]
    if (nrow(data) == 0)
        return(NULL)
    idag <- aggregate(id ~ time, data = data, FUN = unique)
    id <- idag[, 2]
    names(id) <- idag[, 1]
    id
}   

#' @importFrom flipStatistics Table
quantityByTime <- function(data, volume, time, by)
{
    form <- if (volume) paste0("value ~ ", time) else paste0("id ~ ", time)
    func <- if (volume) sum else unique
    t <- Table(as.formula(formula), data = data, FUN = func) 
    FillInDateVector(t, by)
}


#' @export
plot.NewCustomers <- function(x, ...)
{
    columnChart(x, y.title = "New customers", ...)
}
