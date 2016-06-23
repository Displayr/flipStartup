#' \code{RevenueData}
#'
#' @description Computes the  statistics for use in growth accounting of a startup.
#' @param value A vector of containing the revenue per transaction.
#' @param from A vector of class \code{POSIXct} or \code{POSIXt}, recording the date
#' and time each subscription commences.
#' @param to A vector of class \code{POSIXct} or \code{POSIXt}, recording the date
#' and time each subscription ends
#' @param begin The date at which the analysis outputs should commence. By default,
#' the earliest date recorded in \code{from}.
#' @param end The date at which the analysis ends, which is used to determine churn.
#' By default, the most recent  date recorded in \code{from}.
#' @param id A vector of \code{character}, unique identifier for subscribers that
#' made the transactions (e.g., email addresses, names, subscriber keys).
#' @param by \code{year} to view the data by year, \code{quarter}, and \code{month}. This is assumed to be the billing period
#' when determining if subscribers have churned or not.
#' @param subset An optional vector specifying a subset of observations to be used in the calculations
#' @param profiling A \code{data.frame} containing data, unique by \code{id}, to be included in the final \code{data.frame}.
#' Either it must contain the unique identifiers in a variable called \code{id}, or, the  \code{rownames} must match
#' the values of \code{id}.
#' @param trim.id The maximum length of the strings to be used showing ID names (used to avoid situations where
#' string names are so long as to make reading of tables impossible.
#' @return A \code{\link{data.frame}} containing the following variables, along with any other variables in the \code{data}:
#'   \code{id}{The unique identifier.}
#'   \code{value}{The value of the transaction.}
#'   \code{from}{The commencement date of the subscription.}
#'   \code{to}{The end-date of the subscription.}
#'   \code{start} The date of first subscription.
#'   \code{last.from} The \code{from} date of the most recent subscription.
#'   \code{last.from.period} The \code{period} of the \code{last.from} date.
#'   \code{end} The date of the end of the most recent subscription.
#'   \code{start.period} The \code{period} of the beginning of the first subscription.
#'   \code{last.period} The \code{period} of the end of the most recent subscription.
#'   \code{churned} A \code{logical} indicating if the subscriber had ever ceased subscribing.
#'   \code{churned} A \code{logical} indicating if the subscriber had ceased subscribing in that period.
#'   \code{tenure.interval} A \code{interval} of \code{start} to \code{end}.
#'   \code{tenure} The number of whole periods from the begining of the first subscription
#'   to the end of the most recent.
#'   \code{period} The \code{period} of the subscription.
#'   \code{period.counter} The number of the period, where 0 indicates the initial period.
#'   \code{observation} The number of the subscription, starting from 1.
#'
#' @importFrom lubridate year years quarter month week weeks day days interval floor_date
#' @export
RevenueData <- function(value, from, to, begin = min(from), end = max(from), id, by = "year", subset = rep(TRUE, length(id)), profiling = NULL, trim.id = 50) #, tolerance = .01)
{
    # Units.
     .period <- function(x)
     {
        if (by == "year")
            return(format(floor_date(x, by),"%Y"))
        if (by == "month" | by == "quarter")
            return(format(floor_date(x, by),"%Y-%m"))
        floor_date(data$from, by)
     }
    units <- switch(by, days = days(1), week = weeks(1), month = months(1), quarter = months(3), year = years(1))
    dys <- switch(by, year = 365.25, quarter = 365.25 / 4, month = 365.25 / 12, week = 7)
    # Merging profiling data.
    end <- floor_date(end, by)
    data <- data.frame(id = as.character(id), value, from = floor_date(from, by),  to = floor_date(to, by))
    if (!is.null(profiling))
    {
        if (!("id" %in% names(profiling)))
            profiling$id <- rownames(profiling)
        if (pos <- "value" %in% names(profiling))
        {
            names(profiling)[pos] <- "value.profiling"
            cat("'value' in 'profiling' has been renamed as 'value.profiling'.")
        }
        if (pos <- "from" %in% names(profiling))
        {
            names(profiling)[pos] <- "from.profiling"
            cat("'from' in 'profiling' has been renamed as 'from.profiling'.")
        }
        if (pos <- "to" %in% names(profiling))
        {
            names(profiling)[pos] <- "to.profiling"
            cat("'to' in 'profiling' has been renamed as 'to.profiling'.")
        }
        if (length(unique(id)) != nrow(profiling))
            stop("The number of unique 'id' values is different to the number of rows in 'profiling'.")
        data <- cbind(data, profiling[match(data$id, profiling$id), ])
    }
    # Filtering data.
    n.initial <- nrow(data)
    cat(paste0(n.initial, " transactions.\n"))
    n.subset <- sum(subset)
    if (n.subset < n.initial)
    {
        cat(paste0(n.initial - n.subset, " transactions filtered out.\n"))
        data <- subset(data, subset = subset)
    }
    # Removing observations that start after the end.
    n <- nrow(data)
    n.start.too.late <- sum(data$from > end)
    if (n.start.too.late > 0)
    {
        cat(paste0(n.start.too.late, " transactions removed as they start after the 'end' date.\n"))
        data <- subset(data, subset = data$from <= end)
    }
    zero <- data$value == 0
    n.zero <- sum(zero)
    if (n.zero > 0)
    {
        cat(paste0(n.zero, " transactions removed due to having 0 value.\n"))
        data <- subset(data, !zero)
    }
    negative <- data$value < 0
    n.negative <- sum(negative)
    if (n.negative > 0)
    {
        cat(paste0(n.negative, " transactions removed due to having a negative value.\n"))
        data <- subset(data, !negative)
    }
    n <- nrow(data)
    cat(paste0(n, " transactions remaining.\n"))
    # Aggregating transactions that occor in the same time period.
    data <- aggregate(value ~ id + from + to, data = data, FUN = sum)
    data$to.period <- .period(data$to)
    n <- nrow(data)
    cat(paste0(n, " aggregated transactions (i.e., with same beginning and end date) remaining.\n"))
    # Subscriber-level calculations.
    id.data <- aggregate(from ~ id, data, min)
    names(id.data)[2] <- "start"
    # Creating time-based metrics.
    id.data$last.from <- aggregate(from ~ id, data, max)[, 1]
    id.data$last.from.period <- .period(floor_date(aggregate(from ~ id, data, max)[, 2], by))
    cat(paste0(nrow(id.data), " subscribers.\n"))
    id.data$end.from <- aggregate(from ~ id, data, max)$from
    id.data$end.to <- aggregate(to ~ id, data, max)$to
    periods.from.start <- interval(id.data$start, id.data$end.to) %/% units
    id.data$start.period <- .period(id.data$start)
    id.data$last.period <- .period(id.data$end.from)
    id.data$churned <- id.data$end.to < end
    not.churned <- !id.data$churned
    if (sum(not.churned) == 0)
        stop("The analyses assume that 1 or more subscribers have churned. None are shown as having churned in the data.")
    #renewal.date[not.churned] <- (renewal.date + units)[not.churned] # The renewal data of the current license period.
    #print("b")
    #id.data$renewal.date <- renewal.date#<- as.Date(ifelse(id.data$churned, renewal.date, ))
    id.data$tenure.interval <- interval(id.data$start, id.data$end.to)
    id.data$tenure <- id.data$tenure.interval %/% units
    # Merging.
    data <- merge(data, id.data, by = "id", all.x = TRUE, sort = TRUE)
    data$period <- .period(data$from)
    #data$end.period <- .period(data$from)
    data$churn <- data$churned & data$period == data$last.period & .period(data$end.to) == .period(data$to)
    #print(table(data$churn))
    data$period.counter <- interval(data$start, data$from) %/% units
    # Sorting.
    data <- data[order(data$id, data$from),]
    # Creating a variable indicating observation number. Randomly sorts ties.
    observation <- rep(1, n)
    ids <- data$id
    for (i in 2:nrow(data))
        if (ids[i] == ids[i - 1])
            observation[i] = observation[i - 1] + 1
    data$observation <- observation
    data$id <- sub("\\s+$", "", as.character(data$id))
    class(data) <- c(class(data), "RevenueData")
    data
}
