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
#' @param trim.id The maximum length of the strings to be used showing ID names (used to avoid situations where
#' string names are so long as to make reading of tables impossible.
#' @return A \code{\link{data.frame}} containing the following variables:
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
#'   \code{churned} A \code{logical} indicating if the subscriber had ceased subscribing.
#'   \code{tenure.interval} A \code{interval} of \code{start} to \code{end}.
#'   \code{tenure} The number of whole periods from the begining of the first subscription
#'   to the end of the most recent.
#'   \code{period} The \code{period} of the subscription.
#'   \code{period.counter} The number of the period, where 0 indicates the initial period.
#'   \code{observation} The number of the subscription, starting from 1.
#'
#' @importFrom lubridate year years quarter month week weeks day days interval floor_date
#' @export
RevenueData <- function(value, from, to, begin = min(from), end = max(from), id, by = "year", subset = rep(TRUE, length(id)), trim.id = 20) #, tolerance = .01)
{
    # Units.
    .period <- function(x)
        sapply(paste0(by, "('", eval(substitute(x)), "')"), function(x) eval(parse(text = x)))
    units <- switch(by, days = days(1), week = weeks(1), month = months(1), quarter = quarters(1), year = years(1))
    dys <- switch(by, year = 365.25, quarter = 365.25 / 4, month = 365.25 / 12, week = 7)
    #.periods <- function(x)
    #    sapply(paste0(by, "s('", eval(substitute(x)), "')"), function(x) eval(parse(text = x)))
    # Filtering data.
    data <- data.frame(id = as.character(id), value, from, to)
    n.initial <- nrow(data)
    cat(paste0(n.initial, " transactions.\n"))
    n.subset <- sum(subset)
    if (n.subset < n.initial)
    {
        cat(paste0(n.initial - n.subset, " transactions filtered out.\n"))
        data <- subset(data, subset = subset)
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
    id.data <- aggregate(from ~ id, data, min)
    names(id.data)[2] <- "start"
    id.data$last.from <- aggregate(from ~ id, data, max)
    id.data$last.from.period <- .period(floor_date(aggregate(from ~ id, data, max)[, 2], by))
    cat(paste0(nrow(id.data), " subscribers.\n"))
    id.data$end <- aggregate(to ~ id, data, max)$to
    periods.from.start <- interval(id.data$start, id.data$end) %/% units
    id.data$start.period <- .period(floor_date(id.data$start, by))
    id.data$last.period <- .period(floor_date(id.data$end, by))
    id.data$churned <- id.data$end < end
    not.churned <- !id.data$churned
    if (sum(not.churned) == 0)
        stop("The analyses assume that 1 or more subscribers have churned. None are shown as having churned in the data.")
    #renewal.date[not.churned] <- (renewal.date + units)[not.churned] # The renewal data of the current license period.
    #print("b")
    #id.data$renewal.date <- renewal.date#<- as.Date(ifelse(id.data$churned, renewal.date, ))
    id.data$tenure.interval <- interval(id.data$start, id.data$end)
    id.data$tenure <- id.data$tenure.interval %/% units
    # Merging.
    data <- merge(data, id.data, by = "id", all.x = TRUE, sort = TRUE)
    data$period <- .period(floor_date(data$from, by))
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
