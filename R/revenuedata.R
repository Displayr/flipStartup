#' \code{RevenueData}
#'
#' @description Cleans and tidies data for use in growth accounting computations for a startup.
#' @param value A vector of containing the revenue per transaction.
#' @param from A vector of class \code{POSIXct} or \code{POSIXt}, recording the date
#' and time each subscription commences.
#' @param to A vector of class \code{POSIXct} or \code{POSIXt}, recording the date
#' and time each subscription ends
#' @param start The date at which the analysis outputs should commence. By default,
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
#' @return A \code{\link{data.frame}} qhwew where the rows represent unique combinations of periods
#' and subscribers. Where a subscriber has multiple transactions in a period, they are aggregated. Contains
#' the  following variables, along with any other variables in the \code{data}:
#'   \code{id}{The unique identifier.}
#'   \code{value}{The total fee/price for a subscription.}
#'   \code{from}{The commencement date of a subscription.}
#'   \code{from.period} The \code{period} of the subscription commencement as a character.
#'   \code{period.counter} The number of the period, where 0 indicates the initial period.
#'   \code{to}{The end-date of a subscription.}
#'   \code{to.period} The \code{period} of the subscription end as a character.
#'   \code{subscriber.from} The date of a customer's first subscription's commencement.
#'   \code{subscriber.from.period} The \code{period} of \code{subscriber.from}.
#'   \code{subscriber.to} The final date of their most recent subscription.
#'   \code{subscriber.to.period} The period of \code{subscriber.to}.
#'   \code{last.from} The \code{from} date of the most recent subscription.
#'   \code{last.from.period} The period of \code{last.from}.
#'   \code{churned} A \code{logical} indicating if the subscriber had ceased subscribing prior to \code{end}.
#'   \code{churn} A \code{logical} indicating if the subscriber had ceased subscribing in that period.
#'   \code{tenure.interval} A \code{interval} of \code{subscriber.from} to \code{subscriber.to}.
#'   \code{tenure} The number of whole periods from the begining of the first subscription
#'   to the end of the most recent.
#'   \code{observation} The number of the subscription for a particular customer, starting from 1.
#'
#' @importFrom lubridate year years quarter month week weeks day days interval floor_date
#' @export
RevenueData <- function(value, from, to, start = min(from), end = max(from), id, by = "year", subset = rep(TRUE, length(id)), profiling = NULL, trim.id = 50) #, tolerance = .01)
{
    Sys.setenv(TZ='GMT')
    # Units.
    units <- switch(by, days = days(1), week = weeks(1), month = months(1), quarter = months(3), year = years(1))
    dys <- DaysPerPeriod(by)
    # Merging profiling data.
    end <- floor_date(end, by)
    data <- data.frame(id = as.character(id), value, from = floor_date(from, by), to = floor_date(to, by))
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
    # n <- nrow(data)
    # start.too.late <- data$from > end
    # n.start.too.late <- sum(start.too.late)
    # if (n.start.too.late > 0)
    # {
    #     cat(paste0(n.start.too.late, " transactions removed as the subscription starts after the 'end' date.\n"))
    #     data <- subset(data, !start.too.late)
    # }
    # # Removing observations that end after start
    # n <- nrow(data)
    # end.too.early <- data$to < start
    # n.end.too.early <- sum(end.too.early)
    # if (n.end.too.early > 0)
    # {
    #     cat(paste0(n.end.too.early, " transactions removed as the subscription ends before the 'start' date.\n"))
    #     data <- subset(data, subset = !end.too.early)
    # }
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
    data$to.period <- Period(data$to, by)
    data <- aggregate(value ~ id + from + to, data = data, FUN = sum)#data <- aggregate(value ~ id + from + to, data = data, FUN = sum)
    n <- nrow(data)
    cat(paste0(n, " aggregated transactions (i.e., summed together when sharing a from and end date) remaining.\n"))
    # Subscriber-level calculations.
    id.data <- aggregate(from ~ id, data, min)
    names(id.data)[2] <- "subscriber.from"
    id.data$subscriber.to <- aggregate(to ~ id, data, max)[, 2]
    if (!is.null(profiling))
    {
        if (("id" %in% names(profiling)))
        {
            profiling.id <- as.character(profiling$id)
            profiling$id <- NULL
        }
        else
            profiling.id <- rownames(profiling)
        profiling.id <- as.character(profiling.id)

        lookup <- match(as.character(id.data$id), profiling.id)
        if (sum(!is.na(lookup)) == 0)
        {
            stop("The 'profiling' data is needs to either contain an 'id' variable, or have rownames that contain the 'id' values.")
        }
        else if (sum(is.na(lookup)) > 0)
        {
            missing.ids <- paste(id.data$id[is.na(lookup)], collapse = ",")
            stop(paste0("The 'profiling' data is missing some ids: ", missing.ids))
        }
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
        id.data <- cbind(id.data, profiling[lookup, ])
    }
    # Creating time-based metrics.
    id.data$last.from <- aggregate(from ~ id, data, max)[, 1]
    id.data$last.from.period <- Period(floor_date(aggregate(from ~ id, data, max)[, 2], by), by)
#print("doga")
    cat(paste0(nrow(id.data), " subscribers.\n"))
#id.data$end.from <- aggregate(from ~ id, data, max)$from
#print("dogb")
    id.data$subscription.to <- aggregate(to ~ id, data, max)$to
#print("dob")
    id.data$tenure.interval <- interval(id.data$subscriber.from, id.data$subscriber.to)
    id.data$tenure <- id.data$tenure.interval %/% units
#print("dogd")
    id.data$subscriber.from.period <- Period(id.data$subscriber.from, by)
    id.data$subscriber.to.period <- Period(id.data$subscriber.to, by)
#id.data$last.period <- Period(id.data$end.from, by)
#print("dog")
    id.data$churned <- id.data$subscriber.to < end
#print("cat")
    not.churned <- !id.data$churned
    #if (sum(not.churned) == 0)
    #    stop("The analyses assume that 1 or more subscribers have churned. None are shown as having churned in the data.")
    # Merging.
    data <- merge(data, id.data, by = "id", all.x = TRUE, sort = TRUE)
    data$from.period <- Period(data$from, by)
    data$to.period <- Period(data$to, by)
    #data$period.date <- floor_date(data$from, by)
    data$churn <- data$churned & data$from.period == data$last.from.period# & Period(data$relationship.to, by) == Period(data$to, by)
    data$period.counter <- interval(data$subscriber.from, data$from) %/% units
    # Sorting.
    data <- data[order(data$id, data$from),]
    # Creating a variable indicating observation number. Randomly sorts ties.
    observation <- rep(1, n <- nrow(data))
    ids <- data$id
    for (i in 2:n)
        if (ids[i] == ids[i - 1])
            observation[i] = observation[i - 1] + 1
    data$observation <- observation
    data$id <- sub("\\s+$", "", as.character(data$id))
    data <- data[data$from >= start & data$from <= end, ]
    attr(data, "by") <- by
    class(data) <- c(class(data), "RevenueData")
    data
}
