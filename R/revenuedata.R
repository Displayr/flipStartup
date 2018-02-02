#' \code{RevenueData}
#'
#' @description Cleans and tidies data for use in growth accounting
#'     computations for a startup.
#' @param value A vector of containing the revenue per transaction.
#' @param from A vector of class \code{POSIXct} or \code{POSIXt},
#'     recording the date and time each subscription commences.
#' @param to A vector of class \code{POSIXct} or \code{POSIXt},
#'     recording the date and time each subscription ends
#' @param start The date at which the analysis outputs should
#'     commence. By default, the earliest date recorded in
#'     \code{from}.
#' @param end The date at which the analysis ends, which is used to
#'     determine churn.  By default, the most recent date recorded in
#'     \code{from}.
#' @param id A vector of \code{character}, unique identifier for
#'     subscribers that made the transactions (e.g., email addresses,
#'     names, subscriber keys).
#' @param subscription.length The time unit that describes the
#'     subscription length: \code{year} to view the data by year,
#'     \code{quarter}, and \code{month}. This is assumed to be the
#'     billing period when determining if subscribers have churned or
#'     not.
#' @param subset An optional vector specifying a subset of
#'     observations to be used in the calculations
#' @param profiling A \code{data.frame} containing data, unique by
#'     \code{id}, to be included in the final
#'     \code{data.frame}. Either it must contain the unique
#'     identifiers in a variable called \code{id}, or, the
#'     \code{rownames} must match the values of \code{id}.
#' @param trim.id The maximum length of the strings to be used showing
#'     ID names (used to avoid situations where string names are so
#'     long as to make reading of tables impossible.
#' @return A \code{\link{data.frame}} qhwew where the rows represent
#'     unique combinations of periods and subscribers. Where a
#'     subscriber has multiple transactions in a period, they are
#'     aggregated. Contains the following variables, along with any
#'     other variables in the \code{data}: \code{id}{The unique
#'     identifier.}  \code{value}{The total fee/price for a
#'     subscription.}  \code{from}{The commencement date of a
#'     subscription.}  \code{from.period} The \code{period} of the
#'     subscription commencement as a character.
#'     \code{period.counter} The number of the period, where 0
#'     indicates the initial period.  \code{to}{The end-date of a
#'     subscription.}  \code{to.period} The \code{period} of the
#'     subscription end as a character.  \code{subscriber.from} The
#'     date of a customer's first subscription's commencement.
#'     \code{subscriber.from.period} The \code{period} of
#'     \code{subscriber.from}.  \code{subscriber.to} The final date of
#'     their most recent subscription.  \code{subscriber.to.period}
#'     The period of \code{subscriber.to}.  \code{last.from} The
#'     \code{from} date of the most recent subscription.
#'     \code{last.from.period} The period of \code{last.from}.
#'     \code{churned} A \code{logical} indicating if the subscriber
#'     had ceased subscribing prior to \code{end}.  \code{churn} A
#'     \code{logical} indicating if the subscriber had ceased
#'     subscribing in that period.  \code{tenure} The number of whole
#'     periods from the begining of the first subscription to the end
#'     of the most recent.  \code{observation} The invoice number for
#'     a particular customer, starting from 1.
#'     \code{observation.within.period} The number of the subscription
#'     for a particular customer, starting from 1 for each new
#'     subscription period (as determined by a common to.period).
#'
#' @importFrom lubridate period year years quarter month week weeks
#' day days interval floor_date tz  "tz<-"
#' @importFrom flipTime Period Periods AsDate DiffPeriod
#' @export
RevenueData <- function(value, from, to, start = min(from), end = max(from), id,
                        subscription.length = "year", subset = rep(TRUE, length(id)),
                        profiling = NULL, trim.id = 50) #, tolerance = .01)
{
    default.start.end <- start == min(from, na.rm = TRUE) & end == max(from, na.rm = TRUE)
    # Units.
    units <- Periods(1, subscription.length)
    data <- data.frame(id = as.character(id), value, from, to)

    # Filtering data.
    n.initial <- nrow(data)
    cat(paste0(n.initial, " transactions.\n"))
    n.subset <- sum(subset)
    if (n.subset < n.initial)
    {
        cat(paste0(n.initial - n.subset, " transactions filtered out.\n"))
        data <- subset(data, subset = subset)
    }
    # Sorting by company name and start-date
    data <- data[with(data, order(id, from)), ]
    # Removing observations that start after the end.
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
    if (n == 0)
        return(NULL)
    # Splitting apart transactions that exceed the subscription length.
    to <- as.Date(data$to)
    n <- length(to)
    to.day.month <- to - lubridate::years(year(to))
    #print(to.day.month)
    mode.day.month <- ave(to.day.month, data$id, FUN = function(x) names(sort(-table(x)))[1])
    n.subscriptions <- DiffPeriod(data$from, to, ceiling = TRUE, by = subscription.length)
    max.n.subscriptions <- max(n.subscriptions)
    if (max.n.subscriptions > 1)
        for (i in 1:max.n.subscriptions)
        {
            to <- as.Date(data$to)
            from <- as.Date(data$to)
            new.from <- to - units
            
            
        }
        

    # Aggregating transactions that occur in the same time period.
    data$to.period <- Period(data$to, subscription.length)
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
    id.data$last.from.period <- Period(floor_date(aggregate(from ~ id, data, max)[, 2], subscription.length), subscription.length)
    cat(paste0(nrow(id.data), " subscribers.\n"))
    id.data$subscription.to <- aggregate(to ~ id, data, max)$to
    tenure.interval <- interval(id.data$subscriber.from, id.data$subscriber.to)
    id.data$tenure <- tenure.interval %/% units
    id.data$subscriber.from.period <- Period(id.data$subscriber.from, subscription.length)
    id.data$subscriber.to.period <- Period(id.data$subscriber.to, subscription.length)
    id.data$churned <- id.data$subscriber.to <= end
    not.churned <- !id.data$churned
    data <- merge(data, id.data, by = "id", all.x = TRUE, sort = TRUE)
    data$from.period <- Period(data$from, subscription.length)
    data$to.period <- Period(data$to, subscription.length)
    data$churn <- data$churned & data$from.period == data$last.from.period
    data$period.counter <- interval(data$subscriber.from, data$from) %/% units
    # Sorting.
    data <- data[order(data$id, data$from),]
    # Creating a variable indicating observation number. Randomly sorts ties.
    observation <- observation.within.period <- rep(1, n <- nrow(data))
    if (n > 1)
    {
        ids <- data$id
        for (i in 2:n)
            if (ids[i] == ids[i - 1])
            {
                if (data$to.period[i] == data$to.period[i - 1])
                    observation.within.period[i] = observation.within.period[i - 1] + 1
                observation[i] = observation[i - 1] + 1

            }
    }
    data$observation <- observation
    data$observation.within.period <- observation.within.period

    data$id <- sub("\\s+$", "", as.character(data$id))
    if (!default.start.end)
    {
        window <- interval(start, end)
        old.tz <- tz(from)
        from <- AsDate(data$from.period, on.parse.failure = "silent")
        tz(from) <- old.tz
        to <- AsDate(data$to.period, on.parse.failure = "silent")
        tz(to) <- old.tz
        data <- data[to %within% window | from %within% window | from < start & to > end, ]
        cat(paste0(nrow(data), " aggregated transactions left after taking 'start' and/or 'end' into account.\n"))
        cat(paste0(length(unique(data$id)), " subscribers left after taking 'start' and/or 'end' into account.\n"))
    }
    attr(data, "subscription.length") <- subscription.length
    attr(data, "end") <- end
    class(data) <- c(class(data), "RevenueData")
    data
}
