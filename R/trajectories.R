#' \code{Trajectories}
#'
#' @description Computse the trajectory for each unique ID.
#' @param value A vector of containing the quantity of interest.
#' @param from A vector of class \code{POSIXct} or \code{POSIXt}, recording the date
#' and time each subscription commences.
#' @param to A vector of class \code{POSIXct} or \code{POSIXt}, recording the date
#' and time each subscription ends
#' @param id A vector of \code{character}, unique identifier for subscribers that
#' made the transactions (e.g., email addresses, names, subscriber keys).
#' @return A \code{\link{list}}, in which each element is the cumultative value within the subscription period, and the name of the element is the date.
#' The subscription period is determined based on the \code{from} and \code{to} dates.
#' @importFrom lubridate interval
#' 
#' @export
Trajectories <- function(value, from, to, id)
{
    # # Aggregating transactions that occur on the same data.
    # data = aggregate(value, list(order(id, to, from)), sum)
    #  # sorting
    # data = data[order(data$id, data$to, data$from), ]
    #id = data$id
    #from = data$from
    int <- interval(from, to)
    unique.id <- unique(id)
    n.id <- length(unique.id)
    result <- vector("list", n.id)
    names(result) = unique.id
    for (i in 1:n.id)
    {
        m <- id == unique.id[i]
        i.from <- from[m]
        i.interval <- int[m]
        i.from.unique <- sort(unique(i.from))
        n.i <- length(i.from.unique)
        res <- rep(0, n.i)
        i.value <- value[m]
        for (i.d in 1:n.i)
        {
            d <- i.from.unique[i.d]
            res[i.d] <- res[i.d] + sum(i.value[d %within% i.interval])
            
        }
        names(res) <- i.from.unique
        result[[i]] <- res
    }
    class(result) <- c("Trajectories", class(result))
    result
}

#' \code{as.data.frame.Trajectories}
#'
#' @description Creates a 'long' \code{data.frame} of a \code{Trajectories} object.
#' Computse the trajectory for each unique ID.
#' @param x A \code{Trajectories} object.
#' @param row.names \code{NULL} or a character vector giving the row names for the data frame. Missing values are not allowed.
#' @param optional Currently not used.
#' @param ... Currently not used.
#' A \code{Trajectories} object.
#' @return A \code{\link{data.frame}},
#' @importFrom lubridate interval years
#' @method as.data.frame Trajectories
#' 
#' @export
as.data.frame.Trajectories <- function(x, row.names = NULL, optional = FALSE, ...)
{
    if (optional)
        warning("'optional' parameter not supported.")
    n <- length(unlist(x))
    n.id <- length(x)
    max.value <- min.value <- initial.value <- n.observations <- observation <- id <- days <- years <- dates <- index <- values <- rep(NA, n)
    ids <- names(x)
    counter <- 0
    for (i in 1:n.id)
    {
        i.x <- x[[i]]
        i.n <- length(i.x)
        o <- counter + 1:i.n
        observation[o] <- 1:i.n
        n.observations[o] <- i.n
        id[o] <- ids[i]
        values[o] <- i.x
        initial.value[o] <- init <- i.x[1]
        index[o] <- i.x / init
        min.value[o] <- min(i.x)
        max.value[o] <- max(i.x)
        dates[o] <- d <- names(i.x)
        d <- as.Date(d)
        days[o] <- (d - d[1])
        years[o] <- interval(d[1], d) / years(1)
        counter <- counter + i.n
    }
    result <- data.frame(id, observation, n.observations, initial.value = initial.value, value = values, min.value = min.value, max.value = max.value, index, date = as.Date(dates), days = days, years = years)
    if (!is.null(row.names))
        rownames(result) <- row.names
    result$start.year = interval(min(result$date), result$date) / years(1) 
    result
}