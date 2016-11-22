#' \code{PeriodNameToDate}
#'
#' @description Converts a vector of period names from a \code{\link{RevenueData}} object
#' into a date.
#' @param x The vector of \code{char} to convert.
#' @param by The time aggregation.
#' @importFrom lubridate ymd
#' @export
PeriodNameToDate <- function(x, by)
{
    if (by == "year")
        x <- paste0(x, "-01")
    if (by %in% c("month", "quarter", "year"))
        x <- paste0(x, "-01")
    ymd(x)
}

#' \code{CompleteListPeriodNames}
#'
#' @description Returns a vector that contains all the possible period dates.
#' @param x The vector of \code{char} to convert.
#' @param by The time aggregation.
#' @importFrom lubridate ymd
#' @export
CompleteListPeriodNames <- function(x, by)
{
    if(by == "year")
    {
        x <- as.numeric(x)
        return(as.character(min(x):max(x)))
    }
    observed.dates <- PeriodNameToDate(x, by)
    Period(seq(min(observed.dates), max(observed.dates), by = by), by)
}
#' \code{Period}
#'
#' @description Converts a date into a character.
#' @param x The date.
#' @param by The period used in the conversion (e.g., "week", "year").
#' @importFrom lubridate floor_date
#' @export
Period <- function(x, by)
{
    if (by == "year")
        return(format(floor_date(x, by),"%Y"))
    if (by == "month" | by == "quarter")
        return(format(floor_date(x, by),"%Y-%m"))
    if (by == "week")
        return(format(floor_date(x, by),"%Y-%m-%d"))
    format(floor_date(x, by),"%Y-%m-%d")
}

#' \code{DaysPerPeriod}
#'
#' @description The average number of dates in a period. E.g., 7 for a day, 365.25 for a year.
#' @param by The period used in the conversion (e.g., "week", "year").
#' @export
DaysPerPeriod <- function(by)
{
    switch(by, year = 365.25, quarter = 365.25 / 4, month = 365.25 / 12, week = 7, day = 1)

}


#' \code{FillInMatrix}
#'
#' @description Fills in missing rows and/or columns in a matrix.
#' @param x The matrix.
#' @param row.names The required row names, which includes the current row names
#' as a subset.
#' @param col.names The required colum names, which includes the current column names
#' as a subset.
#' @param value The value to fill in for cells that are not in the origial matrix.
#' @export
FillInMatrix <- function(x, row.names, col.names, value = 0)
{
    new.dimnames <- list(row.names, col.names)
    names(new.dimnames) <- names(dimnames(x))
    new.x <- matrix(value, length(row.names), length(col.names), dimnames = new.dimnames)
    new.x[match(rownames(x), row.names), match(colnames(x), col.names)] <- x
    new.x
}

#' \code{FillInVector}
#'
#' @description Fills in missing rows and/or columns in a matrix.
#' @param x The vector
#' @param element.names The required names of the elements of the vector.
#' @param value The value to fill in for cells that are not in the origial matrix.
#' @export
FillInVector <- function(x, element.names, value = 0)
{
    n <- length(element.names)
    new.x <- rep(value, n)
    names(new.x) <- element.names
    new.x[match(names(x), element.names)] <- x
    new.x
}





#' \code{Triangle}
#'
#' @description Lower and Upper Triangle  (left and right) part of a matrix.
#' @param x A matrix.
#' @param position One of \code{"lower left"}, \code{"lower right"},
#' \code{"upper left"}, or \code{"upper right"}.
#' @param diag Logical. If \code{TRUE}, the diagonal is included in the triangle.
#'
#' @export
Triangle <- function(x, position = "lower right", diag = FALSE)
{
    switch(position,
           "lower left" = lower.tri(x, diag),
           "lower right" = lower.tri(x, diag)[, ncol(x):1],
           "upper right" = upper.tri(x, diag),
           "upper left" = upper.tri(x, diag)[, ncol(x):1])
}

#' \code{Diagonal}
#'
#' @description Extract or replace the diagonal of a matrix, or construct a diagonal matri.
#' @param x A \code{matrix} or a number indicating the number of rows in a square matrix.
#' @param off \code{logical}. Operates on the off diagonal if selected.
#'
#' @export
Diagonal <- function(x, off = FALSE)
{
    if (is.matrix(x)){
        if (off)
            x <- x[,ncol(x):1]
        return(diag(x))
    }
    d <- diag(x)
    if (off)
        d <- d[,ncol(d):1]
    d
}


