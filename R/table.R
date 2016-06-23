#' \code{Table}
#' @description Creates a crosstab by aggregating numeric data over factors.
#' @param formula A \code{formula} where the dependent variable is the variable to be aggregated over.
#' @param data A \code{data.frame}.
#' @param FUN the function to be applied: see \code{apply} for details.
#'
#' @importFrom stats terms
#' @export
Table <- function(formula, data, FUN)
{
    has.outcome <- attr(terms(formula), "response") != 0
    if (!has.outcome)
    {
        if (!missing(FUN))
            stop("'FUN' can only be provided with a dependent variable.")
        return(xtabs(formula, data = data))
    }
    xtabs(formula, data = aggregate(formula, data, FUN = FUN))
}


aggregateAsVector <- function(x)
{
    #print(x)
    result <- x[, 2]
    names(result) <- x[, 1]
    result
}

#' @export
print.Index <- function(x, ...) print.table(x, digits = 3)
