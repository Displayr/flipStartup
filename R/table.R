#' \code{Table}
#' @description Creates a crosstab by aggregating numeric data over factors.
#' @param formula A \code{formula} where the dependent variable is the variable to be aggregated over.
#' @param data A \code{data.frame}.
#' @param FUN the function to be applied: see \code{apply} for details.
#'
#' @export
Table <- function(formula, data, FUN)
{
    tbl <- xtabs(formula, data = aggregate(formula, data, FUN = FUN))
    #if (!is(tbl, "data.frame"))
    #    tbl <- aggregateAsVector(tbl)
    #class(tbl) <- c("Table", class(tbl))
    tbl
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
