
#' \code{Index}
#' @description Indexes the data on a table by its rows or columns.
#' @param x The data to be indexed.
#' @param i The row or column to be used as the base for the index.
#' @param by.row Index y rows.
#' @param STATS The row or column number to index by, or, a vector containing the data to be swept out.
#' @param remove Where the options are "", which does nothing, and "lower left",
#' "lower right", "upper left", and "upper right", and they are set to NA.
#' @param remove.diag If TRUE, and \code{remove} is not set to "", removes the diagonal as well.
#' @export
Index <- function(x, by.row = TRUE, i = 1, STATS = NULL, remove = "", remove.diag = FALSE)
{
    margin <- if(by.row) 1 else 2
    if (is.null(STATS))
        STATS <- if (by.row) x[, i] else x[i, ]
    result <- sweep(x, margin, STATS, "/") * 100
    n.rows <- nrow(x)
    n.columns <- ncol(x)
    if (remove != "")
        result[Triangle(result, position = remove, diag = remove.diag)] <- NA
    class(result) <- "Index"
    result
}

#' \code{IndexDiagonal}
#' @description Indexes relative to the diagonal.
#' @param x The data to be indexed.
#' @param off TRUE if indexing by the off diagonal.
#' @param by.row Indexes by row (versus by column).
#' those to the right of them, to NA.
#' @export
IndexDiagonal <- function(x, off = FALSE, by.row = TRUE)
{
    STATS <- Diagonal(x, off)
    remove <- if(off) "lower right" else "lower left"
    result <- Index(x, by.row = by.row, STATS = STATS, remove = remove)
    result
}


#' @export
print.Index <- function(x, ...) print.table(x, digits = 0)
