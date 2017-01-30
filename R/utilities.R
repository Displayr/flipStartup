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


