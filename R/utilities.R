#' \code{Triangle}
#'
#' @description Lower and Upper Triangle  (left and right) part of a matrix.
#' @param x A matrix.
#' @param position One of \code{"lower left"}, \code{"lower right"},
#' \code{"upper left"}, or \code{"upper right"}.
#' @param \code{logical}. Should the diagonal be included?
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


