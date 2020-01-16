# Tested in test-startup metric

#' @export
Tab <- function(x)
{
    UseMethod("Tab", x)
}

#' @export
Tab.default <- function(x, ...)
{
    sapply(x$id, paste, collapse = ", ")
}
