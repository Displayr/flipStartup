#' #' \code{Bass}
#' #'
#' #' @description Fits the Bass model for new products.
#' #' @param data A \code{data.frame} that has the same variables as a \code{RevenueData} object.
#' #' @param remove.last Remove the final period (as usually is incomplete).
#' #' @return A \code{\link{list}} containing the following elements:
#' #' \item{p}{The coefficient of innovation}.
#' #' \item{q}{The coefficient of immitation}.
#' #' \item{observed}{The observed sales/penetration}.
#' #' \item{predicted}{The predicted/fitted sales/penetration}.
#' #' \item{max}{The maximum revenue (or the revenue at 1,000 time periods, whichever is smaller)}.
#' #' @details Fits the model using OLS. While NLS estimation may be better in some situations, OLS is less prone to computational errors (e.g., problems with start points, non-convergence)
#' #' @references Bass, Frank (1969). "A new product growth for model consumer durables".
#' #' Management Science 15 (5): 215-227.
#' #' @importFrom stats lm
#' #' @export
#' Bass <- function(data, remove.last = TRUE)
#' {
#'     if (remove.last)
#'         data <- removeLast(data)
#'     revenue <- Growth(data, remove.last)$revenue
#'     time <- as.numeric(names(revenue))
#'     rev <- Sales <- c(revenue[1], revenue[-1] - revenue[-length(revenue)])
#'     df <- data.frame(revenue = c(0, rev), time = c(time[1] - (time[2] - time[1]), time))
#'     df$cumulative <- cumsum(df$revenue)
#'     df$cumulative.lag <- c(NA, df$cumulative[-length(time)])
#'     df <- df[-1, ]
#'     coefs <- lm(revenue ~ cumulative + cumulative.lag ^ 2, data = df)$coef
#'     #print(summary(lm(revenue ~ cumulative + cumulative.lag ^ 2, data = df)))
#'     a <- coefs[1]
#'     b <- coefs[2]
#'     c <- coefs[3]
#'     m.minus <- (-b - sqrt(b ^ 2 - 4 * a * c)) / (2 * c)
#'     m.plus <- (-b + sqrt(b ^ 2 - 4 * a * c)) / (2 * c)
#'     m <- m.minus#-(b + sqrt(b ^ 2 - 4 * a * c)) / (2 * c)
#'     print(m.plus)
#'     p <- 1 / m
#'     q <- b + p
#'     #print(c(m.minus, m.plus, m, q, p))
#'     #print(df)
#'     .predict <- function(p, q, m, T = 100)
#'     {
#'         y <- rep(NA, T)
#'         Ycum <- 0
#'         t <- 1:T
#'         y <- m * (((p + q)^2 / p) * exp( - (p + q) * t )) / (1 + q / p * exp(-(p + q) * t)) ^ 2
#'         # for (t in 1:T)
#'         # {
#'         #     y[t] <- (p + q / m * Ycum) * (m - Ycum)
#'         #     Ycum <- y[t] + Ycum
#'         # }
#'         y
#'     }
#'     predicted <- .predict(p, q, m)
#'     print(sum(!is.na(predicted)))
#'     print(summary(predicted))
#'     predicted <- predicted[!is.na(predicted) & predicted < Inf ]
#'     max <- (log(q) - log(p)) / (p + q)
#'     predicted <- predicted[predicted / max <= 0.9999]
#'     list(p = p, q = q, m = m, observed = revenue, predicted = predicted, max = max)
#' }
#' 
#' 
#' 
