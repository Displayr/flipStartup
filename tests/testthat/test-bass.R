# example
Sales <- c(840, 1470, 2110, 4000, 7590, 10950, 10530, 9470, 7790, 5890)
Sales <- Growth(rd, FALSE)$revenue
Sales <- c(Sales[1]/ 5, Sales)
Sales <- c(Sales[1], Sales[-1] - Sales[-length(Sales)])

T79 <- 1:length(Sales)
Tdelt <- (1:100)/length(Sales)
Cusales <- cumsum(Sales)
Bass.nls <- nls(Sales ~ M * (((P + Q)^2/P) * exp(-(P + Q) * T79))/(1 + (Q/P) *
             exp(-(P + Q) * T79))^2, start = list(M = 61000, P = 0.03, Q = 0.38))
summary(Bass.nls)


# get coefficient
Bcoef <- coef(Bass.nls)
m <- Bcoef[1]
p <- Bcoef[2]
q <- Bcoef[3]
# setting the starting value for M to the recorded total sales.
ngete <- exp(-(p + q) * Tdelt)

# plot pdf
Bpdf <- m * ((p + q)^2/p) * ngete/(1 + (q/p) * ngete)^2
plot(Tdelt, Bpdf, xlab = "Year from xxx", ylab = "Sales per year", type = "l")
points(T79, Sales)


