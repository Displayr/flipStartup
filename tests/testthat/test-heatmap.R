context("heatmap")
# library(flipStatistics)
# data(q.invoice.lines)
# d <- q.invoice.lines
#

# # Heatmap.
# mydata <- mtcars[, c(1,3,4,5,6,7)]
# cormat <- round(cor(mydata),2)
# heatmap(cormat)
# Heatmap(cormat)
# cormat[lower.tri(cormat)] <- NA
# Heatmap(cormat)
# names(dimnames(cormat)) <- c("dog","cat")
# Heatmap(cormat, title = "Churn")
#


# data(q.invoice.lines)
# d <- q.invoice.lines
# by = "year"
# library(lubridate)
# today <- ISOdate(2016,6,30)
# tz(today) <- "GMT"
# #today <- today + hours(12) - seconds(1)
# Sys.setenv(TZ='GMT')
#
# rd <- RevenueData(d$AUD, d$ValidFrom, d$ValidTo, end = ISOdate(2016,06,14), id = d$name, by = by, subset = d$validInvoice == 1)
# # retention <- Retention(rd, FALSE)

# Heatmap(t(retention$index), "Retention")
# Heatmap(retention$volume * 100, "Retention (%)")
# Heatmap(retention$retention * 100, "Retention (%)")
#
#
# l <- LifetimeValue(rd)
# LifetimeValue(rd)
# Heatmap(l$total, "Total")
# Heatmap(l$mean, "Mean")
# Heatmap(l$cumulative, "Cumulative")
# Heatmap(l$index, "Index")
#
#
# rd <- RevenueData(d$AUD, from , to, end = end, id = d$name, by = "year", subset = d$validInvoice == 1)
# LayerCake(rd)

