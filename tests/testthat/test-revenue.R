context("Revenue")
data(q.invoice.lines)
d <- q.invoice.lines
library(lubridate)
#Sys.setenv(TZ='GMT')
end <-  ISOdate(2016,12,31)
start <-  ISOdate(2012,7,1)
by = "week"
for (by in c("week", "month", "quarter", "year"))
    test_that(paste("Creating RevenueData", by),
          {
            expect_error(capture.output(rd <- RevenueData(d$AUD, d$ValidFrom, d$ValidTo, start = start, end = end, id = d$name, subscription.length = by, subset = d$validInvoice == 1)), NA)
            expect_error(capture.output(print(Revenue(rd, by = by))), NA)
})


dollars <- c(100, 100, 100, 100)
from <- as.Date(c("2016/01/01", "2016/06/01", "2016/08/01", "2016/10/01"))
to <- as.Date(c("2016/12/31", "2016/12/31", "2017/07/31", "2017/10/31"))
name <- c("A", "A", "B", "C")
test_that("MRR", {
    capture.output(rd <- RevenueData(dollars, from, to, id = name, subscription.length = "year"))
    expect_equal(sum(Revenue(rd, by = "month")), 300)
    capture.output(rd <- RevenueData(dollars, from, to, id = name, subscription.length = "month"))
    expect_equal(sum(Revenue(rd, by = "month")), 4400)
})

test_that("YRR", {
    capture.output(rd <- RevenueData(dollars, from, to, id = name, subscription.length = "year"))
    expect_equal(sum(Revenue(rd, by = "year")), 300)
    capture.output(rd <- RevenueData(dollars, from, to, id = name, subscription.length = "month"))
    expect_equal(sum(Revenue(rd, by = "year")), 300)
})
