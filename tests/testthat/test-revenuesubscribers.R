context("Revenue subscribers")
data(q.invoice.lines)
d <- q.invoice.lines
library(lubridate)
Sys.setenv(TZ='GMT')
end <-  ISOdate(2016,12,31)
start <-  ISOdate(2012,7,1)
by = "month"
for (by in c("week", "month", "quarter", "year"))
    test_that(paste("Creating RevenueData", by),
          {
            expect_error(capture.output(rd <- RevenueData(d$AUD, d$ValidFrom, d$ValidTo, start = start, end = end, id = d$name, by = by, subset = d$validInvoice == 1)), NA)
    plot(Subscribers(rd, by = "month"))
    plot(Revenue(rd))
})


