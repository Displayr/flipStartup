context("Layercake")
data(q.invoice.lines)
d <- q.invoice.lines
library(lubridate)
Sys.setenv(TZ='GMT')
end <-  ISOdate(2016,12,31)
start <-  ISOdate(2012,7,1)
by = "year"
for (by in c("week", "month", "quarter", "year"))
    test_that(paste("Layercake", by),
          {
            expect_error(print(LayerCake(RevenueData(d$AUD, d$ValidFrom, d$ValidTo, start = start, end = end, id = d$name, by = by, subset = d$validInvoice == 1))), NA)
})

