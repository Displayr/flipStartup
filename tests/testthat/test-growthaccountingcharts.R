context("growth accounting charts")
data(q.invoice.lines)
d <- q.invoice.lines
library(lubridate)
#Sys.setenv(TZ='GMT')
end <-  ISOdate(2016,2,15)
start <-  ISOdate(2012,7,1)
by = "year"
for (by in c("week", "month", "quarter", "year"))
    test_that(paste("Creating RevenueData", by),
          {
              
            expect_error(capture.output(rd <- RevenueData(d$AUD, d$ValidFrom, d$ValidTo, start = start, end = end, id = d$name, subscription.length = by, subset = d$validInvoice == 1)), NA)
            expect_error(rg <- GrowthAccounting(rd, remove.last = FALSE), NA)
            expect_error(print(plot(rg)), NA)
            expect_error(rg <- GrowthAccounting(rd, remove.last = TRUE), NA)
            expect_error(print(plot(rg)), NA)
            expect_error(print(QuickRatioPlot(rg)), NA)

})



