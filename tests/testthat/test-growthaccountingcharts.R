context("growth accounting charts")
data(q.invoice.lines)
d <- q.invoice.lines
library(lubridate)
Sys.setenv(TZ='GMT')
end <-  ISOdate(2016,2,15)
start <-  ISOdate(2012,7,1)

for (by in c("week", "month", "quarter", "year"))
    test_that(paste("Creating RevenueData", by),
          {
            expect_error(capture.output(rd <- RevenueData(d$AUD, d$ValidFrom, d$ValidTo, start = start, end = end, id = d$name, by = by, subset = d$validInvoice == 1)), NA)
            expect_error(rg <- RevenueGrowthAccounting(rd), NA)

            expect_error(print(plot(rg)), NA)
            expect_error(print(QuickRatioPlot(rg)), NA)

            expect_error(capture.output(print(w <- Waterfall(rg))), NA)
            expect_error(print(plot(w)), NA)
            expect_error(capture.output(print(w <- Waterfall(rg, names(rg$Revenue)[length(names(rg$Revenue)) - 1]))), NA)
            expect_error(print(plot(w)), NA)

})



