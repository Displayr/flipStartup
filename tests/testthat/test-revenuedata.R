context("Revenue data")
data(q.invoice.lines)
d <- q.invoice.lines
library(lubridate)
#Sys.setenv(TZ='GMT')
end <-  ISOdate(2016,2,15)
start <-  ISOdate(2012,7,1)

for (by in c("week", "month", "quarter", "year"))
    test_that(paste("Creating RevenueData", by),
          {
            # Beginning and end
            expect_error(capture.output(rd <- RevenueData(d$AUD, d$ValidFrom, d$ValidTo, id = d$name, subscription.length = by, subset = d$validInvoice == 1)), NA)
            expect_error(capture.output(RevenueGrowthAccounting(rd)), NA)

            expect_error(capture.output(rd <- RevenueData(d$AUD, d$ValidFrom, d$ValidTo, start = start, id = d$name, subscription.length = by, subset = d$validInvoice == 1)), NA)
            expect_error(capture.output(RevenueGrowthAccounting(rd)), NA)

            expect_error(capture.output(rd <- RevenueData(d$AUD, d$ValidFrom, d$ValidTo, end = end, id = d$name, subscription.length = by, subset = d$validInvoice == 1)), NA)
            expect_error(capture.output(RevenueGrowthAccounting(rd)), NA)

            expect_error(capture.output(rd <- RevenueData(d$AUD, d$ValidFrom, d$ValidTo, start = start, end = end, id = d$name, subscription.length = by, subset = d$validInvoice == 1)), NA)
            expect_error(capture.output(RevenueGrowthAccounting(rd)), NA)

            # Adding profiling variables of the wrong dimension
            expect_error(capture.output(rd <- RevenueData(d$AUD, d$ValidFrom, d$ValidTo, end = ISOdate(2016,06,14), id = d$name, subscription.length = by, subset = d$validInvoice == 1, profiling = d)))
            expect_error(capture.output(RevenueGrowthAccounting(rd)), NA)

            # Adding profiling variables of the correct dimension
            unique.names <- sort(unique(d$name))
            zprofiling <- d[match(unique.names, as.character(d$name)), ]
            expect_error(capture.output(rd <- RevenueData(d$AUD, d$ValidFrom, d$ValidTo, end = ISOdate(2016,06,14), id = d$name, subscription.length = by, subset = d$validInvoice == 1, profiling = zprofiling)))
            expect_error(capture.output(RevenueGrowthAccounting(rd)), NA)

            # Adding profiling variables with id shown in row names
            rownames(zprofiling) <- unique.names
            capture.output(rd <- RevenueData(d$AUD, d$ValidFrom, d$ValidTo, id = d$name, end = ISOdate(2016,11,1), subscription.length = by, subset = d$validInvoice == 1, profiling = zprofiling))
            expect_error(capture.output(RevenueGrowthAccounting(rd)), NA)
})



