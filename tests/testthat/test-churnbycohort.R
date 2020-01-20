context("Churn by cohort")
data(q.invoice.lines)
d <- q.invoice.lines
library(lubridate)
#Sys.setenv(TZ='GMT')
end <-  ISOdate(2016,6,30)
#start <-  ISOdate(2012,7,1)
by = "year"
for (v in c(FALSE, TRUE))
    for (by in c("quarter", "year"))
        test_that(paste("ChurnByCohort", by, "volume:", v),
                  {
                      expect_error(capture.output(rd <- RevenueData(d$AUD, d$ValidFrom, d$ValidTo, end = end, id = d$name, subscription.length = by, subset = d$validInvoice == 1)), NA)
                      expect_error(a <- ChurnByCohort(rd, volume = v), NA)
                      expect_false(is.null(attr(a, "detail")))
                      expect_error(p <- plot(a), NA)
                      expect_error(capture.output(print(p)), NA)
                  })