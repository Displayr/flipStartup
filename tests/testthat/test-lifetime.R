context("lifetime")
data(q.invoice.lines)
d <- q.invoice.lines
library(lubridate)
#Sys.setenv(TZ='GMT')
end <-  ISOdate(2016,6,30)
start <-  ISOdate(2012,7,1)
by = "year"
for (by in c("month", "year"))
    test_that(paste("Creating RevenueData", by),
          {
#            expect_error(capture.output(
                
                
                rd <- RevenueData(d$AUD, d$ValidFrom, d$ValidTo, end = end, id = d$name, subscription.length = by, subset = d$validInvoice == 1)
                z = rd[rd$observation.within.period == 1,]
                rd[364 > (z$to - z$from), ]
                #)
                Lifetime(rd)
                
                
 #               , NA)
            expect_error(r <- Lifetime(rd), NA)
            expect_error(capture.output(print(r)), NA)
})


