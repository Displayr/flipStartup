context("RecurringRevenue")
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
                  expect_error(capture.output(rd <- RevenueData(d$AUD, d$ValidFro, d$ValidTo, start = start, end = end, id = d$name, subscription.length = by, subset = d$validInvoice == 1)), NA)
                  expect_error(capture.output(print(RecurringRevenue(rd, by = by))), NA)
              })


dollars <- c(100, 100, 100, 100)
from <- as.Date(c("2016/01/01", "2016/06/01", "2016/08/01", "2016/10/01"))
to <- as.Date(c("2016/12/31", "2016/12/31", "2017/07/31", "2017/10/31"))
name <- c("A", "A", "B", "C")
test_that("Recurring Revenue", {
    # Years ending the day before 12 months
    capture.output(rd <- RevenueData(dollars, from, to, id = name, subscription.length = "year"))
    r <- RecurringRevenue(rd, by = "year")
    expect_equivalent(r["2016"], 100) # 2016
    expect_equivalent(r["2017"], 100 + 100 * 12/13, tolerance = .02) # 2017
    r <- RecurringRevenue(rd, by = "month")
    expect_equivalent(r["2016-01"], 100) # 2016
    expect_equivalent(r["2016-06"], 100 + 100 * (12/7), tolerance = .02) # 2016
    expect_equivalent(r["2016-08"], 100 + 100 * (12/7) + 100, tolerance = .02) # 2016
    expect_equivalent(r["2017-01"], 100 + 100 * 12/13, tolerance = .02) # 2017
    r <- RecurringRevenue(rd, by = "quarter")
    expect_equivalent(r["2016-01"], 100) # 2016
    expect_equivalent(r["2016-07"], 100 + 100 * (12/7), tolerance = .02) # 2016
    expect_equivalent(r["2016-10"], 100 + 100 * (12/7) + 100 + 100 * 12/13, tolerance = .02) # 2016
    expect_equivalent(r["2017-01"], 100 + 100 * 12/13, tolerance = .02) # 2017
    r <- RecurringRevenue(rd, by = "week")
    expect_equivalent(r["2016-01-03"], 100) # 2016
    expect_equivalent(r["2016-07-03"], 100 + 100 * (12/7), tolerance = .02) # 2016
    expect_equivalent(r["2016-10-02"], 100 + 100 * (12/7) + 100 + 100 * 12/13, tolerance = .02) # 2016
    expect_equivalent(r["2017-01-01"], 100 + 100 * 12/13, tolerance = .02) # 2017
})


