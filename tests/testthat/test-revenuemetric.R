context("Revenue Metric")
data(q.invoice.lines)
d <- q.invoice.lines
library(lubridate)
#Sys.setenv(TZ='GMT')
end <-  ISOdate(2016,2,15)
start <-  ISOdate(2012,7,1)

# merging categories
d$country <- as.character(d$country)
t <- table(d$country)
d$country[d$country %in% names(t[t < 100])] <- "Other"

d$salesman <- as.character(d$salesman)
t <- table(d$salesman)
d$salesman[d$salesman %in% names(t[t < 100])] <- "Other"

            
test_that("Create subsets",
          {
              # Country
              s <- flipStartup:::createFilters(d[, "country", drop = FALSE], subset = NULL, id = d$name)
              expect_equal(length(names(s)), 5)
              expect_equal(names(s)[1], "Australia\nn: 385")
              expect_equal(sum(s[[1]]), 2390) # Observations, companies can have multiple observations
              
              # Country - with a filter
              f <- d$salesman == "4"
              s <- flipStartup:::createFilters(d[, "country", drop = FALSE], subset = f, id = d$name)
              expect_equal(length(names(s)), 5)
              expect_equal(names(s)[1], "Australia\nn: 49")
              expect_equal(sum(s[[1]]), 504) 

              # Saleman
              s <- flipStartup:::createFilters(d[, "salesman", drop = FALSE], subset = NULL, id = d$name)
              expect_equal(length(names(s)), 4)
              expect_equal(names(s)[1], "MISSING DATA\nn: 486")
              
              # Country and salesman
              s <- flipStartup:::createFilters(d[, c("country", "salesman")], subset = NULL, id = d$name)
              expect_equal(length(names(s)), 20)
              expect_equal(names(s)[1], "Australia + MISSING DATA\nn: 191")
          })

by= "quarter"
for (by in c("month", "quarter", "year"))
  test_that(paste("Churn consistency", by), {
      z1 = RevenueMetric("CustomerChurn", output = "Table", volume = FALSE, d$AUD,d$ValidFrom,d$ValidTo, id = d$name, by = by)
      capture.output(rdd <- RevenueData(d$AUD,d$ValidFrom,d$ValidTo, id = d$name, subscription.length = "year"))
      r <- Retention(rdd, by = by)
      z2 = 1 - r$retention.rate.by.period
      expect_equal(z1[, 1], z2[rownames(z1)])
  })


set.seed(1223)
d <- d[d$validInvoice == 1, ]
d <- d[sample(1:nrow(d), 100), ]

# This is just checking for errors. Blog projects will be used for checking outputs.
fun = "RecurringRevenue"
out = "Table"
by = "month"
p.country <- d[, "country", drop = FALSE]
p.country.salesman <- d[, c("country", "salesman")]
for (fun in c("NewCustomers", "CustomerChurn", "RecurringRevenueChurn", "RecurringRevenue"))
    for (out in c("Table", "Plot", "Detail"))
        for (by in c("month", "quarter", "year"))
            test_that(paste("metrics", fun, out, by), 
                      {
                          capture.output({
              # Aggregate 
              s = RevenueMetric(FUN = fun, output = out, d$AUD,d$ValidFrom,d$ValidTo, id = d$name, by = by)
              expect_error(print(s), NA)
              # one profiling
              s = RevenueMetric(FUN = fun, output = out, d$AUD,d$ValidFrom,d$ValidTo, id = d$name, by = by, profiling = p.country)
              expect_error(print(s), NA)
              
              # two profiling
              s = RevenueMetric(fun, output = out, d$AUD, d$ValidFrom,d$ValidTo, id = d$name, by = by, profiling = p.country.salesman, )
              expect_error(print(s), NA)
          })
      })


