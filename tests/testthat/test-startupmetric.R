context("Startup Metric")
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

# create Subsets
test_that("Create subsets",
          {
              # Country
              s <- flipStartup:::createFilters(d[, "country", drop = FALSE], subset = NULL, id = d$name)
              expect_equal(length(names(s)), 5)
              expect_equal(names(s)[1], "Australia\nn: 385")
              expect_equal(sum(s[[1]]), 2390) # Observations, companies can have multiple observations
              # Saleman
              s <- flipStartup:::createFilters(d[, "salesman", drop = FALSE], subset = NULL, id = d$name)
              expect_equal(length(names(s)), 4)
              expect_equal(names(s)[1], "MISSING DATA\nn: 486")
              # Country and salesman
              s <- flipStartup:::createFilters(d[, c("country", "salesman")], subset = NULL, id = d$name)
              expect_equal(length(names(s)), 20)
              expect_equal(names(s)[1], "Australia + MISSING DATA\nn: 191")
          })

# Aggretate acuisition plot
StartupMetric(FUN = "Acquisition", output = "List", d$AUD,d$ValidFrom,d$ValidTo, id = d$name, subscription.length = "quarter", subset = d$validInvoice == 1)
StartupMetric(FUN = "Acquisition", output = "Table", d$AUD,d$ValidFrom,d$ValidTo, id = d$name, subscription.length = "quarter", subset = d$validInvoice == 1)
StartupMetric(FUN = "Acquisition", output = "Plot", d$AUD,d$ValidFrom,d$ValidTo, id = d$name, subscription.length = "quarter", subset = d$validInvoice == 1)

# one profiling
p <- d[, "country", drop = FALSE]
StartupMetric(FUN = "Acquisition", output = "List", d$AUD,d$ValidFrom,d$ValidTo, id = d$name, subscription.length = "quarter", profiling = p, subset = d$validInvoice == 1)
StartupMetric(FUN = "Acquisition", output = "Table", d$AUD,d$ValidFrom,d$ValidTo, id = d$name, subscription.length = "quarter", profiling = p, subset = d$validInvoice == 1)
StartupMetric(FUN = "Acquisition", output = "Plot", d$AUD,d$ValidFrom,d$ValidTo, id = d$name, subscription.length = "quarter", profiling = p, subset = d$validInvoice == 1)

# two  profiling
p <- d[, c("country", "salesman")]
StartupMetric(FUN = "Acquisition", output = "List", d$AUD,d$ValidFrom,d$ValidTo, id = d$name, subscription.length = "quarter", profiling = p, subset = d$validInvoice == 1)
StartupMetric(FUN = "Acquisition", output = "Table", d$AUD,d$ValidFrom,d$ValidTo, id = d$name, subscription.length = "quarter", profiling = p, subset = d$validInvoice == 1)
StartupMetric(FUN = "Acquisition", output = "Plot", d$AUD,d$ValidFrom,d$ValidTo, id = d$name, subscription.length = "quarter", profiling = p, subset = d$validInvoice == 1)


# 
# 
# path <- "C:/Users/tim/Dropbox (Numbers)/Planning/Forecasts"
# data.for.cox <- read.csv(paste0(path, "/DataForCox.csv"))
# 
# 
# library("readxl")
# path <- "C:/Users/tim/Dropbox (Numbers)/Planning/Forecasts"
# z <- read_excel(paste0(path, "/Combined.revenue.data.weekly.xlsx"))
# 
# # Cleaning
# z$from <- flipTime::AsDate(z$from)
# z$to <- flipTime::AsDate(z$to)
# head(z[order(z$value), ])
# z = z[z$id != "_jennifer", ]
# flipStartup:::createFilters(profiling = z[1:10, "country"], TRUE)
