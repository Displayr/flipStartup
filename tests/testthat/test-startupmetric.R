context("Startup Metric")
data(q.invoice.lines)
d <- q.invoice.lines
library(lubridate)
#Sys.setenv(TZ='GMT')
end <-  ISOdate(2016,2,15)
start <-  ISOdate(2012,7,1)

StartupMetric(d$AUD,d$ValidFrom,d$ValidTo, id = d$name, subscription.length = "quarter", subset = d$validInvoice == 1)





path <- "C:/Users/tim/Dropbox (Numbers)/Planning/Forecasts"
data.for.cox <- read.csv(paste0(path, "/DataForCox.csv"))


library("readxl")
path <- "C:/Users/tim/Dropbox (Numbers)/Planning/Forecasts"
z <- read_excel(paste0(path, "/Combined.revenue.data.weekly.xlsx"))

# Cleaning
z$from <- flipTime::AsDate(z$from)
z$to <- flipTime::AsDate(z$to)
head(z[order(z$value), ])
z = z[z$id != "_jennifer", ]
flipStartup:::createFilters(profiling = z[1:10, "country"], TRUE)
