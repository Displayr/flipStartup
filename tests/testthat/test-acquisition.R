context("Growth Accounting")

library(flipStatistics)
data(q.invoice.lines)
d <- q.invoice.lines
by = "year"
library(lubridate)
today <- ISOdate(2016,6,30)
tz(today) <- "GMT"
today <- today + hours(12) - seconds(1)
for (by in c("month","quarter", "year"))

by <- "quarter"
#rd <- RevenueData(d$AUD, d$ValidFrom, d$ValidTo, end = ISOdate(2016,06,14), id = d$name, by = by, subset = d$validInvoice == 1)
zprofiling <- d[match(unique.names, as.character(d$name)), ]
rownames(zprofiling) <- unique.names
    unique.names <- sort(unique(d$name))
    zprofiling <- d[match(unique.names, as.character(d$name)), ]
    names(zprofiling)[match("name", names(zprofiling))] <- "id"
rd <- RevenueData(d$AUD, d$ValidFrom, d$ValidTo, end = ISOdate(2016,06,14), id = d$name, by = by, subset = d$validInvoice == 1, profiling = zprofiling)

TimeSeriesColumnChart(Acquisition(rd, volume = FALSE)$counts)#, tickformat = "%")
TimeSeriesColumnChart(Acquisition(rd, volume = TRUE)$counts)#, tickformat = "%")
