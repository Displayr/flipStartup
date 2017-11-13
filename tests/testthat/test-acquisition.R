context("acquisition ")
data(q.invoice.lines)
d <- q.invoice.lines
library(flipStandardCharts)
library(lubridate)
#Sys.setenv(TZ='GMT')
end <-  ISOdate(2016,12,31)
start <-  ISOdate(2012,7,1)
by = "month"
for (by in c("week", "month", "quarter", "year"))
    test_that(paste("Creating RevenueData", by),
          {
            expect_error(capture.output(rd <- RevenueData(d$AUD, d$ValidFrom, d$ValidTo, start = start, end = end, id = d$name, subscription.length = by, subset = d$validInvoice == 1)), NA)
            expect_error(a <- Acquisition(rd, volume = FALSE), NA)
<<<<<<< HEAD
            expect_error(p <- flipTime::TimeSeriesColumnChart(a$counts, a$subscription.length, ytitle = "New customers"), NA)
            expect_error(print(p), NA)
            expect_error(a <- Acquisition(rd, volume = FALSE, number.periods = 12), NA)
            expect_error(p <- flipTime::TimeSeriesColumnChart(a$counts, a$subscription.length, ytitle = "New customers"), NA)
            expect_error(print(p), NA)
            expect_error(a <- Acquisition(rd, volume = TRUE), NA)
            expect_error(p <- flipTime::TimeSeriesColumnChart(a$counts, a$subscription.length, ytitle = "New customers"), NA)
=======
            expect_error(p <- Chart(a$counts, y.title = "New customers", fit.type="Smooth"), NA)
            expect_error(print(p), NA)
            expect_error(a <- Acquisition(rd, volume = FALSE, number.periods = 12), NA)
            expect_error(p <- Chart(a$counts, y.title = "New customers", fit.type="Smooth"), NA)
            expect_error(print(p), NA)
            expect_error(a <- Acquisition(rd, volume = TRUE), NA)
            expect_error(p <- Chart(a$counts, y.title = "New customers", fit.type="Smooth"), NA)
>>>>>>> origin/master
            expect_error(capture.output(print(p)), NA)
})







# context("Growth Accounting")
#
# library(flipStatistics)
# data(q.invoice.lines)
# d <- q.invoice.lines
# by = "year"
# library(lubridate)
# today <- ISOdate(2016,6,30)
# tz(today) <- "GMT"
# today <- today + hours(12) - seconds(1)
# for (by in c("month","quarter", "year"))
#
# by <- "quarter"
# #rd <- RevenueData(d$AUD, d$ValidFrom, d$ValidTo, end = ISOdate(2016,06,14), id = d$name, by = by, subset = d$validInvoice == 1)
# zprofiling <- d[match(unique.names, as.character(d$name)), ]
# rownames(zprofiling) <- unique.names
#     unique.names <- sort(unique(d$name))
#     zprofiling <- d[match(unique.names, as.character(d$name)), ]
#     names(zprofiling)[match("name", names(zprofiling))] <- "id"
# rd <- RevenueData(d$AUD, d$ValidFrom, d$ValidTo, end = ISOdate(2016,06,14), id = d$name, by = by, subset = d$validInvoice == 1, profiling = zprofiling)
#
# TimeSeriesColumnChart(Acquisition(rd, volume = FALSE)$counts)#, tickformat = "%")
# TimeSeriesColumnChart(Acquisition(rd, volume = TRUE)$counts)#, tickformat = "%")
