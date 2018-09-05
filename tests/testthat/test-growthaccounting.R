context("Growth Accounting")

library(flipStatistics)
data(q.invoice.lines)
d <- q.invoice.lines
by = "year"
library(lubridate)
today <- ISOdate(2016,6,30)

test_that("Growth accounting",
{
    rd <- RevenueData(d$AUD, d$ValidFrom, d$ValidTo, end = ISOdate(2016,06,14), id = d$name, subset = d$validInvoice == 1)
    rg <- RevenueGrowthAccounting(rd, remove.last = FALSE)
    expect_equal(sum(rd$value), sum(rg$data$value))
    expect_equal(length(table(table(rg$data$id))), 1) # all companies should have entries for each year
})

# test_that("Financial year",
# {
#     unique.names <- sort(unique(d$name))
#     zprofiling <- d[match(unique.names, as.character(d$name)), ]
#     expect_error(capture.output(rd <- RevenueData(d$AUD, d$ValidFrom, d$ValidTo, end = ISOdate(2016,06,14), id = d$name, by = by, subset = d$validInvoice == 1, profiling = zprofiling)))
#     rownames(zprofiling) <- unique.names
# 
#     library(lubridate)
#     from <- d$ValidFrom %m+% months(6)
#     to <- d$ValidTo %m+% months(6)
#     end <- today %m+% months(6) + hours(23) + minutes(59) + seconds(59)
#     twelveMonthsAgo <- ISOdate(2015, 7, 1)
#     #tz(twelveMonthsAgo) <- "GMT"
#     twelveMonthsAgo <- twelveMonthsAgo - hours(12)
#     yearEnd <- ISOdate(2016, 7, 1) - seconds(1) + hours(12)
#     #tz(yearEnd) <- "GMT"
# 
#     capture.output(rd <- RevenueData(d$AUD, from , to, end = end, id = d$name,
#         by = "year", subset = d$validInvoice == 1, profiling = zprofiling))
#     annual.from.annual <- Growth(rd, FALSE)$revenue[8]
# 
#     offset = lubridate::days(180)
#     calculated.from.filter <- sum(rd$value[rd$from - offset >= twelveMonthsAgo  & rd$value - offset <= yearEnd])
#     expect_equal(as.numeric(annual.from.annual), calculated.from.filter)
# 
#     # Computing revenue using monthly calculations.
#     capture.output(rdm <- RevenueData(d$AUD,  d$ValidFrom , d$ValidTo, end = today, id = d$name,
#         by = "month", subset = d$validInvoice == 1, profiling = zprofiling))
# 
#     calculated.from.filter <- sum(rdm$value[rdm$from >= twelveMonthsAgo & rdm$value <= yearEnd])
#     expect_equal(as.numeric(annual.from.annual), calculated.from.filter)
# 
#     expect_error(Revenue(rdm[rdm$from >= twelveMonthsAgo & rdm$value <= yearEnd, ], end = today, by = "month"), NA)
# 
# 
#     rv <- Revenue(rdm, end = today, by = "month")
#     #rv[length(rv)]
#     #rv
# 
#     annual.from.annual - rv[length(rv)]
#     expect_error(g <- Growth(rd, FALSE), NA)
# })


