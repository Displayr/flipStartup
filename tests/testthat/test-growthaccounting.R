context("Growth Accounting")

library(flipStatistics)
data(q.invoice.lines)
d <- q.invoice.lines
by = "year"
library(lubridate)
today <- ISOdate(2016,6,30)
tz(today) <- "GMT"



test_that("Financial year",
{
    unique.names <- sort(unique(d$name))
    zprofiling <- d[match(unique.names, as.character(d$name)), ]
    expect_error(capture.output(rd <- RevenueData(d$AUD, d$ValidFrom, d$ValidTo, end = ISOdate(2016,06,14), id = d$name, by = by, subset = d$validInvoice == 1, profiling = zprofiling)))
    rownames(zprofiling) <- unique.names

    library(lubridate)
    from <- d$ValidFrom %m+% months(6)
    to <- d$ValidTo %m+% months(6)
    end <- today %m+% months(6) + hours(23) + minutes(59) + seconds(59)
    twelveMonthsAgo <- ISOdate(2015, 7, 1)
    tz(twelveMonthsAgo) <- "GMT"
    twelveMonthsAgo <- twelveMonthsAgo - hours(12)
    yearEnd <- ISOdate(2016, 7, 1) - seconds(1) + hours(12)
    tz(yearEnd) <- "GMT"

    capture.output(rd <- RevenueData(d$AUD, from , to, end = end, id = d$name,
        by = "year", subset = d$validInvoice == 1, profiling = zprofiling))
    annual.from.annual <- Growth(rd, FALSE)$revenue[8]

    offset = 180
    calculated.from.filter <- sum(rd$value[rd$from - offset >= twelveMonthsAgo & rd$value - offset <= yearEnd])
    expect_equal(as.numeric(annual.from.annual), calculated.from.filter)

    # Computing revenue using monthly calculations.
    capture.output(rdm <- RevenueData(d$AUD,  d$ValidFrom , d$ValidTo, end = today, id = d$name,
        by = "month", subset = d$validInvoice == 1, profiling = zprofiling))

    calculated.from.filter <- sum(rdm$value[rdm$from >= twelveMonthsAgo & rdm$value <= yearEnd])
    expect_equal(as.numeric(annual.from.annual), calculated.from.filter)

    expect_error(Revenue(rdm[rdm$from >= twelveMonthsAgo & rdm$value <= yearEnd, ], end = today), NA)


    rv <- Revenue(rdm, end = today)
    #rv[length(rv)]
    #rv

    annual.from.annual - rv[length(rv)]
    expect_error(g <- Growth(rd, FALSE), NA)

    # Growth(rd, FALSE)
    #
    #
    # sum(rd$value[rd$from >= twelveMonthsAgo & rd$value <= yearEnd])
    # Table(value ~ from, data = rd, sum)
    #
    # z <- Table(value ~ from, data = rdm, sum)
    # k <- length(z)
    # sum(z[(k-11):k])
    #
    #
    # table(rd$from)
    # rg <- RevenueGrowthAccounting(rd, remove.last = FALSE)
    # plot(rg)
    # QuickRatioPlot(rg, 3)
    #
    #
    # Growth(rd, FALSE)
    # w <- Waterfall(rg)
    # plot(w)
    # w <- Waterfall(rg, names(rg$Revenue)[length(names(rg$Revenue)) - 1])
    # plot(w)
    #
    # ## Churn
    # plot(Acquisition(rd, volume = TRUE))
    #
    # plot(Subscribers(rd))
    # plot(Revenue(rd))
    #
    # #####################################
    # ####  Retention                  ####
    # #####################################
    #
    # Table(id ~ relationship.from.period, data = rd, FUN = function(x) length(unique(x)))
    # Retention(rd)
    # Growth(rd)
    # suppressWarnings(LifetimeValue(rd))
})


# today <- today + hours(12) - seconds(1)
# for (by in c("month","quarter", "year"))
# test_that(by,
# {
#     ## Annual
#     # No profiling.
#     expect_error(capture.output(rd <- RevenueData(d$AUD, d$ValidFrom, d$ValidTo, end = ISOdate(2016,06,14), id = d$name, by = by, subset = d$validInvoice == 1)), NA)
#
#     # Adding profiling variables of the wrong dimension
#     expect_error(capture.output(rd <- RevenueData(d$AUD, d$ValidFrom, d$ValidTo, end = ISOdate(2016,06,14), id = d$name, by = by, subset = d$validInvoice == 1, profiling = d)))
#     # Adding profiling variables of the correct dimension
#     unique.names <- sort(unique(d$name))
#     zprofiling <- d[match(unique.names, as.character(d$name)), ]
#     expect_error(capture.output(rd <- RevenueData(d$AUD, d$ValidFrom, d$ValidTo, end = ISOdate(2016,06,14), id = d$name, by = by, subset = d$validInvoice == 1, profiling = zprofiling)))
#     # Adding profiling variables with id shown in row names
#     rownames(zprofiling) <- unique.names
#     capture.output(rd <- RevenueData(d$AUD, d$ValidFrom, d$ValidTo, id = d$name, end = ISOdate(2016,11,1), by = by, subset = d$validInvoice == 1, profiling = zprofiling))
#     # testing end
#     capture.output(rd <- RevenueData(d$AUD, d$ValidFrom, d$ValidTo, end = today, id = d$name, by = by, subset = d$validInvoice == 1, profiling = zprofiling))
#
#     rg <- RevenueGrowthAccounting(rd)
#     plot(rg)
#     QuickRatioPlot(rg, 3)
#
#     w <- Waterfall(rg)
#     plot(w)
#     w <- Waterfall(rg, names(rg$Revenue)[length(names(rg$Revenue)) - 1])
#     plot(w)
#
#     ## Churn
#     TimeSeriesColumnChart(Churn(rd, volume = FALSE)$rates, tickformat = "%")
#     TimeSeriesColumnChart(Churn(rd, volume = TRUE)$rates, tickformat = "%")
#     TimeSeriesColumnChart(Churn(rd, volume = TRUE)$rates, tickformat = "%")
#     TimeSeriesColumnChart(Acquisition(rd, volume = FALSE)$counts)#, tickformat = "%")
#     TimeSeriesColumnChart(Acquisition(rd, volume = TRUE)$counts)#, tickformat = "%")
#
#     plot(Subscribers(rd, by = "month"))
#     plot(Revenue(rd))
#
#     #####################################
#     ####  Retention                  ####
#     #####################################
#
#     flipStatistics::Table(id ~ relationship.from.period, data = rd, FUN = function(x) length(unique(x)))
#     Retention(rd)
#     Growth(rd)
#     if (by == "annual")
#         LifetimeValue(rd)
# })
#
#




#

# library(devtools)
# install_github("NumbersInternational/flipExampleData")
# install_github("NumbersInternational/flipU")
# install_github("NumbersInternational/flipTransformations")
# install_github("NumbersInternational/flipImputation")
# install_github("NumbersInternational/flipData")
# install_github("NumbersInternational/flipFormat")
# install_github("NumbersInternational/flipStartup")
# install_github("NumbersInternational/flipTrees")
# library(flipTrees)
# library(flipStartup)
#
# data(q.invoice.lines)
# d <- q.invoice.lines
# unique.names <- sort(unique(d$name))
# zprofiling <- d[match(unique.names, as.character(d$name)), ]
# zprofiling$id <- zprofiling$name
# rd <- RevenueData(d$AUD, d$ValidFrom, d$ValidTo, end = ISOdate(2015, 5, 1), id = d$name, by = "year", subset = d$validInvoice == 1, profiling = zprofiling)
# output = "Text"
# CART(churn ~ salesman + ComputersToRenew + country, data = rd, output = output)
# CART(as.integer(churn) ~ salesman + ComputersToRenew + country, data = rd, output = output)
# CART(churn ~ currency, data = rd, output = output)
# CART(as.integer(churn) ~ currency, data = rd, output = output)
# output = "Sankey"
# CART(churn ~ salesman + ComputersToRenew + country, data = rd, output = output)
# CART(as.integer(churn) ~ salesman + ComputersToRenew + country, data = rd, output = output)
# CART(churn ~ currency, data = rd, output = output)
# CART(as.integer(churn) ~ currency, data = rd, output = output)

