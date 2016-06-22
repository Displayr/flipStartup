context("Growth Accounting")

data(q.invoice.lines)
d <- q.invoice.lines
test_that("revenue data",
{
    # No profiling.
    expect_error(capture.output(rd <- RevenueData(d$AUD, d$ValidFrom, d$ValidTo, id = d$name, by = "year", subset = d$validInvoice == 1)), NA)
    # Adding profiling variables of the wrong dimension
    expect_error(capture.output(rd <- RevenueData(d$AUD, d$ValidFrom, d$ValidTo, id = d$name, by = "year", subset = d$validInvoice == 1, profiling = d)))
    # Adding profiling variables of the correct dimension
    unique.names <- sort(unique(d$name))
    zprofiling <- d[match(unique.names, d$name), ]
    expect_error(capture.output(rd <- RevenueData(d$AUD, d$ValidFrom, d$ValidTo, id = d$name, by = "year", subset = d$validInvoice == 1, profiling = zprofiling)), NA)
    # Adding profiling variables with id shown in row names
    rownames(zprofiling) <- unique.names
    capture.output(rd <- RevenueData(d$AUD, d$ValidFrom, d$ValidTo, id = d$name, by = "year", subset = d$validInvoice == 1, profiling = zprofiling))



    rg <- RevenueGrowthAccounting(rd)
    plot(rg)
    p <- QuickRatioPlot(rg, 3)

    Waterfall(rg)
    Waterfall(rg, "2010")
    Waterfall(rg, "2015")
    #####################################
    ####  Retention                  ####
    #####################################

    Table(id ~ start.period, data = rd, FUN = function(x) length(unique(x)))
    Retention(rd)
    Growth(rd)
    LifetimeValue(rd)
})
