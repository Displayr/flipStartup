context("Growth Accounting")

data(q.invoice.lines)
d <- q.invoice.lines
by = "month"
for (by in c("month","quarter", "year"))
test_that(by,
{
    ## Annual
    # No profiling.
    expect_error(capture.output(rd <- RevenueData(d$AUD, d$ValidFrom, d$ValidTo, end = ISOdate(2016,06,14), id = d$name, by = by, subset = d$validInvoice == 1)), NA)

    # Adding profiling variables of the wrong dimension
    expect_error(capture.output(rd <- RevenueData(d$AUD, d$ValidFrom, d$ValidTo, end = ISOdate(2016,06,14), id = d$name, by = by, subset = d$validInvoice == 1, profiling = d)))
    # Adding profiling variables of the correct dimension
    unique.names <- sort(unique(d$name))
    zprofiling <- d[match(unique.names, d$name), ]
    expect_error(capture.output(rd <- RevenueData(d$AUD, d$ValidFrom, d$ValidTo, end = ISOdate(2016,06,14), id = d$name, by = by, subset = d$validInvoice == 1, profiling = zprofiling)), NA)
    # Adding profiling variables with id shown in row names
    rownames(zprofiling) <- unique.names
    capture.output(rd <- RevenueData(d$AUD, d$ValidFrom, d$ValidTo, id = d$name, end = ISOdate(2016,06,14), by = by, subset = d$validInvoice == 1, profiling = zprofiling))
    # testing end
    capture.output(rd <- RevenueData(d$AUD, d$ValidFrom, d$ValidTo, end = ISOdate(2015, 5, 1), id = d$name, end = ISOdate(2016,06,14), by = by, subset = d$validInvoice == 1, profiling = zprofiling))



    rg <- RevenueGrowthAccounting(rd)
    plot(rg)
    QuickRatioPlot(rg, 3)

    w <- Waterfall(rg)
    plot(w)
    w <- Waterfall(rg, names(rg$Revenue)[length(names(rg$Revenue)) - 1])
    plot(w)

    ## Churn
    plot(Churn(rd))
    plot(Churn(rd, volume = TRUE))

    #####################################
    ####  Retention                  ####
    #####################################

    Table(id ~ start.period, data = rd, FUN = function(x) length(unique(x)))
    Retention(rd)
    Growth(rd)
    LifetimeValue(rd)
})
