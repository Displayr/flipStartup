context("Growth Accounting")

data(q.invoice.lines)
d <- q.invoice.lines
test_that("revenue data",
{
    expect_error(capture.output(rd <- RevenueData(d$AUD, d$ValidFrom, d$ValidTo, id = d$name, by = "year", subset = d$validInvoice == 1)), NA)
    expect_error(rg <- RevenueGrowthAccounting(rd), NA)
    expect_error(p <- plot(rg), NA)
    p
    expect_error(p <- QuickRatioPlot(rg, 3), NA)
    p

    #####################################
    ####  Retention                  ####
    #####################################

    # Number of firms
    expect_error(n.by.start.period <- Table(id ~ start.period, data = rd, FUN = function(x) length(unique(x))), NA)
    expect_error(n.by.start.period, NA)

    expect_error(Retention(rd), NA)

    expect_error(Growth(rd), NA)

    expect_error(LifetimeValue(rd), NA)
})

