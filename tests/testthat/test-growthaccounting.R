context("Growth Accounting")

test_that("No error",
{
    library
    d <- q.invoice.lines
    rd <- RevenueData(d$AUD, d$ValidFrom, d$ValidTo, id = d$name, by = "year", subset = d$validInvoice == 1, trim.id = 20)

    rg <- RevenueGrowthAccounting(rd)
    plot(rg)
    QuickRatio(rg)


    #####################################
    ####  Retention                  ####
    #####################################

    # Number of firms
    n.by.start.period <- Table(id ~ start.period, data = rd, FUN = function(x) length(unique(x)))
    n.by.start.period

    Retention(rd)

    Growth(rd)

    LifetimeValue(rd)
})

