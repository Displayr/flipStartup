context("Growth Accounting")

data(q.invoice.lines)
d <- q.invoice.lines
by = "year"
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
    zprofiling <- d[match(unique.names, as.character(d$name)), ]
    expect_error(capture.output(rd <- RevenueData(d$AUD, d$ValidFrom, d$ValidTo, end = ISOdate(2016,06,14), id = d$name, by = by, subset = d$validInvoice == 1, profiling = zprofiling)))
    # Adding profiling variables with id shown in row names
    rownames(zprofiling) <- unique.names
    capture.output(rd <- RevenueData(d$AUD, d$ValidFrom, d$ValidTo, id = d$name, end = ISOdate(2016,06,14), by = by, subset = d$validInvoice == 1, profiling = zprofiling))
    # testing end
    capture.output(rd <- RevenueData(d$AUD, d$ValidFrom, d$ValidTo, end = ISOdate(2015, 5, 1), id = d$name, by = by, subset = d$validInvoice == 1, profiling = zprofiling))



    rg <- RevenueGrowthAccounting(rd)
    plot(rg)
    QuickRatioPlot(rg, 3)

    w <- Waterfall(rg)
    plot(w)
    w <- Waterfall(rg, names(rg$Revenue)[length(names(rg$Revenue)) - 1])
    plot(w)

    ## Churn
    plot(Churn(rd, volume = FALSE))
    plot(Churn(rd, volume = TRUE))
    plot(Acquisition(rd, volume = TRUE))

    plot(Subscribers(rd))

    #####################################
    ####  Retention                  ####
    #####################################

    Table(id ~ start.period, data = rd, FUN = function(x) length(unique(x)))
    Retention(rd)
    Growth(rd)
    LifetimeValue(rd)
})


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
