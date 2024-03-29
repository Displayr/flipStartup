context("Revenue data")
data(q.invoice.lines)
d <- q.invoice.lines
library(lubridate)
#Sys.setenv(TZ='GMT')
end <-  ISOdate(2016,2,15)
start <-  ISOdate(2012,7,1)

for (by in c("week", "month", "quarter", "year"))
    test_that(paste("Creating RevenueData", by),
          {
            # Beginning and end
            expect_error(capture.output(rd <- RevenueData(d$AUD, d$ValidFrom, d$ValidTo, id = d$name, subscription.length = by, subset = d$validInvoice == 1)), NA)
            expect_error(capture.output(RevenueGrowthAccounting(rd)), NA)
            
            # Subset
            expect_error(capture.output(rd <- RevenueData(d$AUD, d$ValidFrom, d$ValidTo, id = d$InvoiceLineID, subscription.length = by, subset = d$validInvoice == 1)), NA)
            expect_equal(length(which(as.character(d$InvoiceLineID[which(d$validInvoice == 0)]) %in% rd$id)), 0)

            expect_error(capture.output(rd <- RevenueData(d$AUD, d$ValidFrom, d$ValidTo, start = start, id = d$name, subscription.length = by, subset = d$validInvoice == 1)), NA)
            expect_error(capture.output(RevenueGrowthAccounting(rd)), NA)

            expect_error(capture.output(rd <- RevenueData(d$AUD, d$ValidFrom, d$ValidTo, end = end, id = d$name, subscription.length = by, subset = d$validInvoice == 1)), NA)
            expect_error(capture.output(RevenueGrowthAccounting(rd)), NA)

            expect_error(capture.output(rd <- RevenueData(d$AUD, d$ValidFrom, d$ValidTo, start = start, end = end, id = d$name, subscription.length = by, subset = d$validInvoice == 1)), NA)
            expect_error(capture.output(RevenueGrowthAccounting(rd)), NA)

            # Adding profiling variables of the wrong dimension
            expect_error(capture.output(rd <- RevenueData(d$AUD, d$ValidFrom, d$ValidTo, end = ISOdate(2016,06,14), id = d$name, subscription.length = by, subset = d$validInvoice == 1, profiling = d)))
            expect_error(capture.output(RevenueGrowthAccounting(rd)), NA)

            # Adding profiling variables of the correct dimension
            unique.names <- sort(unique(d$name))
            zprofiling <- d[match(unique.names, as.character(d$name)), ]
            expect_error(capture.output(rd <- RevenueData(d$AUD, d$ValidFrom, d$ValidTo, end = ISOdate(2016,06,14), id = d$name, subscription.length = by, subset = d$validInvoice == 1, profiling = zprofiling)))
            expect_error(capture.output(RevenueGrowthAccounting(rd)), NA)

            # Adding profiling variables with id shown in row names
            rownames(zprofiling) <- unique.names
            capture.output(rd <- RevenueData(d$AUD, d$ValidFrom, d$ValidTo, id = d$name, end = ISOdate(2016,11,1), subscription.length = by, subset = d$validInvoice == 1, profiling = zprofiling))
            expect_error(capture.output(RevenueGrowthAccounting(rd)), NA)
})


test_that("Displayr", {

    z <- data.frame(
        value = c(3999, 7998, 3999, 7998),
        from = structure(c(1515070800, 1512460800, 1507791600, 1503532800), class = c("POSIXct", "POSIXt", "QDate")),
        to = structure(c(1546606800, 1543996800, 1539327600, 1535068800), class = c("POSIXct", "POSIXt", "QDate")),
        subscription.length = structure(c(1L, 1L, 1L, 1L), .Label = "month", class = "factor"),
        subset = c(TRUE, TRUE, TRUE, TRUE),
        id = LETTERS[1:4],
        start = structure(c(1503532800, 1503532800, 1503532800, 1503532800), class = c("POSIXct", "POSIXt")),
        end = structure(c(17564, 17564, 17564, 17564), class = "Date")
    )
    expect_error(DisplayrTotalPaymentMonthlyRevenueData <- RevenueData(value = z$value,
        from = as.Date(z$from),
        to = as.Date(z$end),
        subscription.length = "month",
        #subset = z$filter,
        id = z$id,
        start = min(z$start, na.rm = TRUE),
        end = "2023-01-01"),
        "All the time arguments"
    )
    expect_error(DisplayrTotalPaymentMonthlyRevenueData <- RevenueData(value = z$value,
        from = as.Date(z$from),
        to = as.Date(z$end),
        subscription.length = "month",
        #subset = z$filter,
        id = z$id,
        start = min(z$start, na.rm = TRUE),
        end = as.POSIXct(Sys.Date())),
        NA
    )

    z = structure(list(value = structure(c(NA, NA, NA, NA, NA, 3999, 
        NA, NA, 7998, NA, 1998.86426651879, NA, NA, NA, NA, NA, 3999, 
        NA, NA, NA, NA, NA, 7998), questiontype = "Number", name = "TotalAnnualLicenceValue", label = "TotalAnnualLicenceValue", question = "TotalAnnualLicenceValue"), 
        from = structure(c(NA, NA, NA, NA, NA, 1515070800, NA, NA, 
        1512460800, NA, 1511787600, NA, NA, NA, NA, NA, 1507791600, 
        NA, NA, NA, NA, NA, 1503532800), class = c("POSIXct", "POSIXt", 
        "QDate"), QDate = structure(c(NA, NA, NA, NA, NA, 134L, NA, 
        NA, 104L, NA, 96L, NA, NA, NA, NA, NA, 50L, NA, NA, NA, NA, 
        NA, 1L), class = c("ordered", "factor"), .Label = c("8/24/2017", 
        "8/25/2017", "8/26/2017", "8/27/2017", "8/28/2017", "8/29/2017", 
        "8/30/2017", "8/31/2017", "9/1/2017", "9/2/2017", "9/3/2017", 
        "9/4/2017", "9/5/2017", "9/6/2017", "9/7/2017", "9/8/2017", 
        "9/9/2017", "9/10/2017", "9/11/2017", "9/12/2017", "9/13/2017", 
        "9/14/2017", "9/15/2017", "9/16/2017", "9/17/2017", "9/18/2017", 
        "9/19/2017", "9/20/2017", "9/21/2017", "9/22/2017", "9/23/2017", 
        "9/24/2017", "9/25/2017", "9/26/2017", "9/27/2017", "9/28/2017", 
        "9/29/2017", "9/30/2017", "10/1/2017", "10/2/2017", "10/3/2017", 
        "10/4/2017", "10/5/2017", "10/6/2017", "10/7/2017", "10/8/2017", 
        "10/9/2017", "10/10/2017", "10/11/2017", "10/12/2017", "10/13/2017", 
        "10/14/2017", "10/15/2017", "10/16/2017", "10/17/2017", "10/18/2017", 
        "10/19/2017", "10/20/2017", "10/21/2017", "10/22/2017", "10/23/2017", 
        "10/24/2017", "10/25/2017", "10/26/2017", "10/27/2017", "10/28/2017", 
        "10/29/2017", "10/30/2017", "10/31/2017", "11/1/2017", "11/2/2017", 
        "11/3/2017", "11/4/2017", "11/5/2017", "11/6/2017", "11/7/2017", 
        "11/8/2017", "11/9/2017", "11/10/2017", "11/11/2017", "11/12/2017", 
        "11/13/2017", "11/14/2017", "11/15/2017", "11/16/2017", "11/17/2017", 
        "11/18/2017", "11/19/2017", "11/20/2017", "11/21/2017", "11/22/2017", 
        "11/23/2017", "11/24/2017", "11/25/2017", "11/26/2017", "11/27/2017", 
        "11/28/2017", "11/29/2017", "11/30/2017", "12/1/2017", "12/2/2017", 
        "12/3/2017", "12/4/2017", "12/5/2017", "12/6/2017", "12/7/2017", 
        "12/8/2017", "12/9/2017", "12/10/2017", "12/11/2017", "12/12/2017", 
        "12/13/2017", "12/14/2017", "12/15/2017", "12/16/2017", "12/17/2017", 
        "12/18/2017", "12/19/2017", "12/20/2017", "12/21/2017", "12/22/2017", 
        "12/23/2017", "12/24/2017", "12/25/2017", "12/26/2017", "12/27/2017", 
        "12/28/2017", "12/29/2017", "12/30/2017", "12/31/2017", "1/1/2018", 
        "1/2/2018", "1/3/2018", "1/4/2018")), questiontype = "Date", name = "StartDate", label = "StartDate", question = "StartDate"), 
        to = structure(c(NA, NA, NA, NA, NA, 1546606800, NA, NA, 
        1543996800, NA, 1527426000, NA, NA, NA, NA, NA, 1539327600, 
        NA, NA, NA, NA, NA, 1535068800), class = c("POSIXct", "POSIXt", 
        "QDate"), QDate = structure(c(NA, NA, NA, NA, NA, 223L, NA, 
        NA, 193L, NA, 1L, NA, NA, NA, NA, NA, 139L, NA, NA, NA, NA, 
        NA, 90L), class = c("ordered", "factor"), .Label = c("5/27/2018", 
        "5/28/2018", "5/29/2018", "5/30/2018", "5/31/2018", "6/1/2018", 
        "6/2/2018", "6/3/2018", "6/4/2018", "6/5/2018", "6/6/2018", 
        "6/7/2018", "6/8/2018", "6/9/2018", "6/10/2018", "6/11/2018", 
        "6/12/2018", "6/13/2018", "6/14/2018", "6/15/2018", "6/16/2018", 
        "6/17/2018", "6/18/2018", "6/19/2018", "6/20/2018", "6/21/2018", 
        "6/22/2018", "6/23/2018", "6/24/2018", "6/25/2018", "6/26/2018", 
        "6/27/2018", "6/28/2018", "6/29/2018", "6/30/2018", "7/1/2018", 
        "7/2/2018", "7/3/2018", "7/4/2018", "7/5/2018", "7/6/2018", 
        "7/7/2018", "7/8/2018", "7/9/2018", "7/10/2018", "7/11/2018", 
        "7/12/2018", "7/13/2018", "7/14/2018", "7/15/2018", "7/16/2018", 
        "7/17/2018", "7/18/2018", "7/19/2018", "7/20/2018", "7/21/2018", 
        "7/22/2018", "7/23/2018", "7/24/2018", "7/25/2018", "7/26/2018", 
        "7/27/2018", "7/28/2018", "7/29/2018", "7/30/2018", "7/31/2018", 
        "8/1/2018", "8/2/2018", "8/3/2018", "8/4/2018", "8/5/2018", 
        "8/6/2018", "8/7/2018", "8/8/2018", "8/9/2018", "8/10/2018", 
        "8/11/2018", "8/12/2018", "8/13/2018", "8/14/2018", "8/15/2018", 
        "8/16/2018", "8/17/2018", "8/18/2018", "8/19/2018", "8/20/2018", 
        "8/21/2018", "8/22/2018", "8/23/2018", "8/24/2018", "8/25/2018", 
        "8/26/2018", "8/27/2018", "8/28/2018", "8/29/2018", "8/30/2018", 
        "8/31/2018", "9/1/2018", "9/2/2018", "9/3/2018", "9/4/2018", 
        "9/5/2018", "9/6/2018", "9/7/2018", "9/8/2018", "9/9/2018", 
        "9/10/2018", "9/11/2018", "9/12/2018", "9/13/2018", "9/14/2018", 
        "9/15/2018", "9/16/2018", "9/17/2018", "9/18/2018", "9/19/2018", 
        "9/20/2018", "9/21/2018", "9/22/2018", "9/23/2018", "9/24/2018", 
        "9/25/2018", "9/26/2018", "9/27/2018", "9/28/2018", "9/29/2018", 
        "9/30/2018", "10/1/2018", "10/2/2018", "10/3/2018", "10/4/2018", 
        "10/5/2018", "10/6/2018", "10/7/2018", "10/8/2018", "10/9/2018", 
        "10/10/2018", "10/11/2018", "10/12/2018", "10/13/2018", "10/14/2018", 
        "10/15/2018", "10/16/2018", "10/17/2018", "10/18/2018", "10/19/2018", 
        "10/20/2018", "10/21/2018", "10/22/2018", "10/23/2018", "10/24/2018", 
        "10/25/2018", "10/26/2018", "10/27/2018", "10/28/2018", "10/29/2018", 
        "10/30/2018", "10/31/2018", "11/1/2018", "11/2/2018", "11/3/2018", 
        "11/4/2018", "11/5/2018", "11/6/2018", "11/7/2018", "11/8/2018", 
        "11/9/2018", "11/10/2018", "11/11/2018", "11/12/2018", "11/13/2018", 
        "11/14/2018", "11/15/2018", "11/16/2018", "11/17/2018", "11/18/2018", 
        "11/19/2018", "11/20/2018", "11/21/2018", "11/22/2018", "11/23/2018", 
        "11/24/2018", "11/25/2018", "11/26/2018", "11/27/2018", "11/28/2018", 
        "11/29/2018", "11/30/2018", "12/1/2018", "12/2/2018", "12/3/2018", 
        "12/4/2018", "12/5/2018", "12/6/2018", "12/7/2018", "12/8/2018", 
        "12/9/2018", "12/10/2018", "12/11/2018", "12/12/2018", "12/13/2018", 
        "12/14/2018", "12/15/2018", "12/16/2018", "12/17/2018", "12/18/2018", 
        "12/19/2018", "12/20/2018", "12/21/2018", "12/22/2018", "12/23/2018", 
        "12/24/2018", "12/25/2018", "12/26/2018", "12/27/2018", "12/28/2018", 
        "12/29/2018", "12/30/2018", "12/31/2018", "1/1/2019", "1/2/2019", 
        "1/3/2019", "1/4/2019")), questiontype = "Date", name = "EndDate", label = "EndDate", question = "EndDate"), 
        subscription.length = structure(c(1L, 1L, 1L, 1L, 1L, 1L, 
        1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 
        1L, 1L), class = "factor", .Label = "month"), subset = c(FALSE, 
        FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, FALSE, 
        FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, 
        FALSE, FALSE, FALSE, TRUE), id =  LETTERS[c(1,1,1,1:20)], start = structure(c(1503532800, 
        1503532800, 1503532800, 1503532800, 1503532800, 1503532800, 
        1503532800, 1503532800, 1503532800, 1503532800, 1503532800, 
        1503532800, 1503532800, 1503532800, 1503532800, 1503532800, 
        1503532800, 1503532800, 1503532800, 1503532800, 1503532800, 
        1503532800, 1503532800), class = c("POSIXct", "POSIXt")), 
        end = structure(c(17564, 17564, 17564, 17564, 17564, 17564, 
        17564, 17564, 17564, 17564, 17564, 17564, 17564, 17564, 17564, 
        17564, 17564, 17564, 17564, 17564, 17564, 17564, 17564), class = "Date")), .Names = c("value", 
        "from", "to", "subscription.length", "subset", "id", "start", 
        "end"), row.names = c(NA, -23L), class = "data.frame")
    expect_error(DisplayrTotalPaymentMonthlyRevenueData <- RevenueData(value = z$value, 
        from = as.Date(z$from),
        to = as.Date(z$end),
        subscription.length = "month",
        subset = z$subset, 
        id = z$id,
        start = min(z$start, na.rm = TRUE),
        end = Sys.Date()))
    
    zdf = structure(list(value = structure(c(NA, NA, NA, NA, NA, 3999, 
NA, NA, 7998, NA, 1998.86426651879, NA, NA, NA, NA, NA, 3999, 
NA, NA, NA, NA, NA, 7998), questiontype = "Number", name = "TotalAnnualLicenceValue", label = "TotalAnnualLicenceValue", question = "TotalAnnualLicenceValue"), 
    from = structure(c(NA, NA, NA, NA, NA, 1515070800, NA, NA, 
    1512460800, NA, 1511787600, NA, NA, NA, NA, NA, 1507791600, 
    NA, NA, NA, NA, NA, 1503532800), class = c("POSIXct", "POSIXt", 
    "QDate"), QDate = structure(c(NA, NA, NA, NA, NA, 134L, NA, 
    NA, 104L, NA, 96L, NA, NA, NA, NA, NA, 50L, NA, NA, NA, NA, 
    NA, 1L), class = c("ordered", "factor"), .Label = c("8/24/2017", 
    "8/25/2017", "8/26/2017", "8/27/2017", "8/28/2017", "8/29/2017", 
    "8/30/2017", "8/31/2017", "9/1/2017", "9/2/2017", "9/3/2017", 
    "9/4/2017", "9/5/2017", "9/6/2017", "9/7/2017", "9/8/2017", 
    "9/9/2017", "9/10/2017", "9/11/2017", "9/12/2017", "9/13/2017", 
    "9/14/2017", "9/15/2017", "9/16/2017", "9/17/2017", "9/18/2017", 
    "9/19/2017", "9/20/2017", "9/21/2017", "9/22/2017", "9/23/2017", 
    "9/24/2017", "9/25/2017", "9/26/2017", "9/27/2017", "9/28/2017", 
    "9/29/2017", "9/30/2017", "10/1/2017", "10/2/2017", "10/3/2017", 
    "10/4/2017", "10/5/2017", "10/6/2017", "10/7/2017", "10/8/2017", 
    "10/9/2017", "10/10/2017", "10/11/2017", "10/12/2017", "10/13/2017", 
    "10/14/2017", "10/15/2017", "10/16/2017", "10/17/2017", "10/18/2017", 
    "10/19/2017", "10/20/2017", "10/21/2017", "10/22/2017", "10/23/2017", 
    "10/24/2017", "10/25/2017", "10/26/2017", "10/27/2017", "10/28/2017", 
    "10/29/2017", "10/30/2017", "10/31/2017", "11/1/2017", "11/2/2017", 
    "11/3/2017", "11/4/2017", "11/5/2017", "11/6/2017", "11/7/2017", 
    "11/8/2017", "11/9/2017", "11/10/2017", "11/11/2017", "11/12/2017", 
    "11/13/2017", "11/14/2017", "11/15/2017", "11/16/2017", "11/17/2017", 
    "11/18/2017", "11/19/2017", "11/20/2017", "11/21/2017", "11/22/2017", 
    "11/23/2017", "11/24/2017", "11/25/2017", "11/26/2017", "11/27/2017", 
    "11/28/2017", "11/29/2017", "11/30/2017", "12/1/2017", "12/2/2017", 
    "12/3/2017", "12/4/2017", "12/5/2017", "12/6/2017", "12/7/2017", 
    "12/8/2017", "12/9/2017", "12/10/2017", "12/11/2017", "12/12/2017", 
    "12/13/2017", "12/14/2017", "12/15/2017", "12/16/2017", "12/17/2017", 
    "12/18/2017", "12/19/2017", "12/20/2017", "12/21/2017", "12/22/2017", 
    "12/23/2017", "12/24/2017", "12/25/2017", "12/26/2017", "12/27/2017", 
    "12/28/2017", "12/29/2017", "12/30/2017", "12/31/2017", "1/1/2018", 
    "1/2/2018", "1/3/2018", "1/4/2018")), questiontype = "Date", name = "StartDate", label = "StartDate", question = "StartDate"), 
    to = structure(c(NA, NA, NA, NA, NA, 1546606800, NA, NA, 
    1543996800, NA, 1527426000, NA, NA, NA, NA, NA, 1539327600, 
    NA, NA, NA, NA, NA, 1535068800), class = c("POSIXct", "POSIXt", 
    "QDate"), QDate = structure(c(NA, NA, NA, NA, NA, 223L, NA, 
    NA, 193L, NA, 1L, NA, NA, NA, NA, NA, 139L, NA, NA, NA, NA, 
    NA, 90L), class = c("ordered", "factor"), .Label = c("5/27/2018", 
    "5/28/2018", "5/29/2018", "5/30/2018", "5/31/2018", "6/1/2018", 
    "6/2/2018", "6/3/2018", "6/4/2018", "6/5/2018", "6/6/2018", 
    "6/7/2018", "6/8/2018", "6/9/2018", "6/10/2018", "6/11/2018", 
    "6/12/2018", "6/13/2018", "6/14/2018", "6/15/2018", "6/16/2018", 
    "6/17/2018", "6/18/2018", "6/19/2018", "6/20/2018", "6/21/2018", 
    "6/22/2018", "6/23/2018", "6/24/2018", "6/25/2018", "6/26/2018", 
    "6/27/2018", "6/28/2018", "6/29/2018", "6/30/2018", "7/1/2018", 
    "7/2/2018", "7/3/2018", "7/4/2018", "7/5/2018", "7/6/2018", 
    "7/7/2018", "7/8/2018", "7/9/2018", "7/10/2018", "7/11/2018", 
    "7/12/2018", "7/13/2018", "7/14/2018", "7/15/2018", "7/16/2018", 
    "7/17/2018", "7/18/2018", "7/19/2018", "7/20/2018", "7/21/2018", 
    "7/22/2018", "7/23/2018", "7/24/2018", "7/25/2018", "7/26/2018", 
    "7/27/2018", "7/28/2018", "7/29/2018", "7/30/2018", "7/31/2018", 
    "8/1/2018", "8/2/2018", "8/3/2018", "8/4/2018", "8/5/2018", 
    "8/6/2018", "8/7/2018", "8/8/2018", "8/9/2018", "8/10/2018", 
    "8/11/2018", "8/12/2018", "8/13/2018", "8/14/2018", "8/15/2018", 
    "8/16/2018", "8/17/2018", "8/18/2018", "8/19/2018", "8/20/2018", 
    "8/21/2018", "8/22/2018", "8/23/2018", "8/24/2018", "8/25/2018", 
    "8/26/2018", "8/27/2018", "8/28/2018", "8/29/2018", "8/30/2018", 
    "8/31/2018", "9/1/2018", "9/2/2018", "9/3/2018", "9/4/2018", 
    "9/5/2018", "9/6/2018", "9/7/2018", "9/8/2018", "9/9/2018", 
    "9/10/2018", "9/11/2018", "9/12/2018", "9/13/2018", "9/14/2018", 
    "9/15/2018", "9/16/2018", "9/17/2018", "9/18/2018", "9/19/2018", 
    "9/20/2018", "9/21/2018", "9/22/2018", "9/23/2018", "9/24/2018", 
    "9/25/2018", "9/26/2018", "9/27/2018", "9/28/2018", "9/29/2018", 
    "9/30/2018", "10/1/2018", "10/2/2018", "10/3/2018", "10/4/2018", 
    "10/5/2018", "10/6/2018", "10/7/2018", "10/8/2018", "10/9/2018", 
    "10/10/2018", "10/11/2018", "10/12/2018", "10/13/2018", "10/14/2018", 
    "10/15/2018", "10/16/2018", "10/17/2018", "10/18/2018", "10/19/2018", 
    "10/20/2018", "10/21/2018", "10/22/2018", "10/23/2018", "10/24/2018", 
    "10/25/2018", "10/26/2018", "10/27/2018", "10/28/2018", "10/29/2018", 
    "10/30/2018", "10/31/2018", "11/1/2018", "11/2/2018", "11/3/2018", 
    "11/4/2018", "11/5/2018", "11/6/2018", "11/7/2018", "11/8/2018", 
    "11/9/2018", "11/10/2018", "11/11/2018", "11/12/2018", "11/13/2018", 
    "11/14/2018", "11/15/2018", "11/16/2018", "11/17/2018", "11/18/2018", 
    "11/19/2018", "11/20/2018", "11/21/2018", "11/22/2018", "11/23/2018", 
    "11/24/2018", "11/25/2018", "11/26/2018", "11/27/2018", "11/28/2018", 
    "11/29/2018", "11/30/2018", "12/1/2018", "12/2/2018", "12/3/2018", 
    "12/4/2018", "12/5/2018", "12/6/2018", "12/7/2018", "12/8/2018", 
    "12/9/2018", "12/10/2018", "12/11/2018", "12/12/2018", "12/13/2018", 
    "12/14/2018", "12/15/2018", "12/16/2018", "12/17/2018", "12/18/2018", 
    "12/19/2018", "12/20/2018", "12/21/2018", "12/22/2018", "12/23/2018", 
    "12/24/2018", "12/25/2018", "12/26/2018", "12/27/2018", "12/28/2018", 
    "12/29/2018", "12/30/2018", "12/31/2018", "1/1/2019", "1/2/2019", 
    "1/3/2019", "1/4/2019")), questiontype = "Date", name = "EndDate", label = "EndDate", question = "EndDate"), 
    subset = c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, 
    FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, 
    TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE), id =  LETTERS[c(1,1,1,1:20)]), row.names = c(NA, 
    -23L), class = "data.frame")

    expect_error(RevenueData(zdf$value, 
        from = zdf$from,
        to = zdf$to,
        subscription.length = "month",
        subset = zdf$subset, 
        id = zdf$id,
        start = min(zdf$from[zdf$subset], na.rm = TRUE)), "'value' contains missing values")
    zdf = subset(zdf, zdf$subset & !is.na(zdf$value))
    expect_error(RevenueData(zdf$value, 
        from = zdf$from,
        to = zdf$to,
        subscription.length = "month",
        id = zdf$id,
        start = min(zdf$from[zdf$subset], na.rm = TRUE)), NA)
    

})
