context("Churn")
data(q.invoice.lines)
d <- q.invoice.lines
library(lubridate)
Sys.setenv(TZ='GMT')
end <-  ISOdate(2016,6,30)
#start <-  ISOdate(2012,7,1)
by = "year"
for (by in c("week", "month", "quarter", "year"))
    test_that(paste("Creating RevenueData", by),
          {
            expect_error(capture.output(rd <- RevenueData(d$AUD, d$ValidFrom, d$ValidTo, end = end, id = d$name, by = by, subset = d$validInvoice == 1)), NA)
            expect_error(ch <- Churn(rd, volume = FALSE), NA)
            expect_error(p <- TimeSeriesColumnChart(ch$rate, ch$by, tickformat = "%"), NA)
            expect_error(capture.output(print(p)), NA)
})

# 
# data(q.invoice.lines)
# d <- q.invoice.lines
# library(lubridate)
# Sys.setenv(TZ='GMT')
# start <-  ISOdate(2012,1,1)
# end <-  ISOdate(2015,12,31)
# #start <-  ISOdate(2012,7,1)
# by = "year"
# rd <- RevenueData(d$AUD, d$ValidFrom, d$ValidTo, start = start, end = end, id = d$name, by = by, subset = d$validInvoice == 1)
# Churn(rd, volume = FALSE, remove.last = FALSE)
# 




# s <- read.csv("https://raw.githubusercontent.com/plotly/datasets/master/school_earnings.csv")
# s <- s[order(s$Men), ]
# library(plotly)
# p <- plot_ly(s, x = ~Women, y = ~School, name = "Women",
#              mode = "markers", marker = list(color = "pink"), type = "scatter") %>%
#   add_trace(x = ~Men, name = "Men", y = ~School, marker = list(color = "blue"),
#             mode = "markers", type = "scatter") %>%
#   layout(
#     title = "Gender earnings disparity",
#     xaxis = list(title = "Annual Salary (in thousands)"),
#     margin = list(l = 65)
#   )
# p
