#Sys.setenv(TZ='GMT')
q.invoice.lines <- foreign::read.spss(system.file("extdata", "invoiceLines.sav", package = "flipStartup"), to.data.frame = TRUE)
q.invoice.lines <- q.invoice.lines[, !(names(q.invoice.lines) %in% c("InvoiceID", "orgID","Edition"))]
q.invoice.lines$ValidFrom <- ISOdate(1582,10,14)  +  q.invoice.lines$ValidFrom
q.invoice.lines$ValidTo <- ISOdate(1582,10,14)  +  q.invoice.lines$ValidTo - lubridate::seconds(1)
exchangeRates <- c(AUD = 1, CNY = 4.84, EUR = 0.66, GBP = 0.52, NZD = 1.05, USD = .74) #11 June 2016
q.invoice.lines$AUD <- q.invoice.lines$Amount / exchangeRates[q.invoice.lines$currency]
q.invoice.lines$ValidTo <- q.invoice.lines$ValidTo - lubridate::seconds(1)
end <- ISOdate(2016, 6, 14)
q.invoice.lines <- q.invoice.lines[q.invoice.lines$ValidFrom < end, ]
devtools::use_data(q.invoice.lines, internal = FALSE, overwrite = TRUE)



# #####################################
# ####  Procesing data             ####
# #####################################
#
# data(q.invoice.lines)
# d <- q.invoice.lines
# rd <- RevenueData(d$AUD, d$ValidFrom, d$ValidTo, id = d$name, by = "year", subset = d$validInvoice == 1, trim.id = 20)
#
# #####################################
# ####  Growth Accounting          ####
# #####################################
#
# rg <- RevenueGrowthAccounting(rd)
# plot(rg)
# QuickRatio(rg)
#
#
# #####################################
# ####  Retention                  ####
# #####################################
#
# # Number of firms
# n.by.start.period <- Table(id ~ start.period, data = dat, FUN = function(x) length(unique(x)))
# n.by.start.period
#
# Retention(rd)
#
# Growth(rd)
#
# LifetimeValue(rd)
#
#
#
# # Survival
# library(survival)
# dat.id <- rd[dat$observation == 1, ]
# dat.id$start.day <- lubridate::interval(date.1, dat.id$start) %/% days(1)
# dat.id$end.day <- lubridate::interval(date.1, dat.id$end) %/% days(1)
# dat.id$days.as.customer <- dat.id$end.day - dat.id$start.day
# hist(dat.id$days.as.customer)
# table(dat.id$days.as.customer)
# n.id <- nrow(dat.id)
# (1:n.id)[dat.id$days.as.customer == 305]
#
# S <- Surv(time = dat.id$days.as.customer, event = dat.id$churned, type = "right")
# s.fit <- survfit(S ~ 1)
# #s.fit <- survfit(S ~ dat.id$period)
#
# summary(s.fit)
# p <- plot(s.fit, xscale = 365.25, xlab = "Years after subscribing", ylab = "Proportion survived")
#
#
# library(ggfortify)
# library(survival)
# autoplot(s.fit)
#
# library(lubridate)
# date.1 <- min(dat.id$start)
#
#
# table(dat[dat$observation == 1, ]$start.period)
#
#
# S <- Surv(time = dat.id$start.day , rep(max(dat.id$date), length(dat.id$start.day ), dat.id$churned)  #dat.id$end.day,
#           s.fit <- survfit(S ~ 1)
#           summary(s.fit)
#           plot(s.fit, xscale = 365.25, xlab = "Years after subscribing", ylab = "Proportion survived")
#
#           s.fit <- survfit(S ~ dat.id$start.period)
#           summary(s.fit)
#           plot(s.fit, xscale = 365.25, xlab = "Years after subscribing", ylab = "Proportion survived")
#           library(ggplot2)
#           ggsurv(s.fit)
#
#
#
#
#
#
#
# # Value - by year (overinflates year 2 pickup)
# z <- HistoricalValue(revenue.data)
# z
# z <- as.data.frame(z$mean)
#
#
# zp <- ggplot(mry, aes(x=year, y=number, group=rating))
# p + geom_line()
#
#
# flipStandardCharts::Chart(z$index, type = "Line", transpose = TRUE, x.title = "Year")
# flipStandardCharts::Chart(z$mean, type = "Line", transpose = TRUE, x.title = "Average")
# flipStandardCharts::Chart(z$cumulative, type = "Line", transpose = TRUE, x.title = "Year")
#
#
# plot(HistoricalValue(revenue.data)$cumulative)
#
#
# z <- Table(value ~ start.period + period, dat, sum)
# rowSums(z)
# IndexDiagonal(z)
#
# # Lifetime value
# z <- Table(value ~ start.period + period.counter, dat, sum)
# rowSums(z)
# Index(z, STATS = z[, 1], remove = "sdf")
#
# value.by.period.by.start.date
#
#
# Index(value.by.period.by.start.date)
#
# value.by.period.by.start.date
#
#
# sum(value.by.period.by.start.date)
# formattable::currency(apply(value.by.period.by.start.date, 1, sum))
# formattable::currency(apply(value.by.period.by.start.date, 2, sum))
# formattable::currency(sum(qApril2016$inc_amt, na.rm = TRUE))
# formattable::currency(aggregate(value ~ period, data = dat, sum))
#
#
# # Retention
# n.by.period.by.start.date <- TableOfStatistics(id ~ start.period + period.counter, data = dat, FUN = function(x) length(unique(x)))
# Index(n.by.period.by.start.date)
#
# sweep(n.by.period.by.start.date, 1, n.by.period.by.start.date[,1] / 100, "/")
#
#
#
#
#
#
#
#           library(ggplot2)
#           d <- data.frame(
#               date = c("01-04-1940", "05-29-1963", "12-02-2002"),
#               value = c(4, 35, 24)
#           )
#
#           d$date <- as.Date(d$date, format = "%m-%d-%Y")
#
#           ggplot(d, aes(x=date, y=value)) + geom_step(colour="blue")
#
#
#           z = TableOfStatistics(value ~ start + period, dat, sum)
#           z[col(z) >= nrow(z) + 1 - row(z)] <- NA
#           z <- sweep(z, 1, z[,1], "/")
#           library(d3heatmap)
#           d3heatmap(z, scale = "none",  Rowv = NULL, Colv = NULL, colors = "Blues")
#
#
#
#
#
#
#
#
#
#           z = TableOfStatistics(value ~ start + period, dat, sum)
#           z[col(z) >= nrow(z) + 1 - row(z)] <- NA
#           z <- sweep(z, 1, z[,1], "/")
#           library(d3heatmap)
#           zz <- matrix("dog\ndog", nrow(z), ncol(z))
#           d3heatmap(z, scale = "none",  Rowv = NULL, Colv = NULL, colors = "Blues", cellnote = zz)
#
#           # Cohort size
#
#
#
#           , xscale = 365.25, xlab = "Years after subscribing", ylab = "Proportion survived")
# 365.25
#
# 365.25
#
#
# xtabs( ~ start + period, data = , sum)
# TableOfStatistics( ~ start + period, dat[dat$observation == 1, ], sum)
# zzz = function(x) {length(unique(x))}
# TableOfStatistics(id ~ start + period, dat, zzz)
#
#
#
# # Survival analysis
#
#
#
#
#
#
#
#
#
#
#
# tenure.by.year <- .aggregate.xtabs(value ~ tenure + start, data = data, FUN)
#
#
#
# data(qApril2016)
# dat <- TidyRevenueData(qApril2016$inc_amt, lubridate::dmy_hms(qApril2016$dt), qApril2016$name, "year")
#
#
#
#
#
# tenure.by.year <- .aggregate.xtabs(value ~ tenure + year, data = data, FUN)
#
#
#
#
#
#
#
# dstrat<-svydesign(id=~1,strata=~strata, weights=~weight, data=df, fpc=~fpc)
# svymean(~answer, dstrat)
#
#
#
#
#
#
#
#
#
#
# year <- .aggregate.xtabs(value ~ year, data = data, FUN)
# tenure <- .aggregate.xtabs(value ~ tenure, data = data, FUN)
# tenure.by.year <- .aggregate.xtabs(value ~ tenure + year, data = data, FUN)
# start.by.year <- .aggregate.xtabs(value ~ start + year, data = data, FUN)
# list(year = year, tenure = tenure, tenure.by.year = tenure.by.year, start.by.year = start.by.year)
# #    terminated
# #    start
# #    strtrim(   , trim.id)
# }
#
#
#
#
#
#
#
#
#
# #qApril2016$AUD <- qApril2016$value / exchangeRates[qApril2016$currency]
# #qApril2016$time <- lubridate::dmy_hms(qApril2016$time)
# #qApril2016$time <- lubridate::dmy_hm(qApril2016$time)
# #devtools::use_data(qApril2016, internal = FALSE, overwrite = TRUE)
#
#
# #RevenueGrowth(d$name, d$year, d$inc_amt)
#
# # library(lubridate)
# # d$year <- year(floor_date(dmy_hms(d$dt), "year"))
# # d$Steve <- "d7e7ee3a-97f3-461a-a4fe-38ce02ccdc20" == d$person
# #
# #
# # aggregate(inc_amt ~ person, data = d, FUN = sum, subset = ((country == "Canada" | country == "United States" ) & year == 2015))
# #
# #
# # sales <- aggregate(inc_amt ~ Steve + year, data = d, sum)
# #
# # sales <- aggregate(inc_amt ~ year + Steve, data = d, FUN = sum, subset =  (country == "Canada" | country == "United States") &  year < 2016)
# #
# #
# # sales <- aggregate(inc_amt ~ Steve + year, data = d, sum)
# #
# # aggregate(inc_amt ~ year, data = d, FUN = sum, subset = country == "Australia"  &  year < 2016)
# #
# #
# # barplot(sales[,2], sales[,1])
# #
# rg <- RevenueGrowth(d$name, d$year, d$inc_amt)
# #rg <- RevenueGrowth(d$id, d$year, d$AUD)
# rg
#
# rg$data[rg$data$period == 2015,]
# rg$data[rg$data$period == 2015,]
#
# rg$data[rg$data$period == 2012 & rg$data$status == "Churned", ]
#
# head(d,20)
#
#
# # Annual active user growth accounting.
# ddd <- rg$Table
# ddd$Period <- rownames(ddd)
# q <- ddd[-nrow(ddd), 7:8] #Quick ratio
# d <- ddd[-nrow(ddd), 1:5]
#
# dd <- reshape2::melt(ddd, id.vars = "Period")
# names(dd)[2] <- "Behavior"
#
#
# # Revenue churn
#
# library(ggplot2)
# ggplot(dd[dd$Behavior %in% c("New", "Expansion", "Resurrected"), ], aes(x = Period, y = value, fill = Behavior)) +
#     geom_bar(stat = "identity", position = "stack") +
#     geom_bar(data = dd[dd$Behavior %in% c("Churned", "Contraction"), ], aes(x = Period, y = value, fill = Behavior),stat = "identity") +
#     labs(y="Change in revenue", x = "") +
#     scale_y_continuous() + #labels = comma) +
#     ggtitle("Growth Accounting - Annual Revenue")
#
#
# d1 <- d <- q[-1,]
# d$type <- "Quick Ratio = (New + Exp. + Res.) / (Con. + Churned)"
# d1$type = "4 is excellent for a business SaaS"
# d1$Quick.Ratio <- 4
# d <- rbind(d,d1)
# ggplot(data=d, aes(x = Period, y = Quick.Ratio, group=type, color = type)) +
#     geom_line() +
#     geom_line() + scale_y_continuous(limits = c(0, max(d$Quick.Ratio))) +
#     labs(y="Quick Ratio", x = "", legend = "") +
#     ggtitle("Quick Ratio - Annual Revenue")
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
# qApril2016$year <- strftime(as.Date(qApril2016$dt), "%Y")
#
# dau <- aggregate(inc_amt ~ name + dt, FUN = sum, data = dau)
# dau$month.of.year <- strftime(dau$dt, "%m")
# dau$year <- strftime(dau$dt, "%Y")
# dau$day.of.month <- strftime(dau$dt, "%d")
# library(lubridate)
# dau$week = floor_date(dau$dt, "week")
# dau$month = floor_date(dau$dt, "month")
# dau
#
#
#
#
#
#
#
# devtools::install_github('cmpolis/datacomb', subdir='pkg', ref='1.1.2');
# library(datacomb);
# Datacomb(iris)
#
#
# rownames(d) <- 1:nrow(d)
# Datacomb(d)
#
# z <- c(a = .13, b =.34)
# z <- percent(z)
# names(z) = LETTERS[1:2]
# z
#
#
#
# g.tab$quick <- apply(g.tab[,3:5], 1, sum) / apply(g.tab[,1:2], 1, sum)
#
#
# aggregate(AUD ~ year, data = d, FUN = sum)
#
# aggregated$id.by.period <- paste(aggregated$ids, aggregated$time)
#
# rep(periods, rep*)
# # Restructuring the data.
# aggregated <- aggregate(value ~ id + period, FUN = sum)
#
#
# period.min <- aggregate(aggregated$period ~ aggregated$id, FUN = min)
# names(period.min) <- c("id", "first.period")
# tidied <- merge(aggregated, period.min, by = "id")
# sorted <- tidied[order(tidied$id,  tidied$period),]
# difference <- aggr
# period <- sorted$period
# first.period <- sorted$first.period
# value <- sorted$value
# id <- sorted$id
# customers <- aggregate(id ~ period, FUN = function(x) length(unique(x)))
# .difference <- function(id, , periods)
#
#     period.names <- customers[, 1]
# customers <- customers[, 2]
# # Internal functions
# .uniqueID <- function(x) {unique(id[x])}
# .nUnique <- function(x) {length(.uniqueID(x))}
# .sum <- function(x) {sum(value(x))}
# # Computing the data frame.
# results <- c(Customers = customers[1], Retained = 0, New = customers[1], Resurrected = 0, Churned = 0)
# for (p in periods[-1])
# {
#     cases.p <- period == p
#     n.new <- .nUnique(cases.p & first.period == p)
#     id.p <- .uniqueID(cases.p)
#     p_minus_1 <- p - 1
#     cases.p_minus_1 <- period == p_minus_1
#     id.p_minus_1 <- .uniqueID(cases.p_minus_1)
#     n.p <- .nUnique(cases.p)
#     lookup.retained <- id.p %in% id.p_minus_1
#     lookup.churned <- !(id.p_minus_1 %in% id.p)
#     id.churned <- id.p_minus_1[lookup.churned]
#     n.churned <- length(id.churned)
#     n.retained <- sum(lookup.retained)
#     n.non.new <- .nUnique(cases.p & first.period < p)
#     n.resurrected <- n.non.new - n.retained
#     results <- rbind(results,
#                      c(n.p, n.retained, n.new, n.resurrected, -n.churned))
# }
# rownames(results) <- periods
# results <- as.data.frame(results)
# results$Period <- periods
# class(results) <- append(class(results), "CustomerGrowthTable")
# results}
#
#
# library(lubridate)
# data(qApril2016)
# d <- qApril2016
# d$year <- floor_date(d$time, "year")
# d$year <- year(floor_date(d$time, "year"))
# g.tab <- CustomerGrowthTable(d$id, d$year, d$AUD)
# g.tab <- g.tab[-nrow(g.tab), ]
#
# dat <-  for example , you do this using melt:
#
#
#
#     # Churn
#     g.tab <- data.frame(rg$Table)
# g.tab$Period <- rownames(g.tab)
# library(ggplot2)
# g.tab$Churn <- c(0, -g.tab$Churned[-1] / rg$Revenue[-nrow(g.tab)]) * 100
# g.tab$QuickRatio <- c(0, -g.tab$Churned[-1] / g.tab$Customers[-nrow(g.tab)]) * 100
# d <- reshape2::melt(g.tab, id.vars = "Period")
# names(d)[2] <- "Behavior"
#
# ggplot() +
#     geom_bar(data = d[d$Behavior == "New", ], aes(x = Period, y = value, fill = Behavior),stat = "identity") +
#     geom_bar(data = d[d$Behavior == "Retained", ], aes(x = Period, y = value, fill = Behavior),stat = "identity") +
#     geom_bar(data = d[d$Behavior == "Resurrected", ], aes(x = Period, y = value, fill = Behavior),stat = "identity") +
#     geom_bar(data = d[d$Behavior == "Churned", ], aes(x = Period, y = value, fill = Behavior),stat = "identity")
# +
#     scale_fill_brewer(type = "seq", palette = 1)
#
# # Annual active user growth accounting
# ggplot(d[d$Behavior %in% c("New", "Retained", "Resurrected"), ], aes(x = Period, y = value, fill = Behavior)) +
#     geom_bar(stat = "identity", position = "stack") +   geom_bar(data = d[d$Behavior == "Churned", ], aes(x = Period, y = value, fill = Behavior),stat = "identity") +
#     labs(y="Number of customers", x + "") +
#     ggtitle("Growth Accounting - Annual Active Users")
# d1 <- d <- g.tab[,c("Period", "q")]
# d$type <- "Quick Ratio = (New + Res.) / Churned"
# d1$type = "1.5 is good for a consumer product"
# d1$q <- 1.5
# d <- rbind(d,d1)
# ggplot(data=d, aes(x=Period, y=q, group=type, color = type)) +
#     geom_line() +
#     geom_line() + scale_y_continuous(limits = c(0, max(g.tab$q))) +
#     labs(y="Quick Ratio", x = "", legend = "") +
#     ggtitle("Quick Ratio - Annual Active Users") +
#     ggtitle("Quick Ratio - Annual Active Users")
#
#
#
#
#
# #' #' \code{Churn}
# #' #' @description Computes the number of customers to have churned.
# #' #' @param id A vector of characters, representing a unique identifier for customers.
# #' #' @param period A vector of class \code{"Date"} or \code{"Integer"}, which represents the time
# #' #' at which a transaction occurred.
# #' #' @param grace The number of days grace that should be used in the calculations. For example, a
# #' #' \code{grace} of 35 would mean a customer is considered to have churned if no transactions have
# #' #' occurred in the past 35
# #' #'
# #' #'   \code{"Integer"}, which represents the time
# #' #' at which a transaction occurred.
# #' #' @param value The value of the transaction.
# #' CustomerGrowthTable <- function(id, period, grace)
# #' {
# #'   # Restructuring the data.
# #'   aggregated <- aggregate(value ~ id + period, FUN = sum)
# #'   period.max <- aggregate(aggregated$period ~ aggregated$id, FUN = max)
# #'   names(period.min) <- c("id", "first.period")
# #'   tidied <- merge(aggregated, period.min, by = "id")
# #'   sorted <- tidied[order(tidied$id,  tidied$period),]
# #'   period <- sorted$period
# #'   first.period <- sorted$first.period
# #'   value <- sorted$value
# #'   id <- sorted$id
# #'   periods <- sort(as.numeric(as.character(unique(period))))
# #'   customers <- aggregate(id ~ period, FUN = function(x) length(unique(x)))
# #'   period.names <- customers[, 1]
# #'   customers <- customers[, 2]
# #'   # Internal functions
# #'   .uniqueID <- function(x) {unique(id[x])}
# #'   .nUnique <- function(x) {length(.uniqueID(x))}
# #'   # Computing the data frame.
# #'   results <- c("customers" = customers[1], retained = 0, new = customers[1], resurrected = 0, churned = 0)
# #'   for (p in periods[-1])
# #'   {
# #'     cases.p <- period == p
# #'     n.new <- .nUnique(cases.p & first.period == p)
# #'     id.p <- .uniqueID(cases.p)
# #'     p_minus_1 <- p - 1
# #'     cases.p_minus_1 <- period == p_minus_1
# #'     id.p_minus_1 <- .uniqueID(cases.p_minus_1)
# #'     n.p <- .nUnique(cases.p)
# #'     lookup.retained <- id.p %in% id.p_minus_1
# #'     lookup.churned <- !(id.p_minus_1 %in% id.p)
# #'     id.churned <- id.p_minus_1[lookup.churned]
# #'     n.churned <- length(id.churned)
# #'     n.retained <- sum(lookup.retained)
# #'     n.non.new <- .nUnique(cases.p & first.period < p)
# #'     n.resurrected <- n.non.new - n.retained
# #'     results <- rbind(results,
# #'                      c(n.p, n.retained, n.new, n.resurrected, -n.churned))
# #'   }
# #'   rownames(results) <- periods
# #'   as.data.frame(results)}
# #'
# #' aggregate()
# #'
# #'
# #' library(lubridate)
# #' d <- qApril2016
# #' d$year <- floor_date(d$time, "year")
# #' d$year <- year(floor_date(d$time, "year"))
# #' g.tab <- CustomerGrowthTable(d$id, d$year, d$AUD)
# #'
# #'
# #' data(qApril2016)
# #'
# #' ggplot() +
# #'   geom_bar(data = g.tab, aes(x=Year, y=OperatingIncome, fill=Division),stat = "identity") +
# #'   geom_bar(data = dat2, aes(x=Year, y=OperatingIncome, fill=Division),stat = "identity") +
# #'   scale_fill_brewer(type = "seq", palette = 1)
# #'
# #'
# #'
# #' ibrary(lubridate())
# #' qApril2016$year <- floor_date(
# #'
# #'
# #'   annual.data.123 <- data.frame(id = invoices$CustOrgID[filter],
# #'                                 time = year((as.Date(invoices$Issued[filter]),"year")),
# #'                                 value = as.numeric((invoices$Total)[filter]))
# #'
# #'
# #'   levels(qApril2016$currency)
# #'
# #'
# #' library(lubridate)
# #' m <- raw.data
# #' annual.growth.accounting <- CustomerGrowthTable(m$id,m$time, m$value)
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
# #' \code{CustomerGrowthTable}
# #' @description Computes statistics for use in growth accounting of a startup.
# #' @param id A vector of characters, representing a unique identifier for customers.
# #' @param period A vector of class \code{"Date"} or \code{"Integer"}, which represents the time
# #' at which a transaction occurred.
# #' @param value The value of the transaction.
# CustomerGrowthTable <- function(id, period, value)
# {
#     # Restructuring the data.
#     aggregated <- aggregate(value ~ id + period, FUN = sum)
#     period.min <- aggregate(aggregated$period ~ aggregated$id, FUN = min)
#     names(period.min) <- c("id", "first.period")
#     tidied <- merge(aggregated, period.min, by = "id")
#     sorted <- tidied[order(tidied$id,  tidied$period),]
#     difference <- aggr
#     period <- sorted$period
#     first.period <- sorted$first.period
#     value <- sorted$value
#     id <- sorted$id
#     periods <- sort(as.numeric(as.character(unique(period))))
#     customers <- aggregate(id ~ period, FUN = function(x) length(unique(x)))
#     .difference <- function(id, , periods)
#
#         period.names <- customers[, 1]
#     customers <- customers[, 2]
#     # Internal functions
#     .uniqueID <- function(x) {unique(id[x])}
#     .nUnique <- function(x) {length(.uniqueID(x))}
#     .sum <- function(x) {sum(value(x))}
#     # Computing the data frame.
#     results <- c(Customers = customers[1], Retained = 0, New = customers[1], Resurrected = 0, Churned = 0)
#     for (p in periods[-1])
#     {
#         cases.p <- period == p
#         n.new <- .nUnique(cases.p & first.period == p)
#         id.p <- .uniqueID(cases.p)
#         p_minus_1 <- p - 1
#         cases.p_minus_1 <- period == p_minus_1
#         id.p_minus_1 <- .uniqueID(cases.p_minus_1)
#         n.p <- .nUnique(cases.p)
#         lookup.retained <- id.p %in% id.p_minus_1
#         lookup.churned <- !(id.p_minus_1 %in% id.p)
#         id.churned <- id.p_minus_1[lookup.churned]
#         n.churned <- length(id.churned)
#         n.retained <- sum(lookup.retained)
#         n.non.new <- .nUnique(cases.p & first.period < p)
#         n.resurrected <- n.non.new - n.retained
#         results <- rbind(results,
#                          c(n.p, n.retained, n.new, n.resurrected, -n.churned))
#     }
#     rownames(results) <- periods
#     results <- as.data.frame(results)
#     results$Period <- periods
#     class(results) <- append(class(results), "CustomerGrowthTable")
#     results}
#
#
# library(lubridate)
# data(qApril2016)
# d <- qApril2016
# d$year <- floor_date(d$time, "year")
# d$year <- year(floor_date(d$time, "year"))
# g.tab <- CustomerGrowthTable(d$id, d$year, d$AUD)
# g.tab <- g.tab[-nrow(g.tab), ]
#
# dat <-  for example , you do this using melt:
#
#
#
#     # Churn
#     library(ggplot2)
# g.tab$Churn <- c(0, -g.tab$Churned[-1] / g.tab$Customers[-nrow(g.tab)]) * 100
# g.tab$QuickRatio <- c(0, -g.tab$Churned[-1] / g.tab$Customers[-nrow(g.tab)]) * 100
# d <- reshape2::melt(g.tab, id.vars = "Period")
# names(d)[2] <- "Behavior"
#
# ggplot() +
#     geom_bar(data = d[d$Behavior == "New", ], aes(x = Period, y = value, fill = Behavior),stat = "identity") +
#     geom_bar(data = d[d$Behavior == "Retained", ], aes(x = Period, y = value, fill = Behavior),stat = "identity") +
#     geom_bar(data = d[d$Behavior == "Resurrected", ], aes(x = Period, y = value, fill = Behavior),stat = "identity") +
#     geom_bar(data = d[d$Behavior == "Churned", ], aes(x = Period, y = value, fill = Behavior),stat = "identity")
# +
#     scale_fill_brewer(type = "seq", palette = 1)
#
# # Annual active user growth accounting
# ggplot(d[d$Behavior %in% c("New", "Retained", "Resurrected"), ], aes(x = Period, y = value, fill = Behavior)) +
#     geom_bar(stat = "identity", position = "stack") +   geom_bar(data = d[d$Behavior == "Churned", ], aes(x = Period, y = value, fill = Behavior),stat = "identity") +
#     labs(y="Number of customers", x + "") +
#     ggtitle("Growth Accounting - Annual Active Users")
# d1 <- d <- g.tab[,c("Period", "q")]
# d$type <- "Quick Ratio = (New + Res.) / Churned"
# d1$type = "1.5 is good for a consumer product"
# d1$q <- 1.5
# d <- rbind(d,d1)
# ggplot(data=d, aes(x=Period, y=q, group=type, color = type)) +
#     geom_line() +
#     geom_line() + scale_y_continuous(limits = c(0, max(g.tab$q))) +
#     labs(y="Quick Ratio", x = "", legend = "") +
#     ggtitle("Quick Ratio - Annual Active Users") +
#     ggtitle("Quick Ratio - Annual Active Users")
#
#
#
#
#
# #' #' \code{Churn}
# #' #' @description Computes the number of customers to have churned.
# #' #' @param id A vector of characters, representing a unique identifier for customers.
# #' #' @param period A vector of class \code{"Date"} or \code{"Integer"}, which represents the time
# #' #' at which a transaction occurred.
# #' #' @param grace The number of days grace that should be used in the calculations. For example, a
# #' #' \code{grace} of 35 would mean a customer is considered to have churned if no transactions have
# #' #' occurred in the past 35
# #' #'
# #' #'   \code{"Integer"}, which represents the time
# #' #' at which a transaction occurred.
# #' #' @param value The value of the transaction.
# #' CustomerGrowthTable <- function(id, period, grace)
# #' {
# #'   # Restructuring the data.
# #'   aggregated <- aggregate(value ~ id + period, FUN = sum)
# #'   period.max <- aggregate(aggregated$period ~ aggregated$id, FUN = max)
# #'   names(period.min) <- c("id", "first.period")
# #'   tidied <- merge(aggregated, period.min, by = "id")
# #'   sorted <- tidied[order(tidied$id,  tidied$period),]
# #'   period <- sorted$period
# #'   first.period <- sorted$first.period
# #'   value <- sorted$value
# #'   id <- sorted$id
# #'   periods <- sort(as.numeric(as.character(unique(period))))
# #'   customers <- aggregate(id ~ period, FUN = function(x) length(unique(x)))
# #'   period.names <- customers[, 1]
# #'   customers <- customers[, 2]
# #'   # Internal functions
# #'   .uniqueID <- function(x) {unique(id[x])}
# #'   .nUnique <- function(x) {length(.uniqueID(x))}
# #'   # Computing the data frame.
# #'   results <- c("customers" = customers[1], retained = 0, new = customers[1], resurrected = 0, churned = 0)
# #'   for (p in periods[-1])
# #'   {
# #'     cases.p <- period == p
# #'     n.new <- .nUnique(cases.p & first.period == p)
# #'     id.p <- .uniqueID(cases.p)
# #'     p_minus_1 <- p - 1
# #'     cases.p_minus_1 <- period == p_minus_1
# #'     id.p_minus_1 <- .uniqueID(cases.p_minus_1)
# #'     n.p <- .nUnique(cases.p)
# #'     lookup.retained <- id.p %in% id.p_minus_1
# #'     lookup.churned <- !(id.p_minus_1 %in% id.p)
# #'     id.churned <- id.p_minus_1[lookup.churned]
# #'     n.churned <- length(id.churned)
# #'     n.retained <- sum(lookup.retained)
# #'     n.non.new <- .nUnique(cases.p & first.period < p)
# #'     n.resurrected <- n.non.new - n.retained
# #'     results <- rbind(results,
# #'                      c(n.p, n.retained, n.new, n.resurrected, -n.churned))
# #'   }
# #'   rownames(results) <- periods
# #'   as.data.frame(results)}
# #'
# #' aggregate()
# #'
# #'
# #' library(lubridate)
# #' d <- qApril2016
# #' d$year <- floor_date(d$time, "year")
# #' d$year <- year(floor_date(d$time, "year"))
# #' g.tab <- CustomerGrowthTable(d$id, d$year, d$AUD)
# #'
# #'
# #' data(qApril2016)
# #'
# #' ggplot() +
# #'   geom_bar(data = g.tab, aes(x=Year, y=OperatingIncome, fill=Division),stat = "identity") +
# #'   geom_bar(data = dat2, aes(x=Year, y=OperatingIncome, fill=Division),stat = "identity") +
# #'   scale_fill_brewer(type = "seq", palette = 1)
# #'
# #'
# #'
# #' ibrary(lubridate())
# #' qApril2016$year <- floor_date(
# #'
# #'
# #'   annual.data.123 <- data.frame(id = invoices$CustOrgID[filter],
# #'                                 time = year((as.Date(invoices$Issued[filter]),"year")),
# #'                                 value = as.numeric((invoices$Total)[filter]))
# #'
# #'
# #'   levels(qApril2016$currency)
# #'
# #'
# #' library(lubridate)
# #' m <- raw.data
# #' annual.growth.accounting <- CustomerGrowthTable(m$id,m$time, m$value)
# #'
# #'
# #'
# #'
# #'
#
#
#
#
#
#
#
#
# tenure.by.year <- .aggregate.xtabs(value ~ tenure + start, data = data, FUN)
#
#
#
# data(qApril2016)
# dat <- TidyRevenueData(qApril2016$inc_amt, lubridate::dmy_hms(qApril2016$dt), qApril2016$name, "year")
#
#
#
#
# #' \code{AverageRevenuePerCustomerOverTime}
# #' @description Creates a crosstab by aggregating numeric data over factors.
# #' @param formula A \code{formula} where the dependent variable is the variable to be aggregated over.
# #' @param FUN the function to be applied: see \code{apply} for details.
# #' @export
# {
#     xtabs(formula, data = aggregate(formula, data, FUN = FUN))
# }
# tenure.by.year <- .aggregate.xtabs(value ~ tenure + year, data = data, FUN)
#
#
#
#
#
#
#
# dstrat<-svydesign(id=~1,strata=~strata, weights=~weight, data=df, fpc=~fpc)
# svymean(~answer, dstrat)
#
#
#
#
#
#
#
#
#
#
# year <- .aggregate.xtabs(value ~ year, data = data, FUN)
# tenure <- .aggregate.xtabs(value ~ tenure, data = data, FUN)
# tenure.by.year <- .aggregate.xtabs(value ~ tenure + year, data = data, FUN)
# start.by.year <- .aggregate.xtabs(value ~ start + year, data = data, FUN)
# list(year = year, tenure = tenure, tenure.by.year = tenure.by.year, start.by.year = start.by.year)
# #    terminated
# #    start
# #    strtrim(   , trim.id)
# }
#
#
#
#
#
#
#
#
# #' \code{AverageRevenuePerCustomerOverTime}
# #' @description Computes the  statistics for use in growth accounting of a startup.
# #' @param value A vector of containing the revenue per transaction.
# #' @param data A vector of class \code{POSIXct}/\code{POSIXt}, recording the date/time of each transaction.
# #' @param id A vector of \code{character}, unique identifier for customers that made the transactions (e.g., email addresses, names, customer keys).
# #' @param by \code{year} to view the data by year, \code{quarter}, and \code{month}.
# #' @param tolerance The tolerance used in determining if a customer has ceased to be a customer. E.g., if yearly date, a value of .1 means
# #' that a customer will be assumed to be still a customer if they have purcahsed within 1 year + 10% of a year.
# #' #' @export
# RevenuePerCustomerOverTime <- function(value, date, id, by = "year", end = Sys.time(), FUN = mean, trim.id = 20, tolerance = .1)
# {
#     data <- data.frame(value, date, id)
#     zero <- data$value == 0
#     n.zero <- sum(zero)
#     if (n.zero > 0)
#     {
#         cat(paste0(n.zero, " transactions removed due to having 0 value.\n"))
#         data <- data[!zero, ]
#     }
#     start <- aggregate(date ~ id, data, min)
#     id <- start$id
#     start <- start$date
#     last <- aggregate(date ~ id, data, max)$date
#     requireNamespace("lubridate")
#     dys <- ceiling((1 + tolerance) *
#                        switch(by, year = 365.25, quarter = 365.25, month = 30, week = 7))
#
#     cutoff <- end - days(dys)
#     churned <- last < cutoff
#     overall.tenure <- interval(start, last)
#     current.tenure <- interval(start, xxxxxxxx)
#     units <- switch(by, year = years(1), quarter = quarter(1), month = month(1), week = week(1))
#     #print(units)
#     #print(table(diff %/% units))
#     #stop("dog")
#     id.lookup <- match(data$id, id)
#     data$tenure <- (tenure %/% units)[id.lookup]
#     data$start <- start[id.lookup]
#     data$year <- year(floor_date(data$date, by))
#
#
#     year <- .aggregate.xtabs(value ~ year, data = data, FUN)
#     tenure <- .aggregate.xtabs(value ~ tenure, data = data, FUN)
#     tenure.by.year <- .aggregate.xtabs(value ~ tenure + year, data = data, FUN)
#     start.by.year <- .aggregate.xtabs(value ~ start + year, data = data, FUN)
#     list(year = year, tenure = tenure, tenure.by.year = tenure.by.year, start.by.year = start.by.year)
#     #    terminated
#     #    start
#     #    strtrim(   , trim.id)
# }
#
#
#
#
#
#   q.invoice.lines$validInvoice
#
#
#
#
#
#
#
#
#
#
#
#
#
#   qApril2016 <- read.csv("C:/delete/invoiceValues.csv", stringsAsFactors = FALSE, header = TRUE)
# qApril2016$currency = factor(qApril2016$currency)
# exchangeRates <- c(AUD = 1, EUR = 0.66, GBP = 0.53, NZD = 1.11, USD = .76) #8 April 2016
#
#
# # Setting up the data.
# data(qApril2016)
#
# #####################################
# ####  Revenue Growth Accounting  ####
# #####################################
# data(qApril2016)
# d <- qApril2016
# # Magic ratio stuff.
# d$year <- lubridate::year(floor_date(dmy_hms(d$dt), "year"))
# rg <- RevenueGrowthAccounting(d$name, d$year, d$inc_amt)
# rg
# plot(rg)
# QuickRatio(rg)
#
#
# ######  Biggest transactions
# head(qApril2016[order(qApril2016$inc_amt, decreasing = TRUE),], 20)
#
#
# #####################################
# ####  Retention                  ####
# #####################################
#
# dat <- TidyRevenueData(qApril2016$inc_amt, lubridate::dmy_hms(qApril2016$dt), qApril2016$name, "year")
#
# # Number of firms
# n.by.start.period <- Table(id ~ start.period, data = dat, FUN = function(x) length(unique(x)))
# n.by.start.period
#
# # Retention
#
# z <- Table(id ~ start.period + period,  data = dat, FUN = function(x) length(unique(x)))
# IndexDiagonal(z)
#
# # Value - by year (overinflates year 2 pickup)
# z <- Table(value ~ start.period + period, dat, sum)
# rowSums(z)
# IndexDiagonal(z)
#
# # Lifetime value
# z <- Table(value ~ start.period + period.counter, dat, sum)
# rowSums(z)
# Index(z, STATS = z[, 1], remove = "sdf")
#
# value.by.period.by.start.date
#
#
# Index(value.by.period.by.start.date)
#
# value.by.period.by.start.date
#
#
# sum(value.by.period.by.start.date)
# formattable::currency(apply(value.by.period.by.start.date, 1, sum))
# formattable::currency(apply(value.by.period.by.start.date, 2, sum))
# formattable::currency(sum(qApril2016$inc_amt, na.rm = TRUE))
# formattable::currency(aggregate(value ~ period, data = dat, sum))
#
#
# # Retention
# n.by.period.by.start.date <- TableOfStatistics(id ~ start.period + period.counter, data = dat, FUN = function(x) length(unique(x)))
# Index(n.by.period.by.start.date)
#
# sweep(n.by.period.by.start.date, 1, n.by.period.by.start.date[,1] / 100, "/")
#
# # Survival
# library(survival)
# dat.id <- dat[dat$observation == 1, ]
# library(lubridate)
# date.1 <- min(dat.id$start)
# dat.id$start.day <- lubridate::interval(date.1, dat.id$start) %/% days(1)
# dat.id$end.day <- lubridate::interval(date.1, dat.id$end) %/% days(1)
#
#
# table(dat[dat$observation == 1, ]$start.period)
#
#
# S <- Surv(time = dat.id$start.day , rep(max(dat.id$date), length(dat.id$start.day ), dat.id$churned)  #dat.id$end.day,
# s.fit <- survfit(S ~ 1)
# summary(s.fit)
# plot(s.fit, xscale = 365.25, xlab = "Years after subscribing", ylab = "Proportion survived")
#
# s.fit <- survfit(S ~ dat.id$start.period)
# summary(s.fit)
# plot(s.fit, xscale = 365.25, xlab = "Years after subscribing", ylab = "Proportion survived")
# library(ggplot2)
# ggsurv(s.fit)
#
#
#
#
#
#
#
#
#
# library(ggplot2)
# d <- data.frame(
#     date = c("01-04-1940", "05-29-1963", "12-02-2002"),
#     value = c(4, 35, 24)
# )
#
# d$date <- as.Date(d$date, format = "%m-%d-%Y")
#
# ggplot(d, aes(x=date, y=value)) + geom_step(colour="blue")
#
#
# z = TableOfStatistics(value ~ start + period, dat, sum)
# z[col(z) >= nrow(z) + 1 - row(z)] <- NA
# z <- sweep(z, 1, z[,1], "/")
# library(d3heatmap)
# d3heatmap(z, scale = "none",  Rowv = NULL, Colv = NULL, colors = "Blues")
#
#
#
#
#
#
#
#
#
# z = TableOfStatistics(value ~ start + period, dat, sum)
# z[col(z) >= nrow(z) + 1 - row(z)] <- NA
# z <- sweep(z, 1, z[,1], "/")
# library(d3heatmap)
# zz <- matrix("dog\ndog", nrow(z), ncol(z))
# d3heatmap(z, scale = "none",  Rowv = NULL, Colv = NULL, colors = "Blues", cellnote = zz)
#
# # Cohort size
#
#
#
# , xscale = 365.25, xlab = "Years after subscribing", ylab = "Proportion survived")
# 365.25
#
# 365.25
#
#
# xtabs( ~ start + period, data = , sum)
# TableOfStatistics( ~ start + period, dat[dat$observation == 1, ], sum)
# zzz = function(x) {length(unique(x))}
# TableOfStatistics(id ~ start + period, dat, zzz)
#
#
#
# # Survival analysis
#
#
#
#
#
#
#
#
#
#
#
# tenure.by.year <- .aggregate.xtabs(value ~ tenure + start, data = data, FUN)
#
#
#
# data(qApril2016)
# dat <- TidyRevenueData(qApril2016$inc_amt, lubridate::dmy_hms(qApril2016$dt), qApril2016$name, "year")
#
#
#
#
#
# tenure.by.year <- .aggregate.xtabs(value ~ tenure + year, data = data, FUN)
#
#
#
#
#
#
#
# dstrat<-svydesign(id=~1,strata=~strata, weights=~weight, data=df, fpc=~fpc)
# svymean(~answer, dstrat)
#
#
#
#
#
#
#
#
#
#
# year <- .aggregate.xtabs(value ~ year, data = data, FUN)
# tenure <- .aggregate.xtabs(value ~ tenure, data = data, FUN)
# tenure.by.year <- .aggregate.xtabs(value ~ tenure + year, data = data, FUN)
# start.by.year <- .aggregate.xtabs(value ~ start + year, data = data, FUN)
# list(year = year, tenure = tenure, tenure.by.year = tenure.by.year, start.by.year = start.by.year)
# #    terminated
# #    start
# #    strtrim(   , trim.id)
# }
#
#
#
#
#
#
#
#
#
# #qApril2016$AUD <- qApril2016$value / exchangeRates[qApril2016$currency]
# #qApril2016$time <- lubridate::dmy_hms(qApril2016$time)
# #qApril2016$time <- lubridate::dmy_hm(qApril2016$time)
# #devtools::use_data(qApril2016, internal = FALSE, overwrite = TRUE)
#
#
# #RevenueGrowth(d$name, d$year, d$inc_amt)
#
# # library(lubridate)
# # d$year <- year(floor_date(dmy_hms(d$dt), "year"))
# # d$Steve <- "d7e7ee3a-97f3-461a-a4fe-38ce02ccdc20" == d$person
# #
# #
# # aggregate(inc_amt ~ person, data = d, FUN = sum, subset = ((country == "Canada" | country == "United States" ) & year == 2015))
# #
# #
# # sales <- aggregate(inc_amt ~ Steve + year, data = d, sum)
# #
# # sales <- aggregate(inc_amt ~ year + Steve, data = d, FUN = sum, subset =  (country == "Canada" | country == "United States") &  year < 2016)
# #
# #
# # sales <- aggregate(inc_amt ~ Steve + year, data = d, sum)
# #
# # aggregate(inc_amt ~ year, data = d, FUN = sum, subset = country == "Australia"  &  year < 2016)
# #
# #
# # barplot(sales[,2], sales[,1])
# #
# rg <- RevenueGrowth(d$name, d$year, d$inc_amt)
# #rg <- RevenueGrowth(d$id, d$year, d$AUD)
# rg
#
# rg$data[rg$data$period == 2015,]
# rg$data[rg$data$period == 2015,]
#
# rg$data[rg$data$period == 2012 & rg$data$status == "Churned", ]
#
# head(d,20)
#
#
# # Annual active user growth accounting.
# ddd <- rg$Table
# ddd$Period <- rownames(ddd)
# q <- ddd[-nrow(ddd), 7:8] #Quick ratio
# d <- ddd[-nrow(ddd), 1:5]
#
# dd <- reshape2::melt(ddd, id.vars = "Period")
# names(dd)[2] <- "Behavior"
#
#
# # Revenue churn
#
# library(ggplot2)
# ggplot(dd[dd$Behavior %in% c("New", "Expansion", "Resurrected"), ], aes(x = Period, y = value, fill = Behavior)) +
#     geom_bar(stat = "identity", position = "stack") +
#     geom_bar(data = dd[dd$Behavior %in% c("Churned", "Contraction"), ], aes(x = Period, y = value, fill = Behavior),stat = "identity") +
#   labs(y="Change in revenue", x = "") +
#     scale_y_continuous(labels = comma) +
#   ggtitle("Growth Accounting - Annual Revenue")
#
#
# d1 <- d <- q[-1,]
# d$type <- "Quick Ratio = (New + Exp. + Res.) / (Con. + Churned)"
# d1$type = "4 is excellent for a business SaaS"
# d1$Quick.Ratio <- 4
# d <- rbind(d,d1)
# ggplot(data=d, aes(x = Period, y = Quick.Ratio, group=type, color = type)) +
#   geom_line() +
#   geom_line() + scale_y_continuous(limits = c(0, max(d$Quick.Ratio))) +
#   labs(y="Quick Ratio", x = "", legend = "") +
#   ggtitle("Quick Ratio - Annual Revenue")
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
# qApril2016$year <- strftime(as.Date(qApril2016$dt), "%Y")
#
# dau <- aggregate(inc_amt ~ name + dt, FUN = sum, data = dau)
# dau$month.of.year <- strftime(dau$dt, "%m")
# dau$year <- strftime(dau$dt, "%Y")
# dau$day.of.month <- strftime(dau$dt, "%d")
# library(lubridate)
# dau$week = floor_date(dau$dt, "week")
# dau$month = floor_date(dau$dt, "month")
# dau
#
#
#
#
#
#
#
# devtools::install_github('cmpolis/datacomb', subdir='pkg', ref='1.1.2');
# library(datacomb);
# Datacomb(iris)
#
#
# rownames(d) <- 1:nrow(d)
# Datacomb(d)
#
# z <- c(a = .13, b =.34)
# z <- percent(z)
# names(z) = LETTERS[1:2]
# z
#
#
#
# g.tab$quick <- apply(g.tab[,3:5], 1, sum) / apply(g.tab[,1:2], 1, sum)
#
#
# aggregate(AUD ~ year, data = d, FUN = sum)
#
#   aggregated$id.by.period <- paste(aggregated$ids, aggregated$time)
#
#   rep(periods, rep*)
#                          # Restructuring the data.
#   aggregated <- aggregate(value ~ id + period, FUN = sum)
#
#
#   period.min <- aggregate(aggregated$period ~ aggregated$id, FUN = min)
#   names(period.min) <- c("id", "first.period")
#   tidied <- merge(aggregated, period.min, by = "id")
#   sorted <- tidied[order(tidied$id,  tidied$period),]
#   difference <- aggr
#   period <- sorted$period
#   first.period <- sorted$first.period
#   value <- sorted$value
#   id <- sorted$id
#   customers <- aggregate(id ~ period, FUN = function(x) length(unique(x)))
#   .difference <- function(id, , periods)
#
#     period.names <- customers[, 1]
#   customers <- customers[, 2]
#   # Internal functions
#   .uniqueID <- function(x) {unique(id[x])}
#   .nUnique <- function(x) {length(.uniqueID(x))}
#   .sum <- function(x) {sum(value(x))}
#   # Computing the data frame.
#   results <- c(Customers = customers[1], Retained = 0, New = customers[1], Resurrected = 0, Churned = 0)
#   for (p in periods[-1])
#   {
#     cases.p <- period == p
#     n.new <- .nUnique(cases.p & first.period == p)
#     id.p <- .uniqueID(cases.p)
#     p_minus_1 <- p - 1
#     cases.p_minus_1 <- period == p_minus_1
#     id.p_minus_1 <- .uniqueID(cases.p_minus_1)
#     n.p <- .nUnique(cases.p)
#     lookup.retained <- id.p %in% id.p_minus_1
#     lookup.churned <- !(id.p_minus_1 %in% id.p)
#     id.churned <- id.p_minus_1[lookup.churned]
#     n.churned <- length(id.churned)
#     n.retained <- sum(lookup.retained)
#     n.non.new <- .nUnique(cases.p & first.period < p)
#     n.resurrected <- n.non.new - n.retained
#     results <- rbind(results,
#                      c(n.p, n.retained, n.new, n.resurrected, -n.churned))
#   }
#   rownames(results) <- periods
#   results <- as.data.frame(results)
#   results$Period <- periods
#   class(results) <- append(class(results), "CustomerGrowthTable")
#   results}
#
#
# library(lubridate)
# data(qApril2016)
# d <- qApril2016
# d$year <- floor_date(d$time, "year")
# d$year <- year(floor_date(d$time, "year"))
# g.tab <- CustomerGrowthTable(d$id, d$year, d$AUD)
# g.tab <- g.tab[-nrow(g.tab), ]
#
# dat <-  for example , you do this using melt:
#
#
#
#   # Churn
#     g.tab <- data.frame(rg$Table)
# g.tab$Period <- rownames(g.tab)
#   library(ggplot2)
# g.tab$Churn <- c(0, -g.tab$Churned[-1] / rg$Revenue[-nrow(g.tab)]) * 100
# g.tab$QuickRatio <- c(0, -g.tab$Churned[-1] / g.tab$Customers[-nrow(g.tab)]) * 100
# d <- reshape2::melt(g.tab, id.vars = "Period")
# names(d)[2] <- "Behavior"
#
# ggplot() +
#   geom_bar(data = d[d$Behavior == "New", ], aes(x = Period, y = value, fill = Behavior),stat = "identity") +
#   geom_bar(data = d[d$Behavior == "Retained", ], aes(x = Period, y = value, fill = Behavior),stat = "identity") +
#   geom_bar(data = d[d$Behavior == "Resurrected", ], aes(x = Period, y = value, fill = Behavior),stat = "identity") +
#   geom_bar(data = d[d$Behavior == "Churned", ], aes(x = Period, y = value, fill = Behavior),stat = "identity")
# +
#   scale_fill_brewer(type = "seq", palette = 1)
#
# # Annual active user growth accounting
# ggplot(d[d$Behavior %in% c("New", "Retained", "Resurrected"), ], aes(x = Period, y = value, fill = Behavior)) +
#   geom_bar(stat = "identity", position = "stack") +   geom_bar(data = d[d$Behavior == "Churned", ], aes(x = Period, y = value, fill = Behavior),stat = "identity") +
#   labs(y="Number of customers", x + "") +
#   ggtitle("Growth Accounting - Annual Active Users")
# d1 <- d <- g.tab[,c("Period", "q")]
# d$type <- "Quick Ratio = (New + Res.) / Churned"
# d1$type = "1.5 is good for a consumer product"
# d1$q <- 1.5
# d <- rbind(d,d1)
# ggplot(data=d, aes(x=Period, y=q, group=type, color = type)) +
#   geom_line() +
#   geom_line() + scale_y_continuous(limits = c(0, max(g.tab$q))) +
#   labs(y="Quick Ratio", x = "", legend = "") +
#   ggtitle("Quick Ratio - Annual Active Users") +
#   ggtitle("Quick Ratio - Annual Active Users")
#
#
#
#
#
# #' #' \code{Churn}
# #' #' @description Computes the number of customers to have churned.
# #' #' @param id A vector of characters, representing a unique identifier for customers.
# #' #' @param period A vector of class \code{"Date"} or \code{"Integer"}, which represents the time
# #' #' at which a transaction occurred.
# #' #' @param grace The number of days grace that should be used in the calculations. For example, a
# #' #' \code{grace} of 35 would mean a customer is considered to have churned if no transactions have
# #' #' occurred in the past 35
# #' #'
# #' #'   \code{"Integer"}, which represents the time
# #' #' at which a transaction occurred.
# #' #' @param value The value of the transaction.
# #' CustomerGrowthTable <- function(id, period, grace)
# #' {
# #'   # Restructuring the data.
# #'   aggregated <- aggregate(value ~ id + period, FUN = sum)
# #'   period.max <- aggregate(aggregated$period ~ aggregated$id, FUN = max)
# #'   names(period.min) <- c("id", "first.period")
# #'   tidied <- merge(aggregated, period.min, by = "id")
# #'   sorted <- tidied[order(tidied$id,  tidied$period),]
# #'   period <- sorted$period
# #'   first.period <- sorted$first.period
# #'   value <- sorted$value
# #'   id <- sorted$id
# #'   periods <- sort(as.numeric(as.character(unique(period))))
# #'   customers <- aggregate(id ~ period, FUN = function(x) length(unique(x)))
# #'   period.names <- customers[, 1]
# #'   customers <- customers[, 2]
# #'   # Internal functions
# #'   .uniqueID <- function(x) {unique(id[x])}
# #'   .nUnique <- function(x) {length(.uniqueID(x))}
# #'   # Computing the data frame.
# #'   results <- c("customers" = customers[1], retained = 0, new = customers[1], resurrected = 0, churned = 0)
# #'   for (p in periods[-1])
# #'   {
# #'     cases.p <- period == p
# #'     n.new <- .nUnique(cases.p & first.period == p)
# #'     id.p <- .uniqueID(cases.p)
# #'     p_minus_1 <- p - 1
# #'     cases.p_minus_1 <- period == p_minus_1
# #'     id.p_minus_1 <- .uniqueID(cases.p_minus_1)
# #'     n.p <- .nUnique(cases.p)
# #'     lookup.retained <- id.p %in% id.p_minus_1
# #'     lookup.churned <- !(id.p_minus_1 %in% id.p)
# #'     id.churned <- id.p_minus_1[lookup.churned]
# #'     n.churned <- length(id.churned)
# #'     n.retained <- sum(lookup.retained)
# #'     n.non.new <- .nUnique(cases.p & first.period < p)
# #'     n.resurrected <- n.non.new - n.retained
# #'     results <- rbind(results,
# #'                      c(n.p, n.retained, n.new, n.resurrected, -n.churned))
# #'   }
# #'   rownames(results) <- periods
# #'   as.data.frame(results)}
# #'
# #' aggregate()
# #'
# #'
# #' library(lubridate)
# #' d <- qApril2016
# #' d$year <- floor_date(d$time, "year")
# #' d$year <- year(floor_date(d$time, "year"))
# #' g.tab <- CustomerGrowthTable(d$id, d$year, d$AUD)
# #'
# #'
# #' data(qApril2016)
# #'
# #' ggplot() +
# #'   geom_bar(data = g.tab, aes(x=Year, y=OperatingIncome, fill=Division),stat = "identity") +
# #'   geom_bar(data = dat2, aes(x=Year, y=OperatingIncome, fill=Division),stat = "identity") +
# #'   scale_fill_brewer(type = "seq", palette = 1)
# #'
# #'
# #'
# #' ibrary(lubridate())
# #' qApril2016$year <- floor_date(
# #'
# #'
# #'   annual.data.123 <- data.frame(id = invoices$CustOrgID[filter],
# #'                                 time = year((as.Date(invoices$Issued[filter]),"year")),
# #'                                 value = as.numeric((invoices$Total)[filter]))
# #'
# #'
# #'   levels(qApril2016$currency)
# #'
# #'
# #' library(lubridate)
# #' m <- raw.data
# #' annual.growth.accounting <- CustomerGrowthTable(m$id,m$time, m$value)
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
# #' \code{CustomerGrowthTable}
# #' @description Computes statistics for use in growth accounting of a startup.
# #' @param id A vector of characters, representing a unique identifier for customers.
# #' @param period A vector of class \code{"Date"} or \code{"Integer"}, which represents the time
# #' at which a transaction occurred.
# #' @param value The value of the transaction.
# CustomerGrowthTable <- function(id, period, value)
# {
#   # Restructuring the data.
#   aggregated <- aggregate(value ~ id + period, FUN = sum)
#   period.min <- aggregate(aggregated$period ~ aggregated$id, FUN = min)
#   names(period.min) <- c("id", "first.period")
#   tidied <- merge(aggregated, period.min, by = "id")
#   sorted <- tidied[order(tidied$id,  tidied$period),]
#   difference <- aggr
#   period <- sorted$period
#   first.period <- sorted$first.period
#   value <- sorted$value
#   id <- sorted$id
#   periods <- sort(as.numeric(as.character(unique(period))))
#   customers <- aggregate(id ~ period, FUN = function(x) length(unique(x)))
#   .difference <- function(id, , periods)
#
#   period.names <- customers[, 1]
#   customers <- customers[, 2]
#   # Internal functions
#   .uniqueID <- function(x) {unique(id[x])}
#   .nUnique <- function(x) {length(.uniqueID(x))}
#   .sum <- function(x) {sum(value(x))}
#   # Computing the data frame.
#   results <- c(Customers = customers[1], Retained = 0, New = customers[1], Resurrected = 0, Churned = 0)
#   for (p in periods[-1])
#   {
#     cases.p <- period == p
#     n.new <- .nUnique(cases.p & first.period == p)
#     id.p <- .uniqueID(cases.p)
#     p_minus_1 <- p - 1
#     cases.p_minus_1 <- period == p_minus_1
#     id.p_minus_1 <- .uniqueID(cases.p_minus_1)
#     n.p <- .nUnique(cases.p)
#     lookup.retained <- id.p %in% id.p_minus_1
#     lookup.churned <- !(id.p_minus_1 %in% id.p)
#     id.churned <- id.p_minus_1[lookup.churned]
#     n.churned <- length(id.churned)
#     n.retained <- sum(lookup.retained)
#     n.non.new <- .nUnique(cases.p & first.period < p)
#     n.resurrected <- n.non.new - n.retained
#     results <- rbind(results,
#                      c(n.p, n.retained, n.new, n.resurrected, -n.churned))
#   }
#   rownames(results) <- periods
#   results <- as.data.frame(results)
#   results$Period <- periods
#   class(results) <- append(class(results), "CustomerGrowthTable")
#   results}
#
#
# library(lubridate)
# data(qApril2016)
# d <- qApril2016
# d$year <- floor_date(d$time, "year")
# d$year <- year(floor_date(d$time, "year"))
# g.tab <- CustomerGrowthTable(d$id, d$year, d$AUD)
# g.tab <- g.tab[-nrow(g.tab), ]
#
# dat <-  for example , you do this using melt:
#
#
#
# # Churn
# library(ggplot2)
# g.tab$Churn <- c(0, -g.tab$Churned[-1] / g.tab$Customers[-nrow(g.tab)]) * 100
# g.tab$QuickRatio <- c(0, -g.tab$Churned[-1] / g.tab$Customers[-nrow(g.tab)]) * 100
# d <- reshape2::melt(g.tab, id.vars = "Period")
# names(d)[2] <- "Behavior"
#
# ggplot() +
#   geom_bar(data = d[d$Behavior == "New", ], aes(x = Period, y = value, fill = Behavior),stat = "identity") +
#   geom_bar(data = d[d$Behavior == "Retained", ], aes(x = Period, y = value, fill = Behavior),stat = "identity") +
#   geom_bar(data = d[d$Behavior == "Resurrected", ], aes(x = Period, y = value, fill = Behavior),stat = "identity") +
#   geom_bar(data = d[d$Behavior == "Churned", ], aes(x = Period, y = value, fill = Behavior),stat = "identity")
# +
#   scale_fill_brewer(type = "seq", palette = 1)
#
# # Annual active user growth accounting
# ggplot(d[d$Behavior %in% c("New", "Retained", "Resurrected"), ], aes(x = Period, y = value, fill = Behavior)) +
#           geom_bar(stat = "identity", position = "stack") +   geom_bar(data = d[d$Behavior == "Churned", ], aes(x = Period, y = value, fill = Behavior),stat = "identity") +
#   labs(y="Number of customers", x + "") +
#   ggtitle("Growth Accounting - Annual Active Users")
# d1 <- d <- g.tab[,c("Period", "q")]
# d$type <- "Quick Ratio = (New + Res.) / Churned"
# d1$type = "1.5 is good for a consumer product"
# d1$q <- 1.5
# d <- rbind(d,d1)
# ggplot(data=d, aes(x=Period, y=q, group=type, color = type)) +
#   geom_line() +
#   geom_line() + scale_y_continuous(limits = c(0, max(g.tab$q))) +
#   labs(y="Quick Ratio", x = "", legend = "") +
#   ggtitle("Quick Ratio - Annual Active Users") +
#   ggtitle("Quick Ratio - Annual Active Users")
#
#
#
#
#
# #' #' \code{Churn}
# #' #' @description Computes the number of customers to have churned.
# #' #' @param id A vector of characters, representing a unique identifier for customers.
# #' #' @param period A vector of class \code{"Date"} or \code{"Integer"}, which represents the time
# #' #' at which a transaction occurred.
# #' #' @param grace The number of days grace that should be used in the calculations. For example, a
# #' #' \code{grace} of 35 would mean a customer is considered to have churned if no transactions have
# #' #' occurred in the past 35
# #' #'
# #' #'   \code{"Integer"}, which represents the time
# #' #' at which a transaction occurred.
# #' #' @param value The value of the transaction.
# #' CustomerGrowthTable <- function(id, period, grace)
# #' {
# #'   # Restructuring the data.
# #'   aggregated <- aggregate(value ~ id + period, FUN = sum)
# #'   period.max <- aggregate(aggregated$period ~ aggregated$id, FUN = max)
# #'   names(period.min) <- c("id", "first.period")
# #'   tidied <- merge(aggregated, period.min, by = "id")
# #'   sorted <- tidied[order(tidied$id,  tidied$period),]
# #'   period <- sorted$period
# #'   first.period <- sorted$first.period
# #'   value <- sorted$value
# #'   id <- sorted$id
# #'   periods <- sort(as.numeric(as.character(unique(period))))
# #'   customers <- aggregate(id ~ period, FUN = function(x) length(unique(x)))
# #'   period.names <- customers[, 1]
# #'   customers <- customers[, 2]
# #'   # Internal functions
# #'   .uniqueID <- function(x) {unique(id[x])}
# #'   .nUnique <- function(x) {length(.uniqueID(x))}
# #'   # Computing the data frame.
# #'   results <- c("customers" = customers[1], retained = 0, new = customers[1], resurrected = 0, churned = 0)
# #'   for (p in periods[-1])
# #'   {
# #'     cases.p <- period == p
# #'     n.new <- .nUnique(cases.p & first.period == p)
# #'     id.p <- .uniqueID(cases.p)
# #'     p_minus_1 <- p - 1
# #'     cases.p_minus_1 <- period == p_minus_1
# #'     id.p_minus_1 <- .uniqueID(cases.p_minus_1)
# #'     n.p <- .nUnique(cases.p)
# #'     lookup.retained <- id.p %in% id.p_minus_1
# #'     lookup.churned <- !(id.p_minus_1 %in% id.p)
# #'     id.churned <- id.p_minus_1[lookup.churned]
# #'     n.churned <- length(id.churned)
# #'     n.retained <- sum(lookup.retained)
# #'     n.non.new <- .nUnique(cases.p & first.period < p)
# #'     n.resurrected <- n.non.new - n.retained
# #'     results <- rbind(results,
# #'                      c(n.p, n.retained, n.new, n.resurrected, -n.churned))
# #'   }
# #'   rownames(results) <- periods
# #'   as.data.frame(results)}
# #'
# #' aggregate()
# #'
# #'
# #' library(lubridate)
# #' d <- qApril2016
# #' d$year <- floor_date(d$time, "year")
# #' d$year <- year(floor_date(d$time, "year"))
# #' g.tab <- CustomerGrowthTable(d$id, d$year, d$AUD)
# #'
# #'
# #' data(qApril2016)
# #'
# #' ggplot() +
# #'   geom_bar(data = g.tab, aes(x=Year, y=OperatingIncome, fill=Division),stat = "identity") +
# #'   geom_bar(data = dat2, aes(x=Year, y=OperatingIncome, fill=Division),stat = "identity") +
# #'   scale_fill_brewer(type = "seq", palette = 1)
# #'
# #'
# #'
# #' ibrary(lubridate())
# #' qApril2016$year <- floor_date(
# #'
# #'
# #'   annual.data.123 <- data.frame(id = invoices$CustOrgID[filter],
# #'                                 time = year((as.Date(invoices$Issued[filter]),"year")),
# #'                                 value = as.numeric((invoices$Total)[filter]))
# #'
# #'
# #'   levels(qApril2016$currency)
# #'
# #'
# #' library(lubridate)
# #' m <- raw.data
# #' annual.growth.accounting <- CustomerGrowthTable(m$id,m$time, m$value)
# #'
# #'
# #'
# #'
# #'
#
#
#
#
#
#
#
#
# tenure.by.year <- .aggregate.xtabs(value ~ tenure + start, data = data, FUN)
#
#
#
# data(qApril2016)
# dat <- TidyRevenueData(qApril2016$inc_amt, lubridate::dmy_hms(qApril2016$dt), qApril2016$name, "year")
#
#
#
#
# #' \code{AverageRevenuePerCustomerOverTime}
# #' @description Creates a crosstab by aggregating numeric data over factors.
# #' @param formula A \code{formula} where the dependent variable is the variable to be aggregated over.
# #' @param FUN the function to be applied: see \code{apply} for details.
# #' @export
# {
#     xtabs(formula, data = aggregate(formula, data, FUN = FUN))
# }
# tenure.by.year <- .aggregate.xtabs(value ~ tenure + year, data = data, FUN)
#
#
#
#
#
#
#
# dstrat<-svydesign(id=~1,strata=~strata, weights=~weight, data=df, fpc=~fpc)
# svymean(~answer, dstrat)
#
#
#
#
#
#
#
#
#
#
# year <- .aggregate.xtabs(value ~ year, data = data, FUN)
# tenure <- .aggregate.xtabs(value ~ tenure, data = data, FUN)
# tenure.by.year <- .aggregate.xtabs(value ~ tenure + year, data = data, FUN)
# start.by.year <- .aggregate.xtabs(value ~ start + year, data = data, FUN)
# list(year = year, tenure = tenure, tenure.by.year = tenure.by.year, start.by.year = start.by.year)
# #    terminated
# #    start
# #    strtrim(   , trim.id)
# }
#
#
#
#
#
#
#
#
# #' \code{AverageRevenuePerCustomerOverTime}
# #' @description Computes the  statistics for use in growth accounting of a startup.
# #' @param value A vector of containing the revenue per transaction.
# #' @param data A vector of class \code{POSIXct}/\code{POSIXt}, recording the date/time of each transaction.
# #' @param id A vector of \code{character}, unique identifier for customers that made the transactions (e.g., email addresses, names, customer keys).
# #' @param by \code{year} to view the data by year, \code{quarter}, and \code{month}.
# #' @param tolerance The tolerance used in determining if a customer has ceased to be a customer. E.g., if yearly date, a value of .1 means
# #' that a customer will be assumed to be still a customer if they have purcahsed within 1 year + 10% of a year.
# #' #' @export
# RevenuePerCustomerOverTime <- function(value, date, id, by = "year", end = Sys.time(), FUN = mean, trim.id = 20, tolerance = .1)
# {
#     data <- data.frame(value, date, id)
#     zero <- data$value == 0
#     n.zero <- sum(zero)
#     if (n.zero > 0)
#     {
#         cat(paste0(n.zero, " transactions removed due to having 0 value.\n"))
#         data <- data[!zero, ]
#     }
#     start <- aggregate(date ~ id, data, min)
#     id <- start$id
#     start <- start$date
#     last <- aggregate(date ~ id, data, max)$date
#     requireNamespace("lubridate")
#     dys <- ceiling((1 + tolerance) *
#                        switch(by, year = 365.25, quarter = 365.25, month = 30, week = 7))
#
#     cutoff <- end - days(dys)
#     churned <- last < cutoff
#     overall.tenure <- interval(start, last)
#     current.tenure <- interval(start, xxxxxxxx)
#     units <- switch(by, year = years(1), quarter = quarter(1), month = month(1), week = week(1))
#     #print(units)
#     #print(table(diff %/% units))
#     #stop("dog")
#     id.lookup <- match(data$id, id)
#     data$tenure <- (tenure %/% units)[id.lookup]
#     data$start <- start[id.lookup]
#     data$year <- year(floor_date(data$date, by))
#
#
#     year <- .aggregate.xtabs(value ~ year, data = data, FUN)
#     tenure <- .aggregate.xtabs(value ~ tenure, data = data, FUN)
#     tenure.by.year <- .aggregate.xtabs(value ~ tenure + year, data = data, FUN)
#     start.by.year <- .aggregate.xtabs(value ~ start + year, data = data, FUN)
#     list(year = year, tenure = tenure, tenure.by.year = tenure.by.year, start.by.year = start.by.year)
#     #    terminated
#     #    start
#     #    strtrim(   , trim.id)
# }
#


