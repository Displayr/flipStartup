#' \code{RevenueGrowthAccounting}
#' @description Computes statistics for use in growth accounting of a startup.
#' @param data A \code{data.frame} that has variables: \code{id}, \code{period},
#' and \code{value} (e.g., a \code{RevenueData} object).
#' @param remove.last Remove the final period (as usually is incomplete).
#' @param tol The tolerance used in calculations of differences. This defaults to 1 (e.g., $1).
#' Values or differences less than this amount are treated as being equivalent. E.g., if revenue for
#' an entity this period is less than \code{tol} higher than in the previous period, it is treated as being constant.
#' @importFrom stats aggregate xtabs
#' @importFrom methods is
#' @export
RevenueGrowthAccounting <- function(data, remove.last = TRUE, tol = 1)
{
    if (remove.last)
        data <- subset(data, data$period != max(data$period))
    id <- data$id
    period <- data$period
    value <- data$value
    id.by.period <- paste(id, period)
    aggregated <- aggregate(value ~ id.by.period, FUN = sum)
    ids <- sort(unique(id))
    periods <- sort(unique(period))
    n.periods <- length(periods)
    n.id <- length(ids)
    id.by.period <- data.frame(id = rep(ids, rep(n.periods + 1, n.id)))
    id.by.period$id.by.period <-  paste(rep(ids, rep(n.periods + 1, n.id)),
      c(0, periods))
    data <- merge(as.data.frame(id.by.period), as.data.frame(aggregated),
                by = "id.by.period", all.x = TRUE, sort = TRUE)
    data$value[is.na(data$value)] <- 0
    data$diff <- c(0, data$value[-1] - data$value[-length(data$value)])
    data$cum <- cumsum(data$value)
    data$cum <- data$cum - rep(data$cum[c(TRUE, rep(FALSE, n.periods))], rep(n.periods + 1, n.id))
    data <- data[c(FALSE, rep(TRUE, n.periods)), ]
    data$period <- periods
    data$status = "Unchanged"
    data$status[data$diff >= tol] <- "Expansion"
    data$status[data$value > tol & abs(data$diff - data$cum) < tol] <- "New"
    data$status[abs(data$value - data$diff) < tol & data$cum >= data$value + tol] <- "Resurrected" #+1 is to take numeric precision of cumsum into account.
    data$status[data$diff <= -tol] <- "Contraction"
    data$status[abs(data$value) < tol & data$diff <= -tol] <- "Churned"
    data$status[abs(data$diff) < tol] <- "Unchanged"
    tab <- aggregate(diff ~ status + period, data = data, sum)
    tab$period.by.status <- paste(tab$period,tab$status)
    stati <- c("New", "Expansion", "Resurrected", "Contraction", "Churned", "Unchanged")
    full.tab <- data.frame(period = rep(periods, rep(6, n.periods)), status = stati)
    full.tab$period.by.status <-  paste(rep(periods, rep(6, n.periods)), stati)
    tab <- merge(full.tab, tab[,-1:-2], by = "period.by.status", all.x = TRUE, sort = TRUE)
    diff <- tab$diff
    diff[is.na(diff)] <- 0
    tab <- as.data.frame(matrix(diff, nrow = n.periods,byrow = TRUE, dimnames = list(periods, sort(stati))))
    #print(tab)
    tab$Quick.Ratio <- -apply(tab[, -1:-2], 1, sum) / apply(tab[, 1:2], 1, sum)
    results <- list(n.id = n.id, n.periods = n.periods, periods = periods, data = data[, -1], Table = tab,
                  Revenue = aggregateAsVector(aggregate(value ~ period, data = data, FUN = sum)),
                  Growth = aggregateAsVector(aggregate(diff ~ status, data = data, FUN = sum)))
    class(results) <- append(class(results), "RevenueGrowthAccounting")
    results}

#' @export
print.RevenueGrowthAccounting <- function(x, ...)
{
    #require(formattable)
    cat('Revenue Growth calculations\n\n')
    cat(paste0('\nNumber of entities: ', x$n.id, '\n'))
    cat('\nRevenue\n')
    print(x$Revenue)
    cat('\nRevenue Growth\n')
    pre <- x$Revenue[-x$n.periods]
    revenue.growth <- round((x$Revenue[-1] - pre) / pre * 100, 0)
    names(revenue.growth) <- x$periods[-1]
    print(revenue.growth)
    cat('\nGrowth\n')
    print(x$Growth)
    cat('\nTable\n')
    print(x$Table)
    #cat('\n')
}

#' @importFrom ggplot2 ggplot geom_bar aes scale_y_continuous ggtitle
#' @importFrom scales comma
#' @importFrom reshape2 melt
#' @export
plot.RevenueGrowthAccounting <- function(x, ...)
{
    growth.table <- x$Table
    growth.table$Period <- rownames(growth.table)
    t <- melt(growth.table, id.vars = "Period")
    names(t)[2] <- "Behavior"
    p <- ggplot(t[t$Behavior %in% c("New", "Expansion", "Resurrected"), ],
                aes_string(x = "Period", y = "value", fill = "Behavior")) +
        geom_bar(data = t[t$Behavior %in% c("Churned", "Contraction"), ],
                 aes_string(x = "Period", y = "value", fill = "Behavior"),stat = "identity") +
        geom_bar(stat = "identity", position = "stack") +
        labs(y="Change in revenue", x = "") +
        scale_y_continuous(labels = comma) + #scale_y_continuous
        ggtitle("Growth Accounting - Annual Revenue")
    p <- p + ggplot2::theme_bw()
    suppressWarnings(print(p))
    #ggplotly(p)
#     p <- p + ggplot2::geom_point(size = point.size, ggplot2::aes(colour = group))
# #    p <- p + geom_text_repel(aes(x = xlab, y = ylab, colour = group, label = label.coords))
#     set.seed(1)
#    # if (auto.tidy)
#         p <- p + ggrepel::geom_text_repel(ggplot2::aes(label = row.labels,  group = group, colour = group),
#              box.padding = ggplot2::unit(0.35, "lines"),
#             point.padding = ggplot2::unit(0.3, "lines"), size = label.font.size / 14 * 5)
# #    else
#  #       p <- p + ggplot2::geom_text(data = coords,
#   #          ggplot2::aes_string(x = "col1", y = "col2",
#    #         label = row.labels, group = "group", colour = "group",
#     #        lineheight = 1.02),
#      #       size = , size = label.font.size / 14 * 5, show.legend = FALSE)
#         # p <- p + ggplot2::aes_string(x = "col1", y = "col2", label = "labels", group = "group", colour = "group",
#         #  lineheight = 1.02),size = label.font.size, show.legend = F)
#
#     #p <- p + ggplot2::geom_text(data = label.coords,
#         # ggplot2::aes_string(x = "col1", y = "col2",
#         #     label = "labels", group = "group", colour = "group",
#         #     lineheight = 1.02),
#         # size = font.size.hack, show.legend = F)
#     p <- p + ggplot2::labs(title = title, x = column.labels[1], y = column.labels[2])#, label = "labels")
#     p <- p + ggplot2::xlim(smallest.x, biggest.x) + ggplot2::ylim(smallest.y, biggest.y)
#     p <- p + ggplot2::scale_colour_manual(values = colors, name = group.name)
#     if (fixed.aspect)
#       p <- p + ggplot2::coord_fixed(ratio = 1)#, xlim = NULL, ylim = NULL, wise = NULL)
#     if (has.groups)
#     {
#         p <- p + ggplot2::theme(legend.text = ggplot2::element_text(colour = general.color, size = legend.font.size)) + # * 5 / 14)) +
#             ggplot2::guides(colour = ggplot2::guide_legend(override.aes = list(size = 4)))
#     }
#     else
#         p <- p + ggplot2::theme(legend.position = "none")
#     p <- p + ggplot2::theme(axis.text.x = ggplot2::element_text(colour = general.color, size = axis.label.font.size))
#     p <- p + ggplot2::theme(axis.text.y = ggplot2::element_text(colour = general.color, size = axis.label.font.size))
#     p <- p + ggplot2::theme(axis.title.x = ggplot2::element_text(colour = general.color, size = axis.title.font.size))
#     p <- p + ggplot2::theme(axis.title.y = ggplot2::element_text(colour = general.color, size = axis.title.font.size))
#     p <- p + ggplot2::theme(axis.title = ggplot2::element_text(size = axis.title.font.size, face = "bold"))
#     p <- p + ggplot2::theme(plot.title = ggplot2::element_text(size = title.font.size, face="bold", vjust = 1.2))
}

#
# z <- data.frame(Amount = unlist(rg$Table), Period = rownames(rg$Table), Type = rep(colnames(rg$Table), rep(8,7)))
# #p <- plot_ly(data=z, x=Period, y=Amount, color=Type, type="bar")
# zz <- as.data.frame(rg$Table)
# p <- plot_ly(x=rownames(zz), y=-zz$Churn, type="bar", marker = list(color = "green"))
# p <- add_trace(x=rownames(zz), y=zz$New, type="bar", marker = list(color = "blue"))
# p <- layout(barmode="stack")
# p
#
# p <- plot_ly(x = as.character(2000:2010), y = -1:-11, type="bar", marker = list(color = "green"))
# p <- add_trace(x = as.character(2000:2010), y = 1:11, type="bar", marker = list(color = "blue"))
# p <- layout(barmode = "stack")
# p
#
#
#
# plot2 <- function(x, ...)
# {
#     growth.tab <- x$Table
#     plot_ly(x=rownames*x)
# }
#' QuickRatioPlot
#'
#' @param x A RevenueGrowthAccounting object.
#' @param supress.first The periods to suppress from the beginning of the data
#' @importFrom plotly plot_ly add_trace layout
#' @export
QuickRatioPlot <- function(x, supress.first = 1)
{
    growth.table <- x$Table
    if (supress.first != 0)
    {
        growth.table <- growth.table[-1:-supress.first, ]
    }
    growth.table$Period <- rownames(growth.table)
#    q <- growth.table[, 7:8] #Quick ratio
#    d <- growth.table[-nrow(growth.table), 1:5]
#    d1 <- d <- q[-1,]
    growth.table$Benchmark = 4

p <- plot_ly(x = rownames(growth.table), hoverinfo="x+text", text=sprintf("%.2f", growth.table$QuickRatio), y = growth.table$Quick.Ratio, type="line", name = "Quick Ratio<br>(New + Exp. + Res.) / (Con. + Churned)")#, marker = list(color = "green"))
p <- add_trace(x = rownames(growth.table), y = growth.table$Benchmark, type="line",
               hoverinfo="x+text", text=sprintf("%.2f", growth.table$Benchmark), name = "4 is excellent for a business SaaS")#, marker = list(color = "blue"))
p <- layout(xaxis = list(title = "Period"), yaxis = list(title = "Quick Ratio", range = c(0, max(growth.table$Quick.Ratio, 4))))
p
# p <- layout(barmode="stack")


#     d <- rbind(growth.table, growth.table)
#     k <- nrow(growth.table)
#     d$Line <- c(rep("Quick Ratio = (New + Exp. + Res.) / (Con. + Churned)", k),
#                 rep("4 is excellent for a business SaaS", k))
#     d$Quick.Ratio[-1:-k] <- 4
# #
# #
# #     d1$Quick.Ratio <- 4
#     max.quick.ratio <- max(d$Quick.Ratio)
#     # d <- rbind(d,d1)
    # p <- ggplot(data=d, aes_string(x = "Period", y = "Quick.Ratio", group="Line", color = "Line")) +
    #     geom_line() +
    #     theme(legend.title=element_blank()) +
    #     geom_line() + scale_y_continuous(limits = c(0, max.quick.ratio)) +
    #     labs(y="Quick Ratio", x = "", legend = "") +
    #     ggtitle("Quick Ratio")
    # ggplotly(p)
}



#'
#'
#' #' \code{RevenuePerSubscriberOverTime}
#' #' @description Computes the  statistics for use in growth accounting of a startup.
#' #' @param value A vector of containing the revenue per transaction.
#' #' @param data A vector of class \code{POSIXct}/\code{POSIXt}, recording the date/time of each transaction.
#' #' @param id A vector of \code{character}, unique identifier for Subscribers that made the transactions (e.g., email addresses, names, Subscriber keys).
#' #' @param by \code{year} to view the data by year, \code{quarter}, and \code{month}.
#' #' @param tolerance The tolerance used in determining if a Subscriber has ceased to be a Subscriber. E.g., if yearly date, a value of .1 means
#' #' that a Subscriber will be assumed to be still a Subscriber if they have purcahsed within 1 year + 10% of a year.
#' #' #' @export
#' RevenuePerSubscriberOverTime <- function(value, date, id, by = "year", end = Sys.time(), FUN = mean, trim.id = 20, tolerance = .1)
#' {
#'     data <- data.frame(value, date, id)
#'     zero <- data$value == 0
#'     n.zero <- sum(zero)
#'     if (n.zero > 0)
#'     {
#'         cat(paste0(n.zero, " transactions removed due to having 0 value.\n"))
#'         data <- data[!zero, ]
#'     }
#'     start <- aggregate(date ~ id, data, min)
#'     id <- start$id
#'     start <- start$date
#'     last <- aggregate(date ~ id, data, max)$date
#'     requireNamespace("lubridate")
#'     dys <- ceiling((1 + tolerance) *
#'                        switch(by, year = 365.25, quarter = 365.25, month = 30, week = 7))
#'
#'     cutoff <- end - days(dys)
#'     churned <- last < cutoff
#'     overall.tenure <- interval(start, last)
#'     current.tenure <- interval(start, xxxxxxxx)
#'     units <- switch(by, year = years(1), quarter = quarter(1), month = month(1), week = week(1))
#'     #print(units)
#'     #print(table(diff %/% units))
#'     #stop("dog")
#'     id.lookup <- match(data$id, id)
#'     data$tenure <- (tenure %/% units)[id.lookup]
#'     data$start <- start[id.lookup]
#'     data$year <- year(floor_date(data$date, by))
#'
#'
#'     year <- Table(value ~ year, data = data, FUN)
#'     tenure <- Table(value ~ tenure, data = data, FUN)
#'     tenure.by.year <- Table(value ~ tenure + year, data = data, FUN)
#'     start.by.year <- Table(value ~ start + year, data = data, FUN)
#'     list(year = year, tenure = tenure, tenure.by.year = tenure.by.year, start.by.year = start.by.year)
#'     #    terminated
#'     #    start
#'     #    strtrim(   , trim.id)
#' }

#' #' \code{AverageRevenuePerSubscriberOverTime}
#' #' @description Creates a crosstab by aggregating numeric data over factors.
#' #' @param formula A \code{formula} where the dependent variable is the variable to be aggregated over.
#' #' @param FUN the function to be applied: see \code{apply} for details.
#' #' @export
#' {
#'     xtabs(formula, data = aggregate(formula, data, FUN = FUN))
#' }
