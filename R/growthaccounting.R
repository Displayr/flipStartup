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

#' @importFrom ggplot2 ggplot geom_bar aes scale_y_continuous ggtitle theme_bw
#' @importFrom scales comma
#' @importFrom reshape2 melt
#' @export
plot.RevenueGrowthAccounting <- function(x, ...)
{
    t <- x$Table
    t$Period <- rownames(t)

#
#     p <- plot_ly(data = t,
#       x = Period,
#       y = New,
#       #name = "New",
#       type = "bar")
#
#     p <- add_trace(
#       p,
#       x = Period,
#       y = Expansion,
#       #name = "LA Zoo",
#       type = "bar")
#     p <- add_trace(
#       p,
#       x = Period,
#       y = Resurrected,
#       #name = "LA Zoo",
#       type = "bar")
#     p <- add_trace(
#       p,
#       x = Period,
#       y = Churned,
#       #name = "LA Zoo",
#       type = "bar")
#     p <- add_trace(
#       p,
#       x = Period,
#       y = Contraction,
#       #name = "LA Zoo",
#       type = "bar")
#     p_final <- layout(p, barmode = 'relative')
#     p_final
    t <- melt(t, id.vars = "Period")
    names(t)[2] <- "Behavior"
    p <- ggplot(t[t$Behavior %in% c("New", "Expansion", "Resurrected"), ],
                aes_string(x = "Period", y = "value", fill = "Behavior")) +
        geom_bar(data = t[t$Behavior %in% c("Churned", "Contraction"), ],
                 aes_string(x = "Period", y = "value", fill = "Behavior"),stat = "identity") +
        geom_bar(stat = "identity", position = "stack") +
        labs(y="Change in revenue", x = "") +
        scale_y_continuous(labels = comma) + #scale_y_continuous
        ggtitle("Growth Accounting - Annual Revenue")
    p <- p + theme_bw()
    suppressWarnings(print(p))
}

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
    growth.table$Benchmark = 4
    p <- plot_ly(x = rownames(growth.table), hoverinfo="x+text", text=sprintf("%.2f", growth.table$QuickRatio), y = growth.table$Quick.Ratio, type="line", name = "Quick Ratio<br>(New + Exp. + Res.) / (Con. + Churned)")#, marker = list(color = "green"))
    p <- add_trace(x = rownames(growth.table), y = growth.table$Benchmark, type="line",
                   hoverinfo="x+text", text=sprintf("%.2f", growth.table$Benchmark), name = "4 is excellent for a business SaaS")#, marker = list(color = "blue"))
    p <- layout(xaxis = list(title = "Period"), yaxis = list(title = "Quick Ratio", range = c(0, max(growth.table$Quick.Ratio, 4))))
    p
}
