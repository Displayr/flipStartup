context("charts")


# Heatmap.
mydata <- mtcars[, c(1,3,4,5,6,7)]
cormat <- round(cor(mydata),2)
heatmap(cormat)
Heatmap(cormat)
cormat[lower.tri(cormat)] <- NA
Heatmap(cormat)
names(dimnames(cormat)) <- c("dog","cat")
Heatmap(cormat, title = "Churn")

rd <- RevenueData(d$AUD, from , to, end = end, id = d$name, by = "year", subset = d$validInvoice == 1)
retention <- Retention(rd, FALSE)

Heatmap(t(retention$index), "Retention")
Heatmap(retention$volume * 100, "Retention (%)")
Heatmap(retention$retention * 100, "Retention (%)")


l <- LifetimeValue(rd)
Heatmap(l$total, "Total")
Heatmap(l$mean, "Mean")
Heatmap(l$cumulative, "Cumulative")
Heatmap(l$index, "Index")


rd <- RevenueData(d$AUD, from , to, end = end, id = d$name, by = "year", subset = d$validInvoice == 1)
LayerCake(rd)



l


library(plotly)
m <- matrix(1:9, nrow = 3, ncol = 3)
dimnames(m) <- list(x = c("a", "b", "c"), y = c("d", "e", "f"))
plot_ly(z = m,
        x = colnames(m),
        y = rownames(m),
        type = "heatmap")



