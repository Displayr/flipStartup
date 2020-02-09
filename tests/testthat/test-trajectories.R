# context("Trajectories")
# data(q.invoice.lines)

# 
# 
# d = q.invoice.lines[q.invoice.lines$validInvoice == 1, ]
# tr = Trajectories(d$numberTransferable * 3 + d$numberStandard, d$ValidFrom, d$ValidTo, d$CustOrgID)
# df = as.data.frame(tr)
# df = subset(df, df$min.value > 0 & df$n.observations > 1)
# head(df)
# #n.purchases = sapply(tr, length)
# #tr[n.purchases > 1]
# library(lubridate)
# 
# library(lcmm)
# m1 = hlme(index ~ poly(years, 4), subject = "id", data = df)
# summary(m1)
# plot(m1)
# plot(predict(m1))
# m2 = hlme(index ~ poly(years, 1), subject = "id", data = df, ng = 2, mixture = ~poly(years, 1), subset = df$n.observations > 1)
# m2
# library(flexmix)
# 
# m1 <- flexmix(index ~ years | id, k = 1, data = df)
# summary(refit(m1))
# 
# m2 <- flexmix(index ~ years | id, k = 2, data = df)
# summary(refit(m2))
# 
# 
# 
# df$zdz = df$index + rnorm(nrow(df), 0, 1)
# 
# # Choosing the number of componets
# k <- 1
# best.bic <- Inf
# bic <- Inf
# while (bic <= best.bic)
# {
#     m = flexmix(index ~ poly(years, 4)  | id, k = k, data = df, control = list(verb = 1, minprior = 0.001))
#     bic <- BIC(m)
#     if (bic < best.bic)
#     {
#         k <- k + 1
#         best.bic = bic
#     }
# }
# 
# 
# summary(m1,m2)
# getModel(m2, "BIC")
