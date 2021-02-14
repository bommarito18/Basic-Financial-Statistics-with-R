library(tidyquant)
library(dplyr)

amzn = tq_get("AMZN",from="2010-01-01", to="2021-02-13")

qqq = tq_get("QQQ",from="2010-01-01", to="2021-02-13")

goog = tq_get("GOOG",from="2010-01-01", to="2021-02-13")

plot(amzn$close, qqq$close, pch = 19, col="lightblue", main = "Relation between AMZN and QQQ", xlab = "AMZN", ylab = "QQQ")

par(mfrow=c(1,3))

plot(amzn$close, qqq$close, pch = 19, col="lightblue", main = "Relation between AMZN and QQQ", xlab = "AMZN", ylab = "QQQ")

plot(goog$close, qqq$close, pch = 19, col="red", main = "Relation between GOOG and QQQ", xlab = "AMZN", ylab = "QQQ")

plot(amzn$close, goog$close, pch = 19, col="orange", main = "Relation between AMZN and GOOG", xlab = "AMZN", ylab = "GOOG")

#==============================================================================================================================

port.symbl <- tibble(stocks = c("AMZN", "BAC", "JNJ", "GE"),
                     industry = c("Technology", "Financial", "Healthcare", "Energy"))

port.a = tq_get(port.symbl, get = "stock.prices", from="2010-01-01", to = "2021-02-13")
tech = filter(port.a, industry=="Technology")

summary(tech)

qtl = quantile(tech$close, prob = c(.05,.10,.25,.50,.75,.95,.99), na.rm=TRUE)

iqr = IQR(tech$close)

quantile(tech$close, prob = c(0.75))-quantile(tech$close, prob=c(0.25))

var = var(tech$close)

var

stdev = sd(tech$close)

stdev

port.grp = group_by(port.a, stocks)

summarise(port.grp, avg = mean(close),
          n = n(),
          minimum = min(close),
          maximum = max(close),
          vlty = sd(close))
