if (!exists("stocks.whole790"))
    source("sw.R")

#x <- stocks.whole790[, c("Date", "Close", "Ret")]
#y <- stocks.whole150[, c("Date", "Close", "Ret")]

dt <- stocks.whole150[, c("Date", "Close")]
dt <- transform(dt, MA60 = SMA(Close, 60), MA240 = SMA(Close, 240))
dt <- transform(dt, MA240.DIFF = (Close - MA240) / MA240 * 100)
dt <- xts(dt[, -1], order.by = dt[, 1])
dt <- do.call(rbind, lapply(split(dt, "months"), first))

dt <- dt[complete.cases(dt),]
row.count = nrow(dt)
count <- c(rep(0, row.count))
total <- c(rep(0, row.count))
value <- c(rep(0, row.count))
profit <- c(rep(0, row.count))
last.row <- NULL

for (i in 1:row.count) {
    row = dt[i,]

    close = as.numeric(row$Close)
    ma60 = as.numeric(row$MA60)
    ma240.diff = as.numeric(row$MA240.DIFF)
    last.count <- ifelse(i == 1, 0, count[i - 1])
    last.total <- ifelse(i == 1, 0, total[i - 1])
    OP <- F

    #if (close < ma60 && last.count > 0) {
    if (close < ma60 && ma240.diff > 0 && last.count > 0 ) {
        if (last.count * close) {
            profit[i] <- last.count * close * 0.99 - last.total
            total[i] <- 0
            count[i] <- 0
            value[i] <- 0

            OP <- T
        }
    }

    if (ma240.diff > 0 && last.count == 0) {
        total.profit <- sum(profit)

        total[i] <- ifelse(total.profit > 10000 , total.profit, 10000) + i * 10000
        count[i] <- total[i] * 0.99 / close
        value[i] <- count[i] * close

        OP <- T
    }

    if (!OP && last.count > 0) {
        total[i] <- last.total + 10000
        count[i] <- last.count + (10000 * 0.99 / close)
        value[i] <- count[i] * close
    }

    last.row <- row
}

dt <- cbind(dt, count = count, total = total, value = value, profit = profit)
