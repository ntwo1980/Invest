x <- stocks.whole790[, c("Date", "Close")]
y <- stocks.whole150[, c("Date", "Close")]
x <- xts(x[, -1], order.by = x[, 1])
y <- xts(y[, -1], order.by = y[, 1])

v <- x / y
dF <- cbind(x, y, v, lag(v, 5))
names(dF) <- c("x", "y", "v", "va")
dF <- dF[complete.cases(dF),]
dF <- cbind(dF, apply(dF[, c("x", "y")], 2, SMA, n = 10))
names(dF)[5:6] <- c("xma10", "yma10")
dF <- cbind(dF, apply(dF[, c("x", "y")], 2, SMA, n = 40))
names(dF)[7:8] <- c("xma20", "yma20")

quantile.value <- function(x, q) {
    return(quantile(x, q))
}
t <- rollapply(dF$va, width = 30, FUN = quantile.value, by.column = F, align = "right", c(0.0, 0.5, 1))
#t <- rollapply(dF$va, width = 30, FUN = quantile.value, by.column = F, align = "right", c(0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1))
#t <- rollapply(dF$va, width = 240, FUN =  quantile.value, by.column = F, align = "right", c(0.0, 0.2, 0.8, 1))
names(t) <- c("p0", "p5", "p100")
dF <- merge(dF, t)
#dF <- transform(dF,
       #l0 = v < p0,
       #l1 = v < p1,
       #l2 = v < p2,
       #l3 = v < p3,
       #l4 = v < p4,
       ##l20 = v < p20,
       ##h80 = v > p80,
       #h6 = v > p6,
       #h7 = v > p7,
       #h8 = v > p8,
       #h9 = v > p9,
       #h100 = v > p100)

#dF <- tail(dF, 240)

#l0 = dF[dF$l0 == 1,]
#l1 = dF[dF$l1 == 1,]
#l2 = dF[dF$l2 == 1,]
#l3 = dF[dF$l3 == 1,]
#l4 = dF[dF$l4 == 1,]
#h6 = dF[dF$h6 == 1,]
#h7 = dF[dF$h7 == 1,]
#h8 = dF[dF$h8 == 1,]
#h9 = dF[dF$h9 == 1,]
#h10 = dF[dF$h10 == 1,]
##x <- dF[, c("l20", "h80")]
#deal <- rbind(l0, h10)
#deal.diff <- diff(deal$l0)
#deal <- merge(deal, deal.diff)
#colnames(deal)[ncol(deal)] <- "pchange"
#deal <- deal[deal$pchange != 0,] 

dF <- dF[complete.cases(dF),]

last.daily.deal <- NULL

d <- t(apply(dF, 1, function(daily.deal) {
    x <- daily.deal[["x"]]
    y <- daily.deal[["y"]]
    v <- daily.deal[["v"]]
    va <- daily.deal[["va"]]
    xma10 <- daily.deal[["xma10"]]
    yma10 <- daily.deal[["yma10"]]
    xma20 <- daily.deal[["xma20"]]
    yma20 <- daily.deal[["yma20"]]
    low <- daily.deal[["p0"]]
    med <- daily.deal[["p5"]]
    high <- daily.deal[["p100"]]
    xc <- ifelse(!is.null(last.daily.deal), last.daily.deal["xc"], 0)

    yc <- ifelse(!is.null(last.daily.deal), last.daily.deal["yc"], 0)
    value <- ifelse(xc > 0, xc * x, yc * y)
    value <- ifelse(value > 0, value, ifelse(!is.null(last.daily.deal), last.daily.deal["value"], 10000))

    if (x > 971.3 && x < 971.5 && y > 5010 && y < 5012) {
        tt <- 3
    }

    if (x >= xma20) {
        if (yc > 0 && v < low && v > va) {
            # sell y, buy x
            yc <- 0
            xc <- value * 0.99 / x
        }

        if (xc == 0 && yc == 0 && v < med) {
            #buy x
            xc <- value * 0.995 / x
        }
    }
    else if(y < yma20) {
        xc <- 0
    }

    if (y >= yma20) {
        if (xc > 0 && v > high && v < va) {
            # sell x, buy y
            xc <- 0
            yc <- value * 0.99 / y
        }

        if (xc == 0 && yc == 0 && v > med) {
            #buy y
            yc <- value * 0.995 / y
        }
    }
    else if(x < xma20) {
        yc <- 0
    }

    last.daily.deal <<- c(daily.deal, xc = xc, yc = yc, value = value)
    return(c(xc = xc, yc = yc, value = value))
}))

rownames(d) <- NULL

dF <- cbind(dF, d)
dF <- cbind(dF, dF$x > dF$xma20 | dF$y > dF$yma20)
