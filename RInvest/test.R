backtest <- function(x, y, ma.period, v.ma.period, vl, w, lp, hp) {
    x <- xts(x[, -1], order.by = x[, 1])
    y <- xts(y[, -1], order.by = y[, 1])

    v <- x / y
    dF <- cbind(x, y, v, lag(v, vl))
    names(dF) <- c("x", "y", "v", "va")
    dF <- dF[complete.cases(dF),]
    dF <- cbind(dF, apply(dF[, c("x", "y")], 2, SMA, n = ma.period))
    dF <- cbind(dF, apply(dF[, c("v")], 2, EMA, n = v.ma.period))
    names(dF)[5:7] <- c("xma", "yma", "vma")

    quantile.value <- function(x, q) {
        return(quantile(x, q))
    }
    t <- rollapply(dF$va, width = w, FUN = quantile.value, by.column = F, align = "right", c(lp, 0.5, hp))
    names(t) <- c("lp", "mp", "hp")
    dF <- merge(dF, t)
    dF <- dF[complete.cases(dF),]

    last.daily.deal <- NULL

    d <- t(apply(dF, 1, function(daily.deal) {
        x <- daily.deal[["x"]]
        y <- daily.deal[["y"]]
        v <- daily.deal[["v"]]
        xma <- daily.deal[["xma"]]
        yma <- daily.deal[["yma"]]
        vma <- daily.deal[["vma"]]
        low <- daily.deal[["lp"]]
        med <- daily.deal[["mp"]]
        high <- daily.deal[["hp"]]
        xc <- ifelse(!is.null(last.daily.deal), last.daily.deal["xc"], 0)
        yc <- ifelse(!is.null(last.daily.deal), last.daily.deal["yc"], 0)
        value <- ifelse(xc > 0, xc * x, yc * y)
        value <- ifelse(value > 0, value, ifelse(!is.null(last.daily.deal), last.daily.deal["value"], 10000))

        if (x >= xma) {
            if (yc > 0 && v < low && v > vma) {
            # sell y, buy x
                yc <- 0
                xc <- value * 0.99 / x
            }

            if (xc == 0 && yc == 0 && v < med) {
            #buy x
                xc <- value * 0.995 / x
            }
        }
        else if (y < yma) {
            xc <- 0
        }

        if (y >= yma) {
            if (xc > 0 && v > high && v < vma) {
            # sell x, buy y
                xc <- 0
                yc <- value * 0.99 / y
            }

            if (xc == 0 && yc == 0 && v > med) {
            #buy y
                yc <- value * 0.995 / y
            }
        }
        else if (x < xma) {
            yc <- 0
        }

        last.daily.deal <<- c(daily.deal, xc = xc, yc = yc, value = value)
        return(c(xc = xc, yc = yc, value = value))
    }))

    rownames(d) <- NULL
    dF <- cbind(dF, d)

    df.nrow <- nrow(dF)
    ldf <- dF[df.nrow,]
    ydf <- dF[df.nrow - 240,]

    return(list(df = dF, tr = c(ma = ma.period, v.ma.period = v.ma.period, vl = vl, w = w, lp = lp, hp = hp, total.return = ldf$value / 10000, year.return = ldf$value / as.numeric(ydf$value))))
}

x <- stocks.whole790[, c("Date", "Close")]
y <- stocks.whole150[, c("Date", "Close")]
tr <- NULL

#for (m in seq(39, 40)) {
    #print(m)
    #for (l in seq(0, 0.5, 0.02)) {
        #for (h in seq(0.5, 1, 0.02)) {
            #tr <- rbind(tr, backtest(x, y, ma.period = m, w=30, vl = 15, lp = l, hp = h)$tr)
        #}
    #}
#}

#for (l in seq(0, 0.5, 0.02)) {
    #for (h in seq(0.5, 1, 0.02)) {
        #tr <- rbind(tr, backtest(x, y, ma.period = 39, v.ma.period = 9, w = 30, vl = 5, lp = l, hp = h)$tr)
    #}
#}

r <- backtest(x, y, ma.period = 39, v.ma.period = 9, w = 30, vl = 5, lp = 0.04, hp = 0.60)

tr <- r$tr
df <- r$df
