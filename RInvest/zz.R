source("utils.R")
#source("sw.R")

load.stocks.data <- function(file) {
    data <- read.table(file, header = T, sep = ",", as.is = T, fileEncoding = "utf-8",
          colClasses = c("factor", "Date", "numeric", "numeric", "numeric"),
          col.names = c("Code", "Date", "PE", "RPE", "PB"))
    data <- sqldf("select Date, Code, PE, RPE, PB from data order by Date")

    return(data)
}

calculate_spread <- function(x, y, beta) {
    return(y - beta * x)
}

calculate_beta_and_level <- function(x, y) {
    dx <- diff(x)
    dy <- diff(y)
    r <- prcomp( ~ dx + dy)

    beta <- r$rotation[2, 1] / r$rotation[1, 1]
    spread <- calculate_spread(x, y, beta)
    names(spread) <- "spread"
    level <- mean(spread, na.rm = T)

    outL <- list()
    outL$spread <- spread
    outL$beta <- beta
    outL$level <- level

    return(outL)
}


calculate_buy_sell_signals <- function(spread, beta, level, lower_threshold, upper_threshold) {
    buy_signals <- ifelse(spread <= level - lower_threshold, 1, 0)
    sell_signals <- ifelse(spread >= level + upper_threshold, 1, 0)

    output <- cbind(spread, buy_signals, sell_signals)
    colnames(output) <- c("spread", "buy_signals", "sell_signals")

    return(output)
}

folder <- paste(getwd(), "/zz/", sep = "")
files <- list.files(folder, pattern = "\\.csv$")

i <- 1
pe.df <- NULL
pb.df <- NULL

for (file in files) {
    cat("processing ", file, "\r\n", sep = "")

    name.postfix <- substr(file, 1, 6)
    stocks.whole.name <- paste("stocks.whole", name.postfix, sep = "")
    stocks.whole <- load.stocks.data(paste(folder, file, sep = ""))
    assign(stocks.whole.name, stocks.whole, pos = .GlobalEnv)

    if (i == 1) {
        pe.df <- sqldf(paste("SELECT Date FROM [stocks.whole]", sep = ""))
        pb.df <- sqldf(paste("SELECT Date FROM [stocks.whole]", sep = ""))
    }

    pe.df <- sqldf(paste("SELECT df1.*, df2.PE AS s", name.postfix, " FROM [pe.df] df1 JOIN [stocks.whole] df2 USING(Date)", sep = ""))
    pb.df <- sqldf(paste("SELECT df1.*, df2.PB AS s", name.postfix, " FROM [pe.df] df1 JOIN [stocks.whole] df2 USING(Date)", sep = ""))

    i <- i + 1
}

bank.pe <- merge(pe.df, stocks790[, c("Date", "PE")])
bank.pe.stat <- apply(bank.pe[, 2:17], 2, function(x) x / bank.pe$PE)
percentiles.pe <- apply(bank.pe.stat, 2, get.percentile, percentile.length = 240, only.last = T)

bank.pb <- merge(pe.df, stocks790[, c("Date", "PB")])
bank.pb.stat <- apply(bank.pb[, 2:17], 2, function(x) x / bank.pb$PB)
percentiles.pb <- apply(bank.pb.stat, 2, get.percentile, percentile.length = 240, only.last = T)

bank.percentiles <- tail(percentiles.pe, 1)
bank.percentiles <- rbind(bank.percentiles, tail(percentiles.pb, 1))
bank.percentiles <- rbind(bank.percentiles, tail(percentiles.pb + percentiles.pe, 1))

#x <- bank.pb[, c("Date", "s600030")]
#y <- bank.pb[, c("Date", "PB")]
#x <- xts(x[, -1], order.by = x[, 1])
#y <- xts(y[, -1], order.by = y[, 1])

x <- stocks811[, c("Date", "PB")]
y <- stocks813[, c("Date", "PB")]
x <- xts(x[, -1], order.by = x[, 1])
y <- xts(y[, -1], order.by = y[, 1])

dF <- cbind(x, y)
names(dF) <- c("x", "y")

run_regression <- function(dF) {
    return(coef(lm(y ~ x - 1, data = as.data.frame(dF))))
}

rolling_beta <- function(z, width) {
    rollapply(z, width = width, FUN = run_regression,
    by.column = FALSE, align = "right")
}
betas <- rolling_beta(diff(dF), 10)
data <- merge(betas, dF)
data$spread <- data$y - lag(betas, 1) * data$x

threshold <- sd(data$spread, na.rm = T)

plot(data$spread, main = "╢Сел vs. п║ел",
    cex.main = 0.8,
    cex.lab = 0.8,
    cex.axis = 0.8)
abline(h = 1, lty = 2)
abline(h = -2, lty = 2)

# Generate sell and buy signals
buys <- ifelse(data$spread > 1, 1, 0)
sells <- ifelse(data$spread < -2, -1, 0)
data$signal <- buys + sells

point_type <- rep(NA, nrow(data))
buy_index <- which(data$signal == 1)
sell_index <- which(data$signal == -1)
point_type[buy_index] <- 21
point_type[sell_index] <- 24
points(data$spread, pch = point_type)

prev_x_qty <- 0
position <- 0
trade_size <- 100
signal <- as.numeric(data$signal)
signal[is.na(signal)] <- 0
beta <- as.numeric(data$beta_out_of_sample)
qty_x <- rep(0, length(signal))
qty_y <- rep(0, length(signal))
for (i in 1:length(signal)) {
    if (signal[i] == 1 && position == 0) {
        # buy the spread
        prev_x_qty <- round(beta[i] * trade_size)
        qty_x[i] <- -prev_x_qty
        qty_y[i] <- trade_size
        position <- 1
    }
    if (signal[i] == -1 && position == 0) {
        # sell the spread initially
        prev_x_qty <- round(beta[i] * trade_size)
        qty_x[i] <- prev_x_qty
        qty_y[i] <- -trade_size
        position <- -1
    }
    if (signal[i] == 1 && position == -1) {
        # we are short the spread and need to buy
        qty_x[i] <- -(round(beta[i] * trade_size) +
        prev_x_qty)
        prev_x_qty <- round(beta[i] * trade_size)
        qty_y[i] <- 2 * trade_size
        position <- 1
    }
    if (signal[i] == -1 && position == 1) {
        # we are long the spread and need to sell
        qty_x[i] <- round(beta[i] * trade_size) + prev_x_qty
        prev_x_qty <- round(beta[i] * trade_size)
        qty_y[i] <- -2 * trade_size
        position <- -1
    }
}

qty_x[length(qty_x)] <- -sum(qty_x)
qty_y[length(qty_y)] <- -sum(qty_y)

data$qty_x <- qty_x
data$qty_y <- qty_y
#results <- calculate_beta_and_level(x, y)
