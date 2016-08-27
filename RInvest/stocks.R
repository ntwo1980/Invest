source("utils.R")
load.stocks.data <- function(file) {
    data <- read.table(file, header = T, sep = ",", as.is = T, fileEncoding = "utf-8",
          colClasses = c("factor", "character", "Date", "numeric", "numeric", "numeric"),
          col.names = c("Code", "Name", "Date", "PE", "DPE", "PB"))
    data <- sqldf("select Date, Code, Name, PE, DPE, PB from data order by Date")
    percentiles <- apply(data[, c("PE", "DPE", "PB")], 2, get.percentile, percentile.length = 240, only.last = T)

    data <- transform(data,
                    PEM = median(get.data.year(PE, 1), na.rm = T),
                    DEPM = median(get.data.year(DPE, 1), na.rm = T),
                    PBM = median(get.data.year(PB, 1), na.rm = T),
                    ROE = round(PB / PE, 2),
                    PEP = percentiles[, 1],
                    DPEP = percentiles[, 2],
                    PBP = percentiles[, 3])

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
pb.df <- NULL

for (file in files) {
    cat("processing ", file, "\r\n", sep = "")

    name.postfix <- substr(file, 1, 6)
    stocks.whole.name <- paste("stocks.whole", name.postfix, sep = "")
    stocks.whole <- load.stocks.data(paste(folder, file, sep = ""))
    assign(stocks.whole.name, stocks.whole, pos = .GlobalEnv)

    if (i == 1) {
        pb.df <- sqldf(paste("SELECT Date FROM [stocks.whole]", sep = ""))
    }

    pb.df <- sqldf(paste("SELECT df1.*, df2.PB AS s", name.postfix, " FROM [pb.df] df1 JOIN [stocks.whole] df2 USING(Date)", sep = ""))

    i <- i + 1
}

rownames(pb.df) <- pb.df[[1]]
pb.df[[1]] <- NULL
pb.xts = as.xts(pb.df)

#stocks813.copy <- stocks813
#rownames(stocks813.copy) <- stocks813.copy$Date
#stocks813.copy$Date <- NULL
#stocks813.copy$Code <- NULL
#stocks813.copy$Name <- NULL
#stocks813.xts <- as.xts(stocks813.copy)
#stocks811.copy <- stocks811
#rownames(stocks811.copy) <- stocks811.copy$Date
#stocks811.copy$Date <- NULL
#stocks811.copy$Code <- NULL
#stocks811.copy$Name <- NULL
#stocks811.xts <- as.xts(stocks811.copy)


x <- pb.xts[, 7]
y <- pb.xts[, 8]

#x <- stocks813.xts$PE
#y <- stocks811.xts$PE

#window_length <- 10

dF <- cbind(x, y)
names(dF) <- c("x", "y")

run_regression <- function(dF) {
    return (coef(lm(y ~x -1 ,data = as.data.frame(dF))))
}

rolling_beta <- function(z, width) {
    rollapply(z, width = width, FUN = run_regression, by.column = FALSE, align = "right")
}

betas <- rolling_beta(diff(dF), 10)

data <- merge(betas, dF)
data$spread <- data$y - lag(betas, 1) * data$x

returns <- diff(dF) / dF
return_beta <- rolling_beta(returns, 10)
data$spreadR <-diff(data$y) / data$y - return_beta * diff(data$x) / data$x

threshold <- sd(data$spread, na.rm = T)


