init.environment <- function() {
    setwd("C:/Users/nn1003/Documents/Invest/RInvest")
    library("sqldf")
}

stat <- function(x, percentile.length, only.last = TRUE) {
    percentiles <- c()
    x.start <- ifelse(only.last == TRUE, length(x), 1)

    for (i in x.start:length(x)) {
        if (i < percentile.length) {
            percentiles[i] <- NA
        } else {
            current <- x[i]
            start <- i + 1 - percentile.length
            end <- i
            percentile.func <- ecdf(x[start:end])
            current.percentile <- round(percentile.func(current), 2)
            percentiles[i] <- current.percentile
        }
    }

    return(percentiles)
}

core.data <- function(x) {
    data = x[, c("Code", "Name", "Date", "Close", "PE", "PB", "ROE", "Returns", "CP", "PEP", "PBP", "VP")]

    return(data)
}

load.stocks.data <- function(file) {
    data <- read.table(file, header = T, sep = ",", stringsAsFactors = F, fileEncoding = "utf-8",
          colClasses = c("factor", "character", "Date", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "NULL", "NULL", "NULL", "NULL", "NULL"),
          col.names = c("Code", "Name", "Date", "Open", "High", "Low", "Close", "Volumn", "Amount", "Change", "Turnover", "PE", "PB", "Average", "AmountPercentage", "HQLTSZ", "AHQLTSZ", "Payout"))
    data <- sqldf("select Date, Code, Name, Open, Close, High, Low, Volumn, PE, PB from data order by Date")
    percentiles <- apply(data[, c("Close", "PE", "PB", "Volumn")], 2, stat, percentile.length = 240, only.last = TRUE)

    data <- transform(data,
                    ROE = PE / PB,
                    Returns = c(NA, round(diff(Close) / Close[ - length(Close)], 4) * 100),
                    CP = percentiles[, 1],
                    PEP = percentiles[, 2],
                    PBP = percentiles[, 3],
                    VP = percentiles[, 4]
                    )

    return(data)
}

get.data.year <- function(data, year) {
    end <- nrow(data)
    start <- end + 1 - year * 240

    return(data[start:end,])
}

init.environment()

files <- c("801005.csv",
           "801120.csv",
           "801150.csv",
           "801160.csv",
           "801780.csv",
           "801790.csv",
           "801823.csv",
           "801853.csv",
           "801813.csv"
           )

last.day.data <- NULL

for (file in files) {
    name.postfix <- substr(file, 5, 6)
    stocks.whole.name <- paste("stocks.whole", name.postfix, sep = "")
    stocks.name <- paste("stocks", name.postfix, sep = "")
    stocks.whole <- load.stocks.data(file)
    assign(stocks.whole.name, stocks.whole, pos=.GlobalEnv )
    stocks <- core.data(get.data.year(stocks.whole, 1))
    assign(stocks.name, stocks, pos=.GlobalEnv )

    row.count = nrow(stocks)
    last.row = stocks[row.count,]
    last.row <- cbind(last.row, SDC = sd(stocks$Close) / mean(stocks$Close),
        Down = min(stocks$Close[(row.count - 4):row.count]) < min(stocks$Close[(row.count - 9):(row.count - 5)]),
        Up = max(stocks$Close[(row.count - 4):row.count]) > max(stocks$Close[(row.count - 9):(row.count - 5)]) )

    last.day.data <- rbind(last.day.data, last.row)
}

last.day.data <- transform(last.day.data,
                           OP = PEP * 0.6 + PBP * 0.4)

last.day.data <- sqldf("select * from [last.day.data] order by OP")
