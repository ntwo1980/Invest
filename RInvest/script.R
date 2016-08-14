pb.priority <- c("801780", "801790")

init.environment <- function() {
    setwd("C:/Users/nn1003/Documents/Invest/RInvest")
    options(digits = 2)
    library("sqldf")
}

get.percentile <- function(x, percentile.length, only.last = TRUE) {
    percentiles <- c()
    x.start <- ifelse(only.last == T, length(x), 1)

    for (i in x.start:length(x)) {
        if (i < percentile.length) {
            percentiles[i] <- NA
        } else {
            current <- x[i]
            start <- i + 1 - percentile.length
            if (start < 1) {
                start <- 1
            }
            end <- i
            percentile.func <- ecdf(x[start:end])
            current.percentile <- percentile.func(current)
            percentiles[i] <- round(current.percentile, 2)
        }
    }

    return(percentiles)
}

core.data <- function(x) {
    data = x[, c("Code", "Name", "Date", "Close", "PE", "PEM", "PB", "PBM", "ROE", "Ret", "CP", "PEP", "PBP", "VP")]

    return(data)
}

load.stocks.data <- function(file) {
    data <- read.table(file, header = T, sep = ",", stringsAsFactors = F, fileEncoding = "utf-8",
          colClasses = c("factor", "character", "Date", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "NULL", "NULL", "NULL", "NULL", "NULL"),
          col.names = c("Code", "Name", "Date", "Open", "High", "Low", "Close", "Volumn", "Amount", "Change", "Turnover", "PE", "PB", "Average", "AmountPercentage", "HQLTSZ", "AHQLTSZ", "Payout"))
    data <- sqldf("select Date, Code, Name, Open, Close, High, Low, Volumn, PE, PB from data order by Date")
    percentiles <- apply(data[, c("Close", "PE", "PB", "Volumn")], 2, get.percentile, percentile.length = 240, only.last = T)

    data <- transform(data,
                    PEM = median(get.data.year(PE, 1), na.rm = T),
                    PBM = median(get.data.year(PB, 1), na.rm = T),
                    ROE = round(PE / PB, 2),
                    Ret = c(NA, round(diff(Close) / Close[ - length(Close)], 4) * 100),
                    CP = percentiles[, 1],
                    PEP = percentiles[, 2],
                    PBP = percentiles[, 3],
                    VP = percentiles[, 4])

    return(data)
}

get.data.year <- function(data, year) {
    if (class(data) == "data.frame") {
        end <- nrow(data)
        start <- end + 1 - year * 240
        if (start < 1) {
            start <- 1
        }

        return(data[start:end,])
    }
    else {
        end <- length(data)
        start <- end + 1 - year * 240
        if (start < 1)
            start <- 1

        return(data[start:end])
    }

}

init.environment()

swFolder <- paste(getwd(), "/sw/", sep = "")
files <- list.files(swFolder, pattern = "\\.csv$")

last.day.data <- NULL

for (file in files) {
    cat("processing ", file, "\r\n", sep = "")
    name.postfix <- substr(file, 5, 6)
    stocks.whole.name <- paste("stocks.whole", name.postfix, sep = "")
    stocks.name <- paste("stocks", name.postfix, sep = "")
    stocks.whole <- load.stocks.data(paste(swFolder, file, sep=""))
    assign(stocks.whole.name, stocks.whole, pos = .GlobalEnv)
    stocks <- core.data(get.data.year(stocks.whole, 1))
    assign(stocks.name, stocks, pos = .GlobalEnv)

    row.count = nrow(stocks)
    last.row = stocks[row.count,]
    last.row <- transform(last.row,
                            Range = (range(stocks$Close, na.rm = T)[2] - range(stocks$Close, na.rm = T)[1]) / mean(stocks$Close),
                            Down = min(stocks$Close[(row.count - 4):row.count]) < min(stocks$Close[(row.count - 9):(row.count - 5)]),
                            Up = max(stocks$Close[(row.count - 4):row.count]) > max(stocks$Close[(row.count - 9):(row.count - 5)]),
                            C = tail(rle(sign(tail(stocks$Ret, 10)))$length, 1),
                            UD = sum(tail(stocks$Ret, 10) > 0))

    last.day.data <- rbind(last.day.data, last.row)
}

last.day.data <- transform(last.day.data,
                           PEBP = ifelse(is.element(as.character(Code), pb.priority), PBP * 0.6 + PEP * 0.4, PBP * 0.4 + PEP * 0.6))

last.day.data <- sqldf("select * from [last.day.data] order by PEBP")
print(last.day.data)
