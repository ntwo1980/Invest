source("utils.R")

pb.priority <- c("801780", "801790")

core.data <- function(x) {
    data = x[, c("Code", "Name", "Date", "Close", "CM", "PE", "PEM", "PB", "PBM", "ROE", "Cost", "Ret", "CP", "PEP", "PBP", "VP")]

    return(data)
}

load.stocks.data <- function(file) {
    data <- read.table(file, header = T, sep = ",", as.is = T, fileEncoding = "utf-8",
          colClasses = c("factor", "character", "Date", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "NULL", "NULL", "NULL", "NULL"),
          col.names = c("Code", "Name", "Date", "Open", "High", "Low", "Close", "Volumn", "Amount", "Change", "Turnover", "PE", "PB", "Price", "AmountPercentage", "HQLTSZ", "AHQLTSZ", "Payout"))
    data <- sqldf("select Date, Code, Name, Open, Close, High, Low, Volumn, PE, PB, Price from data order by Date")
    percentiles <- apply(data[, c("Close", "PE", "PB", "Volumn")], 2, get.percentile, percentile.length = 240, only.last = T)

    data <- transform(data,
                    CM = median(get.data.year(Close, 1), na.rm = T),
                    PEM = median(get.data.year(PE, 1), na.rm = T),
                    PBM = median(get.data.year(PB, 1), na.rm = T),
                    ROE = round(PB / PE, 2),
                    Cost = round(Price * PE / PB, 2),
                    Ret = c(NA, round(diff(Close) / Close[ - length(Close)], 4) * 100),
                    CP = percentiles[, 1],
                    PEP = percentiles[, 2],
                    PBP = percentiles[, 3],
                    VP = percentiles[, 4])

    return(data)
}

swFolder <- paste(getwd(), "/sw/", sep = "")
files <- list.files(swFolder, pattern = "\\.csv$")

last.day.data <- NULL
monthly.stat <- NULL

for (file in files) {
    cat("processing ", file, "\r\n", sep = "")
    name.postfix <- substr(file, 4, 6)
    stocks.whole.name <- paste("stocks.whole", name.postfix, sep = "")
    stocks.name <- paste("stocks", name.postfix, sep = "")
    stocks.whole <- load.stocks.data(paste(swFolder, file, sep = ""))
    assign(stocks.whole.name, stocks.whole, pos = .GlobalEnv)
    stocks <- core.data(get.data.year(stocks.whole, 1))
    assign(stocks.name, stocks, pos = .GlobalEnv)
    stocks.whole.xts <- xts(stocks.whole$Close, stocks.whole$Date)
    stocks.whole.monthly.returns <- monthlyReturn(stocks.whole.xts)
    stocks.whole.monthly.returns <- transform(stocks.whole.monthly.returns,
                                    Month = month(index(stocks.whole.monthly.returns)))
    colnames(stocks.whole.monthly.returns)[1] <- "Returns"
    stocks.whole.monthly.returns.UpRatio <- tapply(stocks.whole.monthly.returns$Returns,
         stocks.whole.monthly.returns$Month,
         function(x) { sum(x > 0) / length(x) * 100 })
     stocks.whole.monthly.returns.MeanReturns <- tapply(stocks.whole.monthly.returns$Returns,
         stocks.whole.monthly.returns$Month,
         function(x) { sum(x) / length(x) * 100})

    stocks.whole.monthly.returns.stat <- as.data.frame(list(UpRatio = stocks.whole.monthly.returns.UpRatio, MeanReturns = stocks.whole.monthly.returns.MeanReturns))
    stocks.whole.monthly.returns.stat <- transform(stocks.whole.monthly.returns.stat, 
        Code = stocks$Code[1],
        Name = stocks$Name[2],
        Month = index(stocks.whole.monthly.returns.stat),
        Length = floor( nrow(stocks.whole) / 240))

    monthly.stat <- rbind(monthly.stat, stocks.whole.monthly.returns.stat)

    row.count = nrow(stocks)
    last.row = stocks[row.count,]
    last.row <- transform(last.row,
                            ROEI = stocks$ROE[row.count] / stocks$ROE[ifelse(row.count - 240 > 0, row.count - 240, 1)],
                            Range = (range(stocks$Close, na.rm = T)[2] - range(stocks$Close, na.rm = T)[1]) / mean(stocks$Close),
                            Down = min(stocks$Close[(row.count - 4):row.count]) < min(stocks$Close[(row.count - 9):(row.count - 5)]),
                            Up = max(stocks$Close[(row.count - 4):row.count]) > max(stocks$Close[(row.count - 9):(row.count - 5)]),
                            UMA10 = (Close - mean(tail(stocks$Close, 10))) / Close * 100,
                            C = tail(rle(sign(tail(stocks$Ret, 10)))$length, 1),
                            UR = sum(tail(stocks$Ret, 10) > 0))

    last.day.data <- rbind(last.day.data, last.row)
}

last.day.data <- transform(last.day.data,
                           PEBP = ifelse(is.element(as.character(Code), pb.priority), PBP * 0.6 + PEP * 0.4, PBP * 0.4 + PEP * 0.6))

last.day.data <- sqldf("select * from [last.day.data] order by PEBP")
top25.value <- quantile(last.day.data$Cost)[4]
last.day <- subset(last.day.data, Cost <= top25.value & !Down)

print(last.day)

#hist(stocks813$PE, 20, xlab = "813", ylab = "PE", main = "")
#abline(v = tail(stocks210$PE, 1), col = "blue")

# sqldf("select * from [monthly.stat] where UpRatio > 70 and Length > 4 order by MeanReturns desc")
# sqldf("select * from [monthly.stat] where Month > 10 and Length > 4 order by MeanReturns desc")

