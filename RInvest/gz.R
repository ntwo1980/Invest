library("stringr")

load.stocks.data <- function(file) {
    data <- read.table(file, header = T, sep = ",", stringsAsFactors = F, fileEncoding = "utf-8",
          colClasses = c("factor", "character", "Date", "numeric", "numeric", "numeric"),
          col.names = c("Code", "Name", "Date", "CompanyCount", "SPE", "RPE"))
    data <- sqldf("select Date, Code, Name, CompanyCount, SPE, RPE from data order by Date")
    percentiles <- apply(data[, c("SPE", "RPE")], 2, get.percentile, percentile.length = 240, only.last = T)

    data <- transform(data,
                    SPEM = median(get.data.year(SPE, 1), na.rm = T),
                    RPEM = median(get.data.year(RPE, 1), na.rm = T),
                    SPEP = percentiles[, 1],
                    RPEP = percentiles[, 2])

    return(data)
}

gzFolder <- paste(getwd(), "/gz/", sep = "")
files <- list.files(gzFolder, pattern = "\\.csv$")

last.day.data <- NULL

for (file in files) {
    #cat("processing ", file, "\r\n", sep = "")
    name.postfix <- substr(file, 2, str_locate(file, "\\.") - 1)
    stocks.whole.name <- paste("stocks.whole", name.postfix, sep = "")
    stocks.name <- paste("stocks", name.postfix, sep = "")
    stocks <- load.stocks.data(paste(gzFolder, file, sep = ""))
    assign(stocks.name, stocks, pos = .GlobalEnv)

    row.count = nrow(stocks)
    last.row = stocks[row.count,]

    last.day.data <- rbind(last.day.data, last.row)
}


last.day.data <- sqldf("select * from [last.day.data] order by RPEP")
print(last.day.data)
