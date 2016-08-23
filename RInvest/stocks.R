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

folder <- paste(getwd(), "/zz/", sep = "")
files <- list.files(folder, pattern = "\\.csv$")

i <- 1
pe.df <- NULL

for (file in files) {
    cat("processing ", file, "\r\n", sep = "")

    name.postfix <- substr(file, 1, 6)
    stocks.whole.name <- paste("stocks.whole", name.postfix, sep = "")
    stocks.whole <- load.stocks.data(paste(folder, file, sep = ""))
    assign(stocks.whole.name, stocks.whole, pos = .GlobalEnv)

    if (i == 1) {
        pe.df <- sqldf(paste("SELECT Date FROM [stocks.whole]", sep=""))
    }

    pe.df <- sqldf(paste("SELECT df1.*, df2.PE AS s", name.postfix , " FROM [pe.df] df1 JOIN [stocks.whole] df2 USING(Date)", sep=""))

    i <- i + 1
}

#pe.df <- data.frame(pe.df, stocks.whole$Date)



