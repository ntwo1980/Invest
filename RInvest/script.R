init.environment <- function() {
    setwd("C:/Users/nn1003/Documents/Invest/RInvest")
    options(digits = 4)
    library("sqldf")
    library("lubridate")
    library("quantmod")
    library("sqldf")
    library("xts")
}

init.environment()

source("utils.R")
#source("sw.R")
source("pair.R")
#source("dt.R")
##source("zz.R")
#source("gz.R")
#source("stocks.R")