init.environment <- function() {
    setwd("C:/Users/nn1003/Documents/Invest/RInvest")
    options(digits = 2)
    library("sqldf")
    library("lubridate")
    library("quantmod")
    library("sqldf")
}

init.environment()

source("utils.R")
#source("sw.R")
#source("gz.R")
source("stocks.R")