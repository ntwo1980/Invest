get.percentile <- function(x, percentile.length, only.last = TRUE) {
    percentiles <- c()
    x.start <- ifelse(only.last == T, length(x), 1)

    for (i in x.start:length(x)) {
        if (i < percentile.length) {
            percentiles[i] <- NA
        }
        else {
            current <- x[i]
            start <- i + 1 - percentile.length
            if (start < 1) {
                start <- 1
            }
            end <- i
            data <- x[start:end]
            if (any(!is.na(data))) {
                percentile.func <- ecdf(data)
                current.percentile <- percentile.func(current)
                percentiles[i] <- round(current.percentile, 2)
            }
            else {
                percentiles[i] <- NA
            }
        }
    }

    return(percentiles)
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

