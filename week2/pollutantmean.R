pollutantmean <- function(directory, pollutant, id = 1:332) {
        m <- 0
        l <- 0
        s <- 0
        old.dir <- getwd()
        setwd(directory)
        for (i in id) {
                data <- read.csv(list.files()[i])
                isna <- is.na(data[pollutant])
                curData <- data[pollutant][!isna]
                m <- m + mean(curData)
                s <- s + sum(curData)
                l <- l + length(curData)
        }
        setwd(old.dir)
        result <- s/l
        result
}