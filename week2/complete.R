complete <- function(directory, id = 1:332) {
        result <- data.frame()
        old.dir <- getwd()
        setwd(directory)
        for (i in id){
                data <- read.csv(list.files()[i])
                goodData <- rep(TRUE, dim(data)[1])
                for (colmn in names(data)){
                        goodData <- goodData & !is.na(data[colmn])
                }
                l <- length(goodData[goodData])
                tempDataFrame <- data.frame(id = i, nobs = l)
                result <- rbind(result, tempDataFrame)
        }
        setwd(old.dir)
        result
}