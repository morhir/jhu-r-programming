corr <- function(directory, threshold = 0) {
        old.dir <- getwd()
        files <- complete(directory)
        setwd(directory)
        goodFiles <- files["id"][files["nobs"] > threshold]
        result <- numeric(0)
        for (i in goodFiles){
                data <- read.csv(list.files()[i])
                goodData <- rep(TRUE, dim(data)[1])
                for (colmn in names(data)){
                        goodData <- goodData & !is.na(data[colmn])
                }
                correl <- cor(data[names(data)[2]][goodData], data[names(data)[3]][goodData])
                result <- c(result, correl)
                
        }
        setwd(old.dir)
        result
}