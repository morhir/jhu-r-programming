best <- function(state, outcome) {
        ## Read outcome data
        data <- read.csv("outcome-of-care-measures.csv", 
                            colClasses = "character")
        ## Check that state and outcome are valid
        if (is.na(match(state, data$State))){
                stop("invalid state")
        }
        if (outcome == "heart attack"){
                n <- 11
        }  
        else if (outcome == "heart failure"){
                n <- 17
        }
        else if (outcome == "pneumonia"){
                n <- 23
        }
        else {
                stop("invalid outcome")
        }
        data[,n] <- as.numeric(data[,n])
        stateData <- split(data, data$State)$state
        ## Return hospital name in that state with lowest 30-day death
        ## rate
        
        
        return(head(stateData[,n]))
}