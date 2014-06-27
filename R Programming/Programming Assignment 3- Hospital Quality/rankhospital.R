rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv",colClasses = "character")
    ## Check that state and outcome are valid
    if (length(data[data["State"]==state,1])==0){
        stop("invalid state")
    }
    outcome_list<- c("heart attack","heart failure","pneumonia")
    if (!(outcome %in% outcome_list)){
        stop("invalid outcome")
    }
    preindex <- match(outcome,outcome_list)
    index <- 5+6*preindex
    data[, index] <- suppressWarnings(as.numeric(data[, index]))
    hospital <- data[data["State"]==state,c(2,index)]
    hospital <- hospital[!is.na(hospital[,2]),]
    word <- c("best","worst")
    if (num %in% word){
        num <- (match(num,word)-1)*(length(hospital[,2])-1)+1
    }
    else if (num > length(hospital[,2])){
        return(NA)
    }
    ## Return hospital name in that state with the given rank
    ## 30-day death rate
    hospital <- hospital[order(hospital[,2],hospital[,1]),1]
    return(hospital[num])
}