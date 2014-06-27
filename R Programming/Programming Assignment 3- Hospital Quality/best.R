best <- function(state, outcome) {
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv",colClass = "character")
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
    ## Return hospital name in that state with lowest 30-day death
    ## rate
    data[, index] <- as.numeric(data[, index])
    best <- data[data["State"]==state,c(2,index)]
    best <- best[!is.na(best[,2]),]
    best <- best[best[,2]==min(best[,2]),]
    return(sort(best[,1])[1])
}