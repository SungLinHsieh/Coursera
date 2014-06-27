rankall <- function(outcome, num = "best") {
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv",colClasses = "character")
    ## Check that outcome is valid
    outcome_list<- c("heart attack","heart failure","pneumonia")
    if (!(outcome %in% outcome_list)){
        stop("invalid outcome")
    }
    preindex <- match(outcome,outcome_list)
    index <- 5+6*preindex
    state_list <- sort(unique(data["State"])[,1])
    data[, index] <- suppressWarnings(as.numeric(data[, index]))
    data <- data[,c(2,7,index)]
    data <- data[!is.na(data[,3]),]
    word <- c("best","worst")
    ## For each state, find the hospital of the given rank
    result <- data.frame(hospital=numeric(),state=numeric())
    for (state in state_list){
        tmp <- data[data["State"]==state,]
        tmp <- tmp[order(tmp[,3],tmp[,1]),]
        rank <- num
        if (rank %in% word){
            rank <- (match(rank,word)-1)*(length(tmp[,2])-1)+1
        }
        else if (rank > nrow(tmp)){
            result[nrow(result)+1,] <- c(NA,state)
            next
        }
        row <- nrow(result)+1
        result[row,1]=tmp[rank,1]
        result[row,2]=tmp[rank,2]
    }
    row.names(result) <- result[,2]
    return(result)
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
}