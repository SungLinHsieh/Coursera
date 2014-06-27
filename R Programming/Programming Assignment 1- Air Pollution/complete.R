complete <- function(directory, id = 1:332) {
    data = numeric()
    table <- data.frame(id=integer(), nobs=integer())
    for (i in id){
        path <- paste(directory , "/",formatC(i,width = 3 ,flag=0),".csv",sep="")
        tmp <- read.csv(path)
        index <- logical()
        for ( j in 1:nrow(tmp)){
            index[j] = all(!is.na(tmp[j,]))
        }
        table[nrow(table)+1,] <- c( i, length(index[index == TRUE]) )
    }
    table
}