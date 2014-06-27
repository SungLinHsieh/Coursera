corr <- function(directory, threshold = 0) {
    cr <- numeric()
    table <- complete(directory)
    table <- table[table["nobs"]>threshold,1]
    for (i in table){
        path <- paste(directory , "/",formatC(i,width = 3 ,flag=0),".csv",sep="")
        tmp <- read.csv(path)
        cr[length(cr)+1] <- cor(tmp[,2],tmp[,3],use="complete.obs")
    }
    cr
}