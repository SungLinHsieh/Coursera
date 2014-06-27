pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  data = numeric()
  myFiles <- list.files(directory,pattern=".csv")
  for (i in id){
    #path = paste( , "/",formatC(i,width = 3 ,flag=0),".csv",sep="")
    tmp <- read.csv(paste(directory,"/",myFiles[i],sep=""))
    data = c( data , tmp[!is.na(tmp[pollutant]),pollutant])
  }
  round(mean(data),3)
}