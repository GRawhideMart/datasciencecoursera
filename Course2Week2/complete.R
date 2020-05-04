nameconstructor <- function(name) {
  if(name < 10) {
    paste('00',name,sep='')
  } else if(name < 100) {
    paste('0',name,sep='')
  } else {
    paste(name, sep='')
  }
}

complete <- function(directory = getwd(), id = 1:332) {
  Id <- vector()
  nobs <- vector()
  for (i in id) {
    data <- read.csv(paste(nameconstructor(i),'.csv',sep=''))
    Nobs <- length(data$ID[!is.na(data$sulfate) & !is.na(data$nitrate)])
    Id <- c(Id, i)
    nobs <- c(nobs, Nobs)
  }
  data.frame(Id,nobs)
}