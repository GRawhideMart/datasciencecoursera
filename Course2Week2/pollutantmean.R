nameconstructor <- function(name) {
  if(name < 10) {
    paste('00',name,sep='')
  } else if(name < 100) {
    paste('0',name,sep='')
  } else {
    paste(name, sep='')
  }
}

pollutantmean <- function(directory = getwd(), pollutant, id = 1:332) {
  meanNA <- 0
  obsC <- 0
  weightedMean <- 0
  for (i in id) {
    data <- read.csv(paste(nameconstructor(i),'.csv',sep=''))
    if (pollutant == 'sulfate') {
      notNA <- data$sulfate[!is.na(data$sulfate)]
      meanNA <- meanNA + length(notNA)*mean(notNA)
      obsC <- obsC + length(notNA)
      weightedMean <- meanNA / obsC
    }  else if (pollutant == 'nitrate') {
      notNA <- data$nitrate[!is.na(data$nitrate)]
      meanNA <- meanNA + length(notNA)*mean(notNA)
      obsC <- obsC + length(notNA)
      weightedMean <- meanNA / obsC
    }  
  }
  weightedMean
}