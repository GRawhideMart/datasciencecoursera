nameconstructor <- function(name) {
  if(name < 10) {
    paste('00',name,sep='')
  } else if(name < 100) {
    paste('0',name,sep='')
  } else {
    paste(name, sep='')
  }
}

pollutantmean <- function(directory, pollutant, id = 1:332) {
  meanNA <- 0
  obsC <- 0
  weightedMean <- 0
  for (i in id) {
    data <- read.csv(paste(nameconstructor(i),'.csv',sep=''))
    if (pollutant == 'sulfate') {
      if (length(data$sulfate[!is.na(data$sulfate)]) == 0) {
        next
      }
      notNA <- data$sulfate[!is.na(data$sulfate)]
      meanNA <- meanNA + length(notNA)*mean(notNA, na.rm = TRUE)
      obsC <- obsC + length(notNA)
      weightedMean <- meanNA / obsC
    }  else if (pollutant == 'nitrate') {
      if (length(data$nitrate[!is.na(data$nitrate)]) == 0) {
        next
      }
      notNA <- data$nitrate[!is.na(data$nitrate)]
      meanNA <- meanNA + length(notNA)*mean(notNA, na.rm = TRUE)
      obsC <- obsC + length(notNA)
      weightedMean <- meanNA / obsC
    }  
  }
  weightedMean
}