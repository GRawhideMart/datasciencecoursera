rankhospital <- function(state, outcome, num = "best") {
  
  data <- read.csv('outcome-of-care-measures.csv', colClasses = 'character')
  disease <- if(outcome == 'heart attack') {
    11
  } else if(outcome == 'heart failure') {
    17
  } else if(outcome == 'pneumonia') {
    23
  }
  # VALIDATION
  possibleOutcomes <- c('heart attack', 'heart failure', 'pneumonia')
  if (!(outcome %in% possibleOutcomes)) stop('invalid outcome')
  if (!(state %in% data$State)) stop('invalid state')
  if (as.numeric(num) == TRUE & (num > length(data[,2]))) return(NA)
  
  #Get new data with selected state
  dataState <- subset(data, data$State == state)
  #Cleaning and formatting data
  dataState[,disease] <- as.numeric(dataState[,disease])
  NACols <- is.na(dataState[, disease])
  cleanData <- dataState[!NACols, ]
  #Sort clean data by ascending order of outcome
  outcomeColName <- names(cleanData)[disease]
  hospitalColName <- names(cleanData)[2]
  index <- with(cleanData, order(cleanData[outcomeColName], cleanData[hospitalColName]))
  sortedData <- cleanData[index, ]
  
  #Handling 'best's and 'worst's
  if (is.character(num) == TRUE) {
    if (num == 'best') num <-1
    if (num == 'worst') num <- length(sortedData[, disease])
  }
  #Return the hospital with outcome of num sorted by name
  sortedData[num,2]
  
}