best <- function(state, outcome) {
  # READ THE OUTCOME -> I'm interested in cols 11, 17, 23
  dat = read.csv('outcome-of-care-measures.csv', colClasses = 'character')
  disease <- if(outcome == 'heart attack') {
    11
  } else if(outcome == 'heart failure') {
    17
  } else if(outcome == 'pneumonia') {
    23
  }
  # VALIDATION
  possibleOutcomes <- c('heart attack', 'heart failure', 'pneumonia')
  if(!(outcome %in% possibleOutcomes)) stop('invalid outcome')
  if(!(state %in% dat$State)) stop('invalid state')
  
  # MAIN LOGIC OF THE SNIPPET
  else {
    dataByState <- subset(data, State == state)
    colOfInterest <- as.numeric(dataByState[,disease])
    cleanData <- dataByState[!is.na(colOfInterest), ]
    consideredCol <- as.numeric(cleanData[,disease])
    selectedRows <- which(consideredCol == min(consideredCol))
    selectedHospitals <- cleanData[selectedRows,2]
    
    if (length(selectedHospitals) == 1) {
      selectedHospitals
    } else {
      sort(selectedHospitals)
    }
  }
}