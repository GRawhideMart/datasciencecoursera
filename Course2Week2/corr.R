corr <- function(directory = getwd(), threshold = 0) {
  files <- list.files()
  correlation <- vector()
  for (file in files) {
    data <- read.csv(file)
    isComplete <- complete.cases(data)
    completecases <- length(data$ID[isComplete])
    if (completecases >= threshold) {
      correlation <- c(correlation, cor(data$sulfate[isComplete],data$nitrate[isComplete]))
    }
  }
  correlation
}