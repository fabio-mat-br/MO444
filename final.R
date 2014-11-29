# FUNCTIONS ####################################################################
## PLOT DATA - PLOT THE RAW DATA IN AN UNIQUE IMAGE ############################
plotData <- function(){
  png(filename = "plot-data-1000.png" , width = 1000, height = 1000, units = "px")
  plot(data)
  dev.off()
}

## SPLIT DATA IN TRAIN, TEST AND VALIDATION ####################################
splitData <- function(dataframe, seed=NULL) {
  if (!is.null(seed)) set.seed(seed)
  index <- 1:nrow(dataframe)
  
  trainindex <- sample(index, trunc(length(index)*.8))
  
  trainset <- dataframe[trainindex, ]
  
  subindex <- 1:nrow(trainset)
  validationindex <- sample(subindex, trunc(length(subindex)*.8))
  
  validationset <- dataframe[-validationindex, ]
  trainset <- trainset[validationindex, ]
  
  
  testset <- dataframe[-trainindex, ]
  list(train=trainset, test=testset, validation=validationset)
}

# LOAD DATA ####################################################################
data <- read.csv("data/data.csv", sep=",")

split <- splitData(data)