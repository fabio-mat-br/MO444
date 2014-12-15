# LOAD PACKAGES ################################################################
library("Hmisc")
library("randomForest")

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

splitSeq <- function(seq, seed=NULL){
  if (!is.null(seed)) set.seed(seed)
  index <- 1:length(seq)
  
  trainindex <- sample(index, trunc(length(index)*.8))
  
  trainset <- seq[trainindex ]
  
  subindex <- 1:length(trainset)
  validationindex <- sample(subindex, trunc(length(subindex)*.8))
  
  validationset <- seq[-validationindex ]
  trainset <- trainset[validationindex ]
  
  
  testset <- seq[-trainindex]
  list(train=trainset, test=testset, validation=validationset)
}
# LOAD DATA ####################################################################
data <- NULL
data <- read.csv("data/data.csv", sep=",")

# CLEAN DATA ###################################################################
ids <- unique(data$id)
# SPLIT DATA ###################################################################
split <- splitSeq(ids)

train_id <- split$train
train_data <- subset(data , id %in% train_id)

for(i in 1:length(train_id)){
  cur_data <- subset(train_data, id == train_id[i]);
  if(dim(cur_data)[1] < 4){
    train_data <- subset(train_data , id != train_id[i])
  } else {
    
  }
}

# ONE BLOCK #################
t <- subset(train_data , id == 9)
odv <- NULL
ultrasounds <- NULL
ultrasounds <- t$t_ultsnd
for(i in 2:dim(t)[1]){
  odv <- c(odv, t$odv2[i])
  odv <- c(odv, t$odv3[i])
  odv <- c(odv, t$odv4[i])
  odv <- c(odv, t$odv5[i])
  odv <- c(odv, t$odv6[i])
  odv <- c(odv, t$odv7[i])
  odv <- c(odv, t$odv8[i])
  print(i)
}

#usDiff <- odv[length(odv)] - odv[1]
timeDiff <- ultrasounds[length(ultrasounds)] - ultrasounds[2]

usDiff <- t$odv5[length(ultrasounds)] - t$odv5[2]

slope <- NULL
slope <- c(slope, t$odv2[length(ultrasounds)] - t$odv2[2] / timeDiff)
slope <- c(slope, t$odv3[length(ultrasounds)] - t$odv3[2] / timeDiff)
slope <- c(slope, t$odv4[length(ultrasounds)] - t$odv4[2] / timeDiff)
slope <- c(slope, t$odv5[length(ultrasounds)] - t$odv5[2] / timeDiff)
slope <- c(slope, t$odv6[length(ultrasounds)] - t$odv6[2] / timeDiff)
slope <- c(slope, t$odv7[length(ultrasounds)] - t$odv7[2] / timeDiff)
slope <- c(slope, t$odv8[length(ultrasounds)] - t$odv8[2] / timeDiff)

# ONE BLOCK (END) #############

inverseS <- matrix(c(
   3554.42, -328.119,
  -328.119, 133.511
  ), ncol=2)

#a   <-NULL
#a$b <- 1