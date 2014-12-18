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
train_dataframe <- data.frame(NULL)
train_id <- split$train
train_data <- subset(data , id %in% train_id)

for(i in 1:length(train_id)){
  cur_data <- subset(train_data, id == train_id[i]);
  if(dim(cur_data)[1] < 4){
    train_data <- subset(train_data , id != train_id[i])
  } else {
    
  }
}

nd <- data.frame(data$birth_sz, data$duration)
plot (nd)

for(cur_id in 1:length(train_id)){
t <- subset(train_data , id == train_data$id[cur_id])
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
}

timeDiff <- ultrasounds[length(ultrasounds)] - ultrasounds[2]

usDiff <- NULL
usDiff <- t$odv5[length(ultrasounds)] - t$odv5[2]

slope <- NULL
slope$odv1 <- t$odv1[1]
slope$odv2 <- t$odv2[length(ultrasounds)] - t$odv2[2] / timeDiff
slope$odv3 <- t$odv3[length(ultrasounds)] - t$odv3[2] / timeDiff
slope$odv4 <- t$odv4[length(ultrasounds)] - t$odv4[2] / timeDiff
slope$odv5 <- t$odv5[length(ultrasounds)] - t$odv5[2] / timeDiff
slope$odv6 <- t$odv6[length(ultrasounds)] - t$odv6[2] / timeDiff
slope$odv7 <- t$odv7[length(ultrasounds)] - t$odv7[2] / timeDiff
slope$odv8 <- t$odv8[length(ultrasounds)] - t$odv8[2] / timeDiff

newEntry <- data.frame(t$id[1], timeDiff, t$sex[1], t$status[1], slope, t$birth_sz[1], t$duration[1])
rbind(train_dataframe, newEntry)->train_dataframe
}

# NORMALIZE #############################

#mean <- NULL
#mean <- sum(slope)/length(slope)
#memsum <- 0
#for(i in 1:length(slope)){
#  memsum <- memsum + ((slope[i] - mean)^2)
#  print(sqrt(memsum / (length(slope) - 1)))
#}
#std <- sqrt(memsum / (length(slope) - 1))
# ############################################3
# ONE BLOCK (END) #############

#inverseS <- matrix(c(
#   3554.42, -328.119,
#  -328.119, 133.511
#  ), ncol=2)
