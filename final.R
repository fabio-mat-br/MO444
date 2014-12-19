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

test_id <- split$train
test_data <- subset(data , id %in% test_id)

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
colnames(train_dataframe) <- c("id","t_ultsnd",  "sex",	"status",	"odv1",	"odv2",	"odv3",	"odv4",	"odv5",	"odv6",	"odv7",	"odv8",	"birth_sz",	"duration")

inverseS <- matrix(c(
   3554.42, -328.119,
  -328.119, 133.511
  ), ncol=2)
model <- lm(train_dataframe$birth_sz ~ train_dataframe$odv1 +
                  train_dataframe$odv2 +
                  train_dataframe$odv3 +
                  train_dataframe$odv4 +
                  train_dataframe$odv5 +
                  train_dataframe$odv6 +
                  train_dataframe$odv7 +
                  train_dataframe$odv8)
fit_duration <- lm(train_dataframe$duration ~ train_dataframe$odv1 +
                  train_dataframe$odv2 +
                  train_dataframe$odv3 +
                  train_dataframe$odv4 +
                  train_dataframe$odv5 +
                  train_dataframe$odv6 +
                  train_dataframe$odv7 +
                  train_dataframe$odv8)

model
summary(model)
anova(model)
plot(model)
res <- residuals(model)

plot(jitter(res) ~ jitter(train_dataframe$birth_sz), ylab="Residuals", xlab="Birth Sz")
abline(0,0)
abline(model)
library(splines)
plot(model)
#general summary
summary(model)
#Visualize some diagnostics
plot(model)
#Coefficient values
coef(model)
#Confidence intervals
confint(model)
#predict values
predict(model)
#predict new values
predict(model, newdata = data.frame(y = 1:620))
#Residuals
resid(model)
#Standardized residuals
rstandard(model)
#Studentized residuals
rstudent(model)
#AIC
AIC(model)
#BIC
BIC(model)
#Cook's distance
cooks.distance(model)
#DFFITS
dffits(model)
#lots of measures related to model fit
influence.measures(model)

require(graphics)
D2 <- mahalanobis(nd, FALSE, inverseS)
plot(density(D2, bw = 0.5),
     main="Squared Mahalanobis distances, n=100, p=3") ; 
rug(D2)
qqplot(qchisq(ppoints(100), df = 3), D2,
       main = expression("Q-Q plot of Mahalanobis" * ~D^2 *
                           " vs. quantiles of" * ~ chi[3]^2))
abline(0, 1, col = 'gray')

SS.total      <- with(train_dataframe,sum((birth_sz-mean(birth_sz))^2))
SS.residual   <- sum(residuals(model)^2)
SS.regression <- sum((fitted(model)-mean(train_dataframe$birth_sz))^2)
SS.total - (SS.regression+SS.residual)
# [1] 1.907349e-06
SS.regression/SS.total     # fraction of variation explained by the model
# [1] 0.08965502
1-SS.residual/SS.total     # same thing, for model frame ONLY!!! 
# [1] 0.08965502          
summary(model)$r.squared     # both are = R.squared
# [1] 0.08965502

test_dataframe <- data.frame(NULL)
newEntry <- NULL
for(cur_id in 1:length(test_id)){
  t <- subset(test_data, id == test_data$id[cur_id])
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
  rbind(test_dataframe, newEntry)->test_dataframe
}
colnames(test_dataframe) <- c("id","t_ultsnd",	"sex",	"status",	"odv1",	"odv2",	"odv3",	"odv4",	"odv5",	"odv6",	"odv7",	"odv8",	"birth_sz",	"duration")
View(test_dataframe)
predict(model, newdata = test_dataframe)

plot(test_dataframe[,"birth_sz"], predict(model, newdata = test_dataframe),
     xlim=c(0.2,1), ylim=c(0.4,1))
abline(0,1, col="red")


# RANDOM FOREST ###############################################################################

model_m <- randomForest(data$birth_sz~., 
                      data = data, 
                      importance=TRUE,
                      keep.forest=TRUE
)
print(model_m)
plot(model_m)
varImpPlot(model_m, type=1)

predicted <- predict(model_m, newdata=test_dataframe)
View(test_dataframe)
