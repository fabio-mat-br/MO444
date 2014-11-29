# LOAD DATA ####################################################################
data <- read.csv("data/data.csv", sep=",")
# FUNCTIONS ####################################################################
plotData <- function(){
  png(filename = "plot-data-1000.png" , width = 1000, height = 1000, units = "px")
  plot(data)
  dev.off()
}