# fish school model, standard runs routine
# Alberto Rovellini, ZMT Bremen, 10/06/2015
# alberto.rovellini@zmt-bremen.de

setwd("C:/Users/Alberto/Documents/swarm_runs/F2_Std_s/c40/resources")
library(plyr)
data <- read.csv("params.csv", header=T, sep='\t', dec='.') # reading function 
data[,8] <- as.numeric(gsub(",","", data[,8])) # eliminates the comma as thousand separator
data <- data[c(2,4,6,7,8)]

# exploratory, will be gone

#attach(data)
#lev <- levels(factor(as.character(numFish)))



stdError <- function(x) sqrt(var(x)/length(x)) # function defining standard error
stdErrorColumn<-function(data) apply(data[,2:ncol(data)], 2, stdError) # function to apply stdError to columns

meanPerNumberOfFish <- ddply(data, .(numFish), colMeans) # apply mean factorized by numFish
stdErrorPerNumberOfFish <- ddply(data, .(numFish), stdErrorColumn) # apply stdErrorColumn factorized by numFish
meanAndStdError <- merge(meanPerNumberOfFish, stdErrorPerNumberOfFish, by="numFish")

# single cell size, script for one scenario





        


