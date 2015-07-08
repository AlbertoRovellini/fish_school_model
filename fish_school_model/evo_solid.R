library(plyr)
library(reshape)
library(ggplot2)
library(abind)
library(scatterplot3d)
setwd("C:/Users/Alberto/Documents/swarm_runs/E3_nn") 
listOfFiles <- list.files("C:/Users/Alberto/Documents/swarm_runs/E3_nn", 
                          pattern="*.csv", recursive=TRUE) # write list with all the filenames of the output


# function to skim over the list of F_Results files and read only the desired lines. can be improved a lot.
# for each file, once the lines are read they are bound into a data frame and renamed. function returns
# one dataframe per each run with one line per desired timestep
selectiveReader <- function(data) {
        
        connectionFile <- file(data)
        open(connectionFile)
        
        run <- read.table(connectionFile,skip=66600, nrow=1, header=FALSE) # 66601-st line
        
        close(connectionFile)
        colnames(run) <- c("Time","Polarization","Speed","Activecells","Fishonfood","Expanse","Swarmspeed",
                           "Probswarmbehavior[0.0-0.2]","Probswarmbehavior[0.2-0.4]","Probswarmbehavior[0.4-0.6]",
                           "Probswarmbehavior[0.6-0.8]","Probswarmbehavior[0.8-1.0]")
        return(run)
}

allData <- lapply(listOfFiles, selectiveReader)

# drops the unwanted columns from all the obtained data frames. calculates the 50% probability as
# sum of the last two columns and half of the 0.4-0.6 column. returns a dataframe with only 2 columns,
# one with time and one with the new calculated probability.

dataReducer <- function (x) {
        x <- x[,c(1,8:12)]
}

allTimeCululativeProb <- lapply(allData, dataReducer)

# split the long list containing all the runs into sublists, each of 100 runs. each sublist contains
# the replicates of one number of grid cells, from 40 to 1280.

cells40 <- allTimeCululativeProb[1:100]
cells80 <- allTimeCululativeProb[101:200]
cells160 <- allTimeCululativeProb[201:300]
cells320 <- allTimeCululativeProb[301:400]
cells640 <- allTimeCululativeProb[401:500]
cells1280 <- allTimeCululativeProb[501:600]

stdError <- function(x) sqrt(var(x)/length(x)) # function defining standard error

mean40 <- as.data.frame(t(apply((abind(cells40, along=3)), c(1,2), mean)))[-1,1, drop=FALSE]
se40 <- as.data.frame(apply((abind(cells40, along=3)), c(1,2), stdError))

mean80 <- as.data.frame(t(apply((abind(cells80, along=3)), c(1,2), mean)))[-1,1, drop=FALSE]
se80 <- as.data.frame(apply((abind(cells80, along=3)), c(1,2), stdError))

mean160 <- as.data.frame(t(apply((abind(cells160, along=3)), c(1,2), mean)))[-1,1, drop=FALSE]
se160 <- as.data.frame(apply((abind(cells160, along=3)), c(1,2), stdError))

mean320 <- as.data.frame(t(apply((abind(cells320, along=3)), c(1,2), mean)))[-1,1, drop=FALSE]
se320 <- as.data.frame(apply((abind(cells320, along=3)), c(1,2), stdError))

mean640 <- as.data.frame(t(apply((abind(cells640, along=3)), c(1,2), mean)))[-1,1, drop=FALSE]
se640 <- as.data.frame(apply((abind(cells640, along=3)), c(1,2), stdError))

mean1280 <- as.data.frame(t(apply((abind(cells1280, along=3)), c(1,2), mean)))[-1,1, drop=FALSE]
se1280 <- as.data.frame(apply((abind(cells1280, along=3)), c(1,2), stdError))

meanComplete <- rbind(mean40, mean80, mean160, mean320, mean640, mean1280)
cellsVector <- c(rep(1,5), rep(2,5), rep(3,5), rep(4,5), rep(5,5), rep(6,5))
rangeVector <- rep(c(1:5),6)
meanComplete$cells <- cellsVector
meanComplete$range <- rangeVector
colnames(meanComplete) <- c("schoolingprob", "gridsize", "schoolingprobrange")
meanCompleteL <- split(meanComplete, gridsize)
#meanComplete
#attach(meanComplete)
scat3d <- scatterplot3d(meanCompleteL[[1]][,3], meanCompleteL[[1]][,2], meanCompleteL[[1]][,1],
                        color="grey", pch=19, xlim=c(1:5), ylim=c(1:6), zlim=c(0:60),      
                        type="p", box=TRUE)
scat3d$points3d(meanCompleteL[[1]][,3], meanCompleteL[[1]][,2], meanCompleteL[[1]][,1],
                col = "darkgrey", lty=1, type="l")

scat3d$points3d(meanCompleteL[[2]][,3], meanCompleteL[[2]][,2], meanCompleteL[[2]][,1],
             col = "darkgrey", pch = 18, lty=2, type="p")
scat3d$points3d(meanCompleteL[[2]][,3], meanCompleteL[[2]][,2], meanCompleteL[[2]][,1],
                col = "darkgrey", lty=2, type="l")

scat3d$points3d(meanCompleteL[[3]][,3], meanCompleteL[[3]][,2], meanCompleteL[[3]][,1],
                col = "black", pch = 17, type="p")
scat3d$points3d(meanCompleteL[[3]][,3], meanCompleteL[[3]][,2], meanCompleteL[[3]][,1],
                col = "black", lty=3, type="l")

scat3d$points3d(meanCompleteL[[4]][,3], meanCompleteL[[4]][,2], meanCompleteL[[4]][,1],
                col = "grey", pch = 16, type="p")
scat3d$points3d(meanCompleteL[[4]][,3], meanCompleteL[[4]][,2], meanCompleteL[[4]][,1],
                col = "grey", lty=4, type="l")

scat3d$points3d(meanCompleteL[[5]][,3], meanCompleteL[[5]][,2], meanCompleteL[[5]][,1],
                col = "darkgrey", pch = 15, type="p")
scat3d$points3d(meanCompleteL[[5]][,3], meanCompleteL[[5]][,2], meanCompleteL[[5]][,1],
                col = "darkgrey", lty=5, type="l")

scat3d$points3d(meanCompleteL[[6]][,3], meanCompleteL[[6]][,2], meanCompleteL[[6]][,1],
                col = "black", pch = 14, type="p")
scat3d$points3d(meanCompleteL[[6]][,3], meanCompleteL[[6]][,2], meanCompleteL[[6]][,1],
                col = "black", lty=6, type="l")

write.table(meanComplete, "3dplot.csv")
