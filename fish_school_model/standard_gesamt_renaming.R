# script for the analysis of the output of the swarm model, Standard scenario.
# computes the mean and the standard error of the replicates of the same scenario.
# plots mean and se of Polarization, Food patches, Fish on food and School extent for a given number of fish.
# Also plots mean and se of the fish on food (as fish on food/food patches) for all the numbers of fish.
# input .csv files need to be in the pointed working directory or in its subdirectories.

library(plyr)
library(reshape)
library(ggplot2)
setwd("C:/Users/Alberto/Documents/swarm_runs/F2_Std_s") 
listOfFiles <- list.files("C:/Users/Alberto/Documents/swarm_runs/F2_Std_s", 
                         pattern="*.csv", recursive=TRUE) # write list with all the filenames of the output

nOfCells<- as.numeric(gsub("[^0-9]", "", listOfFiles)) # vector with the number of cells extracted as numeric from the filenames
cellsOrdered <- sort(nOfCells) # order the number of cells ascendingly
inds <- sort.int(nOfCells, index.return=TRUE)[[2]] # extract index of the nOfCells in ascending order
listOfFiles<- listOfFiles[order(listOfFiles)[inds]] # reorders the list on ascending number of cells

read.special<-function(x) { # file reader
        read.csv(x, header=T, sep='\t', dec='.') 
}
allData <- lapply(listOfFiles, read.special) # reads all the output files

# built-in function to drop the comma as thousand separator and drop the undesired columns from the dataframes
dataReducer <- function(data) { 
        data[,8] <- as.numeric(gsub(",","", data[,8])) # drop the comma as thousand separator
        data <- data[c(2,4,6,7,8)] # extract the desired columns
}

shortData <- lapply(allData, dataReducer) # reduces dataframes

# averages the variables factorizing per number of fish, calculates the standard error and returns a data frame
mainRoutine <- function(data) { 
        stdError <- function(x) sqrt(var(x)/length(x)) # function defining standard error
        stdErrorColumn<-function(data) apply(data[,2:ncol(data)], 2, stdError) # function to apply stdError to columns
        meanPerNumberOfFish <- ddply(data, .(numFish), colMeans) # apply mean factorized by numFish
        stdErrorPerNumberOfFish <- ddply(data, .(numFish), stdErrorColumn) # apply stdErrorColumn factorized by numFish
        meanAndStdError <- merge(meanPerNumberOfFish, stdErrorPerNumberOfFish, by="numFish")
}

allDataFrames <- lapply(shortData, mainRoutine) # applies the routine to all the dataframes

wantedFishNumber=30 # to be changed to the desired numFish
relevantFish <- lapply(allDataFrames, function(x) subset(x, x$numFish==wantedFishNumber))
relevantFish <- do.call("rbind", relevantFish)
relevantFish$nOfCells <- cellsOrdered # adds a column with the number of cells

colnames(relevantFish) <- c("Numberoffish", "Polarization", "Foodpatch", "Fishonfood", "Schoolextent",
                            "pol_se", "fp_se", "fof_se", "se_se", "Grid")
meltFish <- melt(relevantFish, id.vars="Grid",variable.name= "variable", 
              value.name="value") # melt the dataset according to grid, for ggplot

# mapping of the limits for the errorbar plot, each mean +- its se. one entry per class is necessary
limits1<-aes(ymax=relevantFish$Polarization+relevantFish$pol_se, ymin=relevantFish$Polarization-relevantFish$pol_se)
limits2<-aes(ymax=relevantFish$Foodpatch+relevantFish$fp_se, ymin=relevantFish$Foodpatch-relevantFish$fp_se)
limits3<-aes(ymax=relevantFish$Fishonfood+relevantFish$fof_se, ymin=relevantFish$Fishonfood-relevantFish$fof_se)
limits4<-aes(ymax=relevantFish$Schoolextent+relevantFish$se_se, ymin=relevantFish$Schoolextent-relevantFish$se_se)
pd <- position_dodge(0.5) # set the dodge span for the errorbars in ggplot

# plotter for the desired variables (mean+sd) for a certain number of fish across all the number of cells
plotNumberFish <-ggplot(subset(meltFish, variable=="Polarization" | variable=="Foodpatch" |
                              variable== "Fishonfood" | variable== "Schoolextent"),
               aes(x=Grid, y=value, group=variable))+
        geom_line(aes(linetype=variable), size=0.2)+
        scale_linetype_manual(values=c("solid", "dashed", "dotted", "dotdash"))+
        geom_point(aes(shape=variable, fill=variable), size=3)+
        scale_shape_manual(values=c(0,5,6,2))+
        scale_x_log10("Grid cells", breaks=cellsOrdered, expand=c(0,0))+
        scale_y_continuous(breaks=seq(0,200, by=10), 
                           limits=c(0,200), expand=c(0,0))+
        geom_errorbar(limits1, data=subset(meltFish, variable=="Polarization"), width=0.05)+
        geom_errorbar(limits2, data=subset(meltFish, variable=="Foodpatch"), width=0.05, position=pd)+
        geom_errorbar(limits3, data=subset(meltFish, variable=="Fishonfood"), width=0.05)+
        geom_errorbar(limits4, data=subset(meltFish, variable=="Schoolextent"), width=0.05, position=pd)+
        theme_bw()
        
plotNumberFish # dodging fails, aesthetics need to be reviewed

fishOnFoodFrames <- lapply(allDataFrames, function(data) data[,c(1,3,4,7,8)]) # keeps fish on food and food cells

# loop region to manipulate the list of dataframes
cellList <- list()
for (i in 1:length(cellsOrdered)) { # create a list of vectors, each a replicate of the number of cells
        cellList[[i]] <- rep(cellsOrdered[i], nrow(fishOnFoodFrames[[i]]))
}
for (k in 1:length(fishOnFoodFrames)) { # merges cellList to the data frames
        fishOnFoodFrames[[k]]$Cells <- cellList[[k]]
}
for (k in 1:length(fishOnFoodFrames)) { # renames the columns of the data frames
        colnames(fishOnFoodFrames[[k]])<- c("Numberoffish", "Foodpatch", "Fishonfood", "fp_se", "fof_se", "Grid")
}
for (k in 1:length(fishOnFoodFrames)) { # divides fish on food by number of cells (standardization)
        fishOnFoodFrames[[k]]$fishOnFoodStd <- fishOnFoodFrames[[k]][,3]/fishOnFoodFrames[[k]][,2]
}
for (k in 1:length(fishOnFoodFrames)) { # calculates the standard error of the new variable (according to error propagation rules)
        fishOnFoodFrames[[k]]$fishOnFoodStd_se <- sqrt((fishOnFoodFrames[[k]][,5]/fishOnFoodFrames[[k]][,3])^2+
                                                        (fishOnFoodFrames[[k]][,4]/fishOnFoodFrames[[k]][,2])^2)
}


# data reorganization and preparation for the plot
mergedFrames <-do.call("rbind", fishOnFoodFrames) # merges all the dataframes of the list fishOnFoodFrames
splitMergedFrames <- split(mergedFrames, mergedFrames$Numberoffish)
temp <- as.data.frame(matrix(nrow=nrow(splitMergedFrames[[1]]), ncol=length(levels(as.factor(mergedFrames[,1])))))
for (i in 1:length(splitMergedFrames)) {
       temp[,i] <- splitMergedFrames[[i]][,length(splitMergedFrames[[i]])-1] 
}
temp2 <- as.data.frame(matrix(nrow=nrow(splitMergedFrames[[1]]), ncol=length(levels(as.factor(mergedFrames[,1])))))
for (i in 1:length(splitMergedFrames)) {
        temp2[,i] <- splitMergedFrames[[i]][,length(splitMergedFrames[[i]])] 
}

fishOnFoodAll <- data.frame(cellsOrdered, temp, temp2)
colnames(fishOnFoodAll)<- c("Grid", "f20", "f30", "f40", "f50", "f60", "se20", "se30", "se40", "se50", "se60")
meltFishOnFoodAll <- melt(fishOnFoodAll, id.vars="Grid")

# mapping of the limits for the errorbar plot, each mean +- its se. one entry per class is necessary
limitsa<-aes(ymax=fishOnFoodAll$f20+fishOnFoodAll$se20, ymin=fishOnFoodAll$f20-fishOnFoodAll$se20)
limitsb<-aes(ymax=fishOnFoodAll$f30+fishOnFoodAll$se30, ymin=fishOnFoodAll$f30-fishOnFoodAll$se30)
limitsc<-aes(ymax=fishOnFoodAll$f40+fishOnFoodAll$se40, ymin=fishOnFoodAll$f40-fishOnFoodAll$se40)
limitsd<-aes(ymax=fishOnFoodAll$f50+fishOnFoodAll$se50, ymin=fishOnFoodAll$f50-fishOnFoodAll$se50)
limitse<-aes(ymax=fishOnFoodAll$f60+fishOnFoodAll$se60, ymin=fishOnFoodAll$f60-fishOnFoodAll$se60)


plotFishOnFoodAll <-ggplot(subset(meltFishOnFoodAll, variable=="f20" | variable=="f30" |
                                variable== "f40" | variable== "f50" | variable== "f60"),
                 aes(x=Grid, y=value, group=variable))+
        geom_line(aes(linetype=variable), size=0.2)+
        scale_linetype_manual(values=c("solid", "dashed", "dotted", "dotdash", "longdash"))+
        geom_point(aes(shape=variable, fill=variable), size=3)+
        #coord_trans(x="log2")+
        scale_shape_manual(values=c(0,5,6,2,3))+
        scale_x_log10("Grid cells", breaks=cellsOrdered, expand=c(0,0))+
        scale_y_continuous("", breaks=seq(0,5, by=0.5), 
                           limits=c(0,5), expand=c(0,0))+
        geom_errorbar(limitsa, data=subset(meltFishOnFoodAll, variable=="f20"), width=0.05)+
        geom_errorbar(limitsb, data=subset(meltFishOnFoodAll, variable=="f30"), width=0.05, position=pd)+
        geom_errorbar(limitsc, data=subset(meltFishOnFoodAll, variable=="f40"), width=0.05)+
        geom_errorbar(limitsd, data=subset(meltFishOnFoodAll, variable=="f50"), width=0.05, position=pd)+
        geom_errorbar(limitse, data=subset(meltFishOnFoodAll, variable=="f60"), width=0.05)+        
        theme_bw()

plotFishOnFoodAll # aesthetics missing
      