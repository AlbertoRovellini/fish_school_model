# script for the analysis of the output of the swarm model, Standard scenario.
# 

library(plyr)
library(reshape)
library(ggplot2)
setwd("C:/Users/Alberto/Documents/swarm_runs/F2_Std_i") 
listOfFiles <- list.files("C:/Users/Alberto/Documents/swarm_runs/F2_Std_i", 
                          pattern="*.csv", recursive=TRUE) # write list with all the filenames of the output

nOfCells<- as.numeric(gsub("[^0-9]", "", listOfFiles)) # vector with the number of cells extracted as numeric from the filenames
cellsOrdered <- sort(nOfCells) # order the number of cells ascendingly
inds <- sort.int(nOfCells, index.return=TRUE)[[2]] # extract index of the nOfCells in ascending order
listOfFiles<- listOfFiles[order(listOfFiles)[inds]] # reorders the list on ascending number of cells
cellSize <- c(500,250,125,62.500,31.250,15.625) # vector with the dimensions of the cells

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

fishOnFoodAll <- data.frame(cellSize, temp, temp2)
fishOnFoodAll <- fishOnFoodAll[with(fishOnFoodAll, order(cellSize)), ]
colnames(fishOnFoodAll)<- c("Cellsize", "f20", "f30", "f40", "f50", "f60", "se20", "se30", "se40", "se50", "se60")
meltFishOnFoodAll <- melt(fishOnFoodAll, id.vars="Cellsize")

# mapping of the limits for the errorbar plot, each mean +- its se. one entry per class is necessary
limitsa<-aes(ymax=fishOnFoodAll$f20+fishOnFoodAll$se20, ymin=fishOnFoodAll$f20-fishOnFoodAll$se20)
limitsb<-aes(ymax=fishOnFoodAll$f30+fishOnFoodAll$se30, ymin=fishOnFoodAll$f30-fishOnFoodAll$se30)
limitsc<-aes(ymax=fishOnFoodAll$f40+fishOnFoodAll$se40, ymin=fishOnFoodAll$f40-fishOnFoodAll$se40)
limitsd<-aes(ymax=fishOnFoodAll$f50+fishOnFoodAll$se50, ymin=fishOnFoodAll$f50-fishOnFoodAll$se50)
limitse<-aes(ymax=fishOnFoodAll$f60+fishOnFoodAll$se60, ymin=fishOnFoodAll$f60-fishOnFoodAll$se60)
pd <- position_dodge(0.5) # set the dodge span for the errorbars in ggplot

# plotter
plotFishOnFoodInd <-ggplot(subset(meltFishOnFoodAll, variable=="f20" | variable=="f30" |
                                          variable== "f40" | variable== "f50" | variable== "f60"),
                           aes(x=Cellsize, y=value, fill=variable))+
        geom_line(aes(linetype=variable), size=0.2)+
        scale_linetype_manual(values=c("solid", "dashed", "dotted", "dotdash", "longdash"))+
        geom_point(aes(shape=variable, fill=variable), size=3)+
        #coord_trans(x="log2")+
        scale_shape_manual(values=c(25,22,23,24,21))+
        scale_fill_manual(values=rep("darkgrey",5))+
        scale_x_log10("Cell size", breaks=cellSize)+
        scale_y_continuous("Fish on food", breaks=seq(0,5, by=0.5), 
                           limits=c(0,5), expand=c(0,0))+
        geom_errorbar(limitsa, data=subset(meltFishOnFoodAll, variable=="f20"), width=0.02, position='dodge')+
        geom_errorbar(limitsb, data=subset(meltFishOnFoodAll, variable=="f30"), width=0.02, position='dodge')+
        geom_errorbar(limitsc, data=subset(meltFishOnFoodAll, variable=="f40"), width=0.02, position='dodge')+
        geom_errorbar(limitsd, data=subset(meltFishOnFoodAll, variable=="f50"), width=0.02, position='dodge')+
        geom_errorbar(limitse, data=subset(meltFishOnFoodAll, variable=="f60"), width=0.02, position='dodge')+        
        theme_bw()+
        theme(panel.grid.major.x = element_blank())+
        theme(axis.title.x = element_text(size=12,vjust=-0.3),
              axis.title.y = element_text(size=12,vjust=1))+
        theme(legend.title = element_text(size=12))+
        theme(axis.text.x=element_text(size=10))+
        theme(axis.text.y=element_text(size=10))

plotFishOnFoodInd # aesthetics missing

write.table(fishOnFoodAll, "C:/Users/Alberto/Documents/swarm_runs/new_output_Std/fishOnFoodIndividual_reverted.csv") # writes .csv output 
#ggsave("Second plot individual.pdf", plotFishOnFoodInd, useDingbats=FALSE)


