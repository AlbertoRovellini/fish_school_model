# will need to merge rowise, possibly rename, point at two directories and so on.

library(plyr)
library(reshape)
library(ggplot2)
setwd("C:/Users/Alberto/Documents/swarm_runs/T2_Std_i") 
listOfFiles <- list.files("C:/Users/Alberto/Documents/swarm_runs/T2_Std_i", 
                          pattern="*.csv", recursive=TRUE) # write list with all the filenames of the output

nCA<- as.numeric(gsub("[^0-9]", "", listOfFiles)) # vector with the number of cells extracted as numeric from the filenames
CAOrdered <- sort(nCA) # order the number of cells ascendingly
inds <- sort.int(nCA, index.return=TRUE)[[2]] # extract index of the nOfCells in ascending order
listOfFiles<- listOfFiles[order(listOfFiles)[inds]] # reorders the list on ascending number of cells

read.special<-function(x) { # file reader
        read.csv(x, header=T, sep='\t', dec='.') 
}
allData <- lapply(listOfFiles, read.special) # reads all the output files

# built-in function to drop the comma as thousand separator and drop the undesired columns from the dataframes
dataReducer <- function(data) { 
        data[,8] <- as.numeric(gsub(",","", data[,8])) # drop the comma as thousand separator
        data[,2] <- as.numeric(gsub(",","", data[,2]))
        data[,2] <- as.numeric(data[,2])
        data <- data[c(2,6,7)] # extract the desired columns
}

allData <- lapply(allData, dataReducer) # reduces dataframes

# second block

setwd("C:/Users/Alberto/Documents/swarm_runs/T2_Std_ia6") 
listOfFiles1 <- list.files("C:/Users/Alberto/Documents/swarm_runs/T2_Std_ia6", 
                           pattern="*.csv", recursive=TRUE) # write list with all the filenames of the output

nCA1<- as.numeric(gsub("[^0-9]", "", listOfFiles1)) # vector with the number of cells extracted as numeric from the filenames
CAOrdered1 <- sort(nCA1) # order the number of cells ascendingly
inds1 <- sort.int(nCA, index.return=TRUE)[[2]] # extract index of the nOfCells in ascending order
listOfFiles1<- listOfFiles1[order(listOfFiles1)[inds1]] # reorders the list on ascending number of cells

allData1 <- lapply(listOfFiles1, read.special) # reads all the output files
allData1 <- lapply(allData1, dataReducer) # reduces dataframes

# merging

complete <- list()
for (i in 1:length(allData)) {
        complete[[i]] <- rbind(allData[[i]], allData1[[i]])
}

subset.custom <- function(data) {
        subset(data, data$CellGridSize %in% (c(40, 80, 160, 320, 640, 1280)))
}

complete <- lapply(complete, subset.custom) # reduced to the actual number of cells, cool job

# routine
mainRoutine <- function(data) { 
        stdError <- function(x) sqrt(var(x)/length(x)) # function defining standard error
        stdErrorColumn<-function(data) apply(data[,2:ncol(data)], 2, stdError) # function to apply stdError to columns
        meanPerCellGridSize <- ddply(data, .(CellGridSize), colMeans) # apply mean factorized by numFish
        stdErrorPerCellGridSize <- ddply(data, .(CellGridSize), stdErrorColumn) # apply stdErrorColumn factorized by numFish
        meanAndStdError <- merge(meanPerCellGridSize, stdErrorPerCellGridSize, by="CellGridSize")
}

allDataFrames <- lapply(complete, mainRoutine) # applies the routine to all the dataframes

CAList <- list()
for (i in 1:length(CAOrdered)) { # create a list of vectors, each a replicate of the number of cells
        CAList[[i]] <- rep(CAOrdered[i], nrow(allDataFrames[[i]]))
}
for (k in 1:length(allDataFrames)) { # merges cellList to the data frames
        allDataFrames[[k]]$CAUpdate <- CAList[[k]]
}
for (k in 1:length(allDataFrames)) { # renames the columns of the data frames
        colnames(allDataFrames[[k]])<- c("Grid", "Foodpatch", "Fishonfood", "fp_se", "fof_se", "CAupdate")
}
for (k in 1:length(allDataFrames)) { # divides fish on food by number of cells (standardization)
        allDataFrames[[k]]$fishOnFoodStd <- allDataFrames[[k]][,3]/allDataFrames[[k]][,2]
}
for (k in 1:length(allDataFrames)) { # calculates the standard error of the new variable (according to error propagation rules)
        allDataFrames[[k]]$fishOnFoodStd_se <- sqrt((allDataFrames[[k]][,5]/allDataFrames[[k]][,3])^2+
                                                            (allDataFrames[[k]][,4]/allDataFrames[[k]][,2])^2)
}

temp <- as.data.frame(matrix(nrow=nrow(allDataFrames[[1]]), ncol=length(allDataFrames)))
for (i in 1:length(allDataFrames)) {
        temp[,i] <- allDataFrames[[i]][,length(allDataFrames[[i]])-1] 
}
temp2 <- as.data.frame(matrix(nrow=nrow(allDataFrames[[1]]), ncol=length(allDataFrames)))
for (i in 1:length(allDataFrames)) {
        temp2[,i] <- allDataFrames[[i]][,length(allDataFrames[[i]])] 
}

cellsOrdered <- c(40,80,160,320,640,1280)
updateAll <- data.frame(cellsOrdered, temp, temp2)
colnames(updateAll)<- c("Grid", "t2", "t4", "t8", "t16", "t32", "t64", "se2", "se4", "se8", "se16",
                        "se32", "se64")
meltupdateAll <- melt(updateAll, id.vars="Grid")

limitsa<-aes(ymax=updateAll$t2+updateAll$se2, ymin=updateAll$t2-updateAll$se2)
limitsb<-aes(ymax=updateAll$t4+updateAll$se4, ymin=updateAll$t4-updateAll$se4)
limitsc<-aes(ymax=updateAll$t8+updateAll$se8, ymin=updateAll$t8-updateAll$se8)
limitsd<-aes(ymax=updateAll$t16+updateAll$se16, ymin=updateAll$t16-updateAll$se16)
limitse<-aes(ymax=updateAll$t32+updateAll$se32, ymin=updateAll$t32-updateAll$se32)
limitsf<-aes(ymax=updateAll$t64+updateAll$se64, ymin=updateAll$t64-updateAll$se64)


plotUpdateAll <-ggplot(subset(meltupdateAll, variable=="t2" | variable=="t4" |
                                      variable== "t8" | variable== "t16" | variable== "t32" | 
                                      variable== "t64"),
                       aes(x=Grid, y=value, fill=variable))+
        geom_line(aes(linetype=variable), size=0.2)+
        scale_linetype_manual(values=c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash"))+
        geom_point(aes(shape=variable, fill=variable), size=3)+
        #coord_trans(x="log2")+
        scale_shape_manual(values=c(25,22,23,24,21,7))+
        scale_fill_manual(values=rep("darkgrey",6))+
        scale_x_log10("Grid cells", breaks=cellsOrdered)+
        scale_y_continuous("Fish on food", breaks=seq(0,5, by=0.5), 
                           limits=c(0,5), expand=c(0,0))+
        geom_errorbar(limitsa, data=subset(meltupdateAll, variable=="t2"), width=0.02, position='dodge')+
        geom_errorbar(limitsb, data=subset(meltupdateAll, variable=="t4"), width=0.02, position='dodge')+
        geom_errorbar(limitsc, data=subset(meltupdateAll, variable=="t8"), width=0.02, position='dodge')+
        geom_errorbar(limitsd, data=subset(meltupdateAll, variable=="t16"), width=0.02, position='dodge')+
        geom_errorbar(limitse, data=subset(meltupdateAll, variable=="t32"), width=0.02, position='dodge')+        
        geom_errorbar(limitsf, data=subset(meltupdateAll, variable=="t64"), width=0.02, position='dodge')+        
        theme_bw()+
        theme(panel.grid.major.x = element_blank())+
        theme(axis.title.x = element_text(size=12,vjust=-0.3),
              axis.title.y = element_text(size=12,vjust=1))+
        theme(legend.title = element_text(size=12))+
        theme(axis.text.x=element_text(size=10))+
        theme(axis.text.y=element_text(size=10))

plotUpdateAll # aesthetics missing

write.table(updateAll, "update_ind.csv")



# need to extract the columns with mean and se for each update time




