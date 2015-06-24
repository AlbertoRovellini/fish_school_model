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

# code region to exclude the school expanse outliers from the dataframe. school expanse upper limit
# corresponds to numFish + 40. 

subsetter <- function(frame) {
        frame1 <- split(frame, frame[1])
        frame2 <- lapply(frame1, function(data) subset(data, data[5]<=data[1,1]+40))
        frame3 <- do.call(rbind, frame2)
        #return(frame3)
}

shortData <- lapply(shortData, subsetter) # overwrite shortData with the version with no outliers

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
                        aes(x=Grid, y=value, fill=variable))+
        geom_line(aes(linetype=variable), size=0.5)+
        scale_linetype_manual(values=c("solid", "dashed", "dotted", "longdash"))+
        geom_point(aes(shape=variable, fill=variable), size=3.5)+
        scale_shape_manual(values=c(25,22,23,24))+
        scale_fill_manual(values=rep("darkgrey",4))+
        scale_x_log10("Grid cells", breaks=cellsOrdered)+
        scale_y_continuous(breaks=seq(0,90, by=10), 
                           limits=c(0,90), expand=c(0,0))+
        geom_errorbar(limits1, data=subset(meltFish, variable=="Polarization"), width=0.02, position='dodge')+
        geom_errorbar(limits2, data=subset(meltFish, variable=="Foodpatch"), width=0.02, position='dodge')+
        geom_errorbar(limits3, data=subset(meltFish, variable=="Fishonfood"), width=0.02, position='dodge')+
        geom_errorbar(limits4, data=subset(meltFish, variable=="Schoolextent"), width=0.02, position='dodge')+
        theme_bw()+
        theme(panel.grid.major.x = element_blank())+
        theme(axis.title.x = element_text(size=12,vjust=-0.5),
              axis.title.y = element_blank())+
        theme(legend.title = element_text(size=12))+
        theme(axis.text.x=element_text(size=10))+
        theme(axis.text.y=element_text(size=10))


plotNumberFish # dodging fails, aesthetics need to be reviewed
ggsave("First plot.pdf", plotNumberFish, useDingbats=FALSE)
