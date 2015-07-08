# script to read and process the output of the evolution scenario.
# need to figure out a way to read only the part I need.
# please name each folder containing the F_Results_*.csv files with the corresponding number of 
# cells of the grid, always with 4 digits (i.e. 0040, 0080, 0160, 0320, 0640, 1280).

library(plyr)
library(reshape)
library(ggplot2)
library(scales)
setwd("C:/Users/Alberto/Documents/swarm_runs/E3_nn") 
listOfFiles <- list.files("C:/Users/Alberto/Documents/swarm_runs/E3_nn", 
                          pattern="*.csv", recursive=TRUE) # write list with all the filenames of the output


skip1 <- 5
skip2 <- 2017-(skip1+1)
skip3 <- 4101-(skip1+skip2+2)
skip4 <- 8267-(skip1+skip2+skip3+3)
skip5 <- 16601-(skip1+skip2+skip3+skip4+4)
skip6 <- 33267-(skip1+skip2+skip3+skip4+skip5+5)
skip7 <- 66600-(skip1+skip2+skip3+skip4+skip5+skip6+6)


# function to skim over the list of F_Results files and read only the desired lines. can be improved a lot.
# for each file, once the lines are read they are bound into a data frame and renamed. function returns
# one dataframe per each run with one line per desired timestep
selectiveReader <- function(data) {

connectionFile <- file(data)
open(connectionFile)

time0 <- read.table(connectionFile,skip=skip1,nrow=1, header=FALSE) # 6-th line
time6250 <- read.table(connectionFile,skip=skip2,nrow=1, header=FALSE) # 2018-th line
time12500 <- read.table(connectionFile,skip=skip3,nrow=1, header=FALSE) # 4102-nd line
time25000 <- read.table(connectionFile,skip=skip4,nrow=1, header=FALSE) # 8268-th line
time50000 <-read.table(connectionFile,skip=skip5,nrow=1, header=FALSE) # 16602-nd line
time100000 <- read.table(connectionFile,skip=skip6,nrow=1, header=FALSE) # 33268-th line
time200000 <- read.table(connectionFile,skip=skip7,nrow=1, header=FALSE) # 66601-st line

close(connectionFile)
run <-rbind(time0, time6250, time12500, time25000, time50000, time100000, time200000)
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
        cumulative <- x[,5]+x[,6]+(x[,4]/2)
        df <- data.frame(x, cumulative)
        df <- df[,c(1,7)]
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

# standard error function definition
stdError <- function(x) sqrt(var(x)/length(x)) # function defining standard error

# calculation of the mean and standard error for the sublists
mean40 <- rowMeans(sapply(cells40, function(X) X$cumulative))
se40 <- apply(sapply(cells40, function(X) X$cumulative), 1, stdError)

mean80 <- rowMeans(sapply(cells80, function(X) X$cumulative))
se80 <- apply(sapply(cells80, function(X) X$cumulative), 1, stdError)

mean160 <- rowMeans(sapply(cells160, function(X) X$cumulative))
se160 <- apply(sapply(cells160, function(X) X$cumulative), 1, stdError)

mean320 <- rowMeans(sapply(cells320, function(X) X$cumulative))
se320 <- apply(sapply(cells320, function(X) X$cumulative), 1, stdError)

mean640 <- rowMeans(sapply(cells640, function(X) X$cumulative))
se640 <- apply(sapply(cells640, function(X) X$cumulative), 1, stdError)

mean1280 <- rowMeans(sapply(cells1280, function(X) X$cumulative))
se1280 <- apply(sapply(cells1280, function(X) X$cumulative), 1, stdError)

Timesteps <- c(3125,6250,12500,25000,50000,100000,200000) # artefact, for the plot

probMeanAndStdErr <- data.frame(Timesteps, mean40, mean80, mean160, mean320, mean640, mean1280, 
                                se40, se80, se160, se320, se640, se1280)
meltprobMeanAndStdErr <- melt(probMeanAndStdErr, id.vars="Timesteps")

# definition of the limits of the error bars in the plot
limits1 <- aes(ymax=probMeanAndStdErr$mean40+probMeanAndStdErr$se40, ymin=probMeanAndStdErr$mean40-probMeanAndStdErr$se40)
limits2 <- aes(ymax=probMeanAndStdErr$mean80+probMeanAndStdErr$se80, ymin=probMeanAndStdErr$mean80-probMeanAndStdErr$se80)
limits3 <- aes(ymax=probMeanAndStdErr$mean160+probMeanAndStdErr$se160, ymin=probMeanAndStdErr$mean160-probMeanAndStdErr$se160)
limits4 <- aes(ymax=probMeanAndStdErr$mean320+probMeanAndStdErr$se320, ymin=probMeanAndStdErr$mean320-probMeanAndStdErr$se320)
limits5 <- aes(ymax=probMeanAndStdErr$mean640+probMeanAndStdErr$se640, ymin=probMeanAndStdErr$mean640-probMeanAndStdErr$se640)
limits6 <- aes(ymax=probMeanAndStdErr$mean1280+probMeanAndStdErr$se1280, ymin=probMeanAndStdErr$mean1280-probMeanAndStdErr$se1280)


plotprobMeanAndStdErr <-ggplot(subset(meltprobMeanAndStdErr, variable=="mean40" | variable=="mean80" |
                                            variable== "mean160" | variable== "mean320"| variable== "mean640" | 
                                              variable== "mean1280"),
                             aes(x=Timesteps, y=value, fill=variable))+
        geom_line(aes(linetype=variable), size=0.2)+
        scale_linetype_manual(values=c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash"))+
        geom_point(aes(shape=variable, fill=variable), size=3)+
        #coord_trans(x="log2")+
        scale_shape_manual(values=c(25,22,23,24,21,7))+
        scale_fill_manual(values=rep("darkgrey",6))+
        scale_x_log10("Time steps", breaks=Timesteps, labels=c(0,Timesteps[2:length(Timesteps)]))+
        scale_y_continuous("Schooling probability > 50%", breaks=seq(0,100, by=10), 
                           limits=c(0,100), expand=c(0,0))+
        geom_errorbar(limits1, data=subset(meltprobMeanAndStdErr, variable=="mean40"), width=0.02, position='dodge')+
        geom_errorbar(limits2, data=subset(meltprobMeanAndStdErr, variable=="mean80"), width=0.02, position='dodge')+
        geom_errorbar(limits3, data=subset(meltprobMeanAndStdErr, variable=="mean160"), width=0.02, position='dodge')+
        geom_errorbar(limits4, data=subset(meltprobMeanAndStdErr, variable=="mean320"), width=0.02, position='dodge')+
        geom_errorbar(limits5, data=subset(meltprobMeanAndStdErr, variable=="mean640"), width=0.02, position='dodge')+
        geom_errorbar(limits6, data=subset(meltprobMeanAndStdErr, variable=="mean1280"), width=0.02, position='dodge')+
        theme_bw()+
        theme(panel.grid.major.x = element_blank())+
        theme(axis.title.x = element_text(size=12,vjust=-0.3),
              axis.title.y = element_text(size=12,vjust=1))+
        theme(legend.title = element_text(size=12))+
        theme(axis.text.x=element_text(size=10))+
        theme(axis.text.y=element_text(size=10))

plotprobMeanAndStdErr

ggsave("Plot evolution.pdf", plotprobMeanAndStdErr, useDingbats=FALSE)



