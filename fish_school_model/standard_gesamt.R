
setwd("C:/Users/Alberto/Documents/swarm_runs/F2_Std_s") # points the folder with the batch output
library(plyr)
library(reshape)
library(ggplot2)
list <- list.files("C:/Users/Alberto/Documents/swarm_runs/F2_Std_s", 
                         pattern="*.csv", recursive=TRUE)

cells<- as.numeric(gsub("[^0-9]", "", list)) # order
cellsOrdered <- sort(cells)
inds <- sort.int(cells, index.return=TRUE)[[2]] 
list<- list[order(list)[inds]] 

read.special<-function(x) {
        read.csv(x, header=T, sep='\t', dec='.') # custom function to read the batches of .csv keeping the header
}
data_list <- lapply(list, read.special)

nocomma <- function(data) {
        data[,8] <- as.numeric(gsub(",","", data[,8])) # eliminates the comma as thousand separator
        data <- data[c(2,4,6,7,8)]
}
squeeze <- lapply(data_list, nocomma) # reduces dataframes


routine <- function(data) {
        stdError <- function(x) sqrt(var(x)/length(x)) # function defining standard error
        stdErrorColumn<-function(data) apply(data[,2:ncol(data)], 2, stdError) # function to apply stdError to columns
        meanPerNumberOfFish <- ddply(data, .(numFish), colMeans) # apply mean factorized by numFish
        stdErrorPerNumberOfFish <- ddply(data, .(numFish), stdErrorColumn) # apply stdErrorColumn factorized by numFish
        meanAndStdError <- merge(meanPerNumberOfFish, stdErrorPerNumberOfFish, by="numFish")
}

frames <- lapply(squeeze, routine) # applies the routine to all the dataframes

wantedFishNumber=30 # to be changed to the desired numFish
relevantFish <- lapply(frames, function(x) subset(x, x$numFish==wantedFishNumber))
relevantFish <- do.call("rbind", relevantFish)
relevantFish$nOfCells <- sort(cells)

colnames(relevantFish) <- c("Numberoffish", "Polarization", "Foodpatch", "Fishonfood", "Schoolextent",
                            "pol_se", "fp_se", "fof_se", "se_se", "Grid")
comb <- melt(relevantFish, id.vars="Grid",variable.name= "variable", 
              value.name="value") # melt the dataset for ggplot
# mapping of the limits for the errorbar plot, each mean +- its sd. one entry per class is necessary
limits1<-aes(ymax=relevantFish$Polarization+relevantFish$pol_se, ymin=relevantFish$Polarization-relevantFish$pol_se)
limits2<-aes(ymax=relevantFish$Foodpatch+relevantFish$fp_se, ymin=relevantFish$Foodpatch-relevantFish$fp_se)
limits3<-aes(ymax=relevantFish$Fishonfood+relevantFish$fof_se, ymin=relevantFish$Fishonfood-relevantFish$fof_se)
limits4<-aes(ymax=relevantFish$Schoolextent+relevantFish$se_se, ymin=relevantFish$Schoolextent-relevantFish$se_se)
pd <- position_dodge(0.2)

gplot <-ggplot(subset(comb, variable=="Polarization" | variable=="Foodpatch" |
                              variable== "Fishonfood" | variable== "Schoolextent"),
               aes(x=Grid, y=value, group=variable))+
        geom_line(aes(linetype=variable), size=0.2)+
        scale_linetype_manual(values=c("solid", "dashed", "dotted", "dotdash"))+
        geom_point(aes(shape=variable, fill=variable), size=3)+
        #coord_trans(x="log2")+
        scale_shape_manual(values=c(0,5,6,2))+
        scale_x_log10("Grid cells", breaks=cellsOrdered, expand=c(0,0))+
        scale_y_continuous(breaks=seq(0,200, by=10), 
                           limits=c(0,200), expand=c(0,0))+
        geom_errorbar(limits1, data=subset(comb, variable=="Polarization"), width=0.05)+
        geom_errorbar(limits2, data=subset(comb, variable=="Foodpatch"), width=0.05, position=pd)+
        geom_errorbar(limits3, data=subset(comb, variable=="Fishonfood"), width=0.05)+
        geom_errorbar(limits4, data=subset(comb, variable=="Schoolextent"), width=0.05, position=pd)+
        theme_bw()
        
        
gplot

cutframes <- lapply(frames, function(data) data[,c(1,3,4,7,8)])
cellList <- list()
for (i in 1:length(cellsOrdered)) {
        cellList[[i]] <- rep(cellsOrdered[i], nrow(cutframes[[i]]))
}

for (k in 1:length(cutframes)) {
       cutframes[[k]]$Cells <- cellList[[k]]
}
for (k in 1:length(cutframes)) {
        colnames(cutframes[[k]])<- c("Numberoffish", "Foodpatch", "Fishonfood", "fp_se", "fof_se", "Grid")
}
for (k in 1:length(cutframes)) {
        cutframes[[k]]$fishOnFoodStd <- cutframes[[k]][,3]/cutframes[[k]][,2]
}
for (k in 1:length(cutframes)) {
        cutframes[[k]]$fishOnFoodStd_se <- sqrt((cutframes[[k]][,5]/cutframes[[k]][,3])^2+
                                                        (cutframes[[k]][,4]/cutframes[[k]][,2])^2)
}
glob <-do.call("rbind", cutframes)
#glob[,1] <- as.factor(glob[,1])
globsplit <- split(glob, glob$Numberoffish)
temp <- as.data.frame(matrix(nrow=nrow(globsplit[[1]]), ncol=length(levels(as.factor(glob[,1])))))
for (i in 1:length(globsplit)) {
       temp[,i] <- globsplit[[i]][,length(globsplit[[i]])-1] 
}
temp2 <- as.data.frame(matrix(nrow=nrow(globsplit[[1]]), ncol=length(levels(as.factor(glob[,1])))))
for (i in 1:length(globsplit)) {
        temp2[,i] <- globsplit[[i]][,length(globsplit[[i]])] 
}
ayy <- data.frame(cellsOrdered, temp, temp2)
colnames(ayy)<- c("Grid", "f20", "f30", "f40", "f50", "f60", "se20", "se30", "se40", "se50", "se60")
comb2 <- melt(ayy, id.vars="Grid")

# mapping of the limits for the errorbar plot, each mean +- its sd. one entry per class is necessary
limitsa<-aes(ymax=ayy$f20+ayy$se20, ymin=ayy$f20-ayy$se20)
limitsb<-aes(ymax=ayy$f30+ayy$se30, ymin=ayy$f30-ayy$se30)
limitsc<-aes(ymax=ayy$f40+ayy$se40, ymin=ayy$f40-ayy$se40)
limitsd<-aes(ymax=ayy$f50+ayy$se50, ymin=ayy$f50-ayy$se50)
limitse<-aes(ymax=ayy$f60+ayy$se60, ymin=ayy$f60-ayy$se60)


gplot2 <-ggplot(subset(comb2, variable=="f20" | variable=="f30" |
                                variable== "f40" | variable== "f50" | variable== "f60"),
                 aes(x=Grid, y=value, group=variable))+
        geom_line(aes(linetype=variable), size=0.2)+
        scale_linetype_manual(values=c("solid", "dashed", "dotted", "dotdash", "longdash"))+
        geom_point(aes(shape=variable, fill=variable), size=3)+
        #coord_trans(x="log2")+
        scale_shape_manual(values=c(0,5,6,2,3))+
        scale_x_log10("Grid cells", breaks=cellsOrdered, expand=c(0,0))+
        scale_y_continuous(breaks=seq(0,5, by=0.5), 
                           limits=c(0,5), expand=c(0,0))+
        geom_errorbar(limitsa, data=subset(comb2, variable=="f20"), width=0.05)+
        geom_errorbar(limitsb, data=subset(comb2, variable=="f30"), width=0.05, position=pd)+
        geom_errorbar(limitsc, data=subset(comb2, variable=="f40"), width=0.05)+
        geom_errorbar(limitsd, data=subset(comb2, variable=="f50"), width=0.05, position=pd)+
        geom_errorbar(limitse, data=subset(comb2, variable=="f60"), width=0.05)+        
        theme_bw()
gplot2











        