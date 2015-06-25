library(reshape)
library(ggplot2)
library(gridExtra)
setwd("C:/Users/Alberto/Documents/swarm_runs/new_output_CA")
listOfFiles <- list.files("C:/Users/Alberto/Documents/swarm_runs/new_output_CA", 
                          pattern="*.csv", recursive=TRUE) # write list with all the filenames of the output
read.special<-function(x) { # file reader
        read.csv(x, header=T, sep=' ', dec='.') 
}
allData <- lapply(listOfFiles, read.special) # reads all the output files

differenceSchoolInd <- as.data.frame(allData[[2]][,c(2:7)]-allData[[1]][,c(2:7)])
differenceStdError <- as.data.frame(sqrt((allData[[2]][,c(8:13)])^2+(allData[[1]][,c(8:13)])^2))
differenceFrame <- data.frame(allData[[1]][,1], differenceSchoolInd, differenceStdError)
differenceFrame <- rename(differenceFrame, c(allData..1.....1.="Grid"))
meltDifference <- melt(differenceFrame, id.vars="Grid")
cellsOrdered <- c(40,80,160,320,640,1280) # vector with the meaningful grid sizes


# mapping of the limits for the errorbar plot, each mean +- its se. one entry per class is necessary
limitsa<-aes(ymax=differenceFrame$t2+differenceFrame$se2, ymin=differenceFrame$t2-differenceFrame$se2)
limitsb<-aes(ymax=differenceFrame$t4+differenceFrame$se4, ymin=differenceFrame$t4-differenceFrame$se4)
limitsc<-aes(ymax=differenceFrame$t8+differenceFrame$se8, ymin=differenceFrame$t8-differenceFrame$se8)
limitsd<-aes(ymax=differenceFrame$t16+differenceFrame$se16, ymin=differenceFrame$t16-differenceFrame$se16)
limitse<-aes(ymax=differenceFrame$t32+differenceFrame$se32, ymin=differenceFrame$t32-differenceFrame$se32)
limitsf<-aes(ymax=differenceFrame$t64+differenceFrame$se64, ymin=differenceFrame$t64-differenceFrame$se64)

pd <- position_dodge(0.5) # set the dodge span for the errorbars in ggplot

plotmeltDifferenceAll <-ggplot(subset(meltDifference, variable=="t2" | variable=="t4" |
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
        scale_y_continuous("Fish on food", breaks=seq(-0.4,1.4, by=0.2), 
                           limits=c(-0.4,1.4), expand=c(0,0))+
        geom_errorbar(limitsa, data=subset(meltDifference, variable=="t2"), width=0.02, position='dodge')+
        geom_errorbar(limitsb, data=subset(meltDifference, variable=="t4"), width=0.02, position='dodge')+
        geom_errorbar(limitsc, data=subset(meltDifference, variable=="t8"), width=0.02, position='dodge')+
        geom_errorbar(limitsd, data=subset(meltDifference, variable=="t16"), width=0.02, position='dodge')+
        geom_errorbar(limitse, data=subset(meltDifference, variable=="t32"), width=0.02, position='dodge')+        
        geom_errorbar(limitsf, data=subset(meltDifference, variable=="t64"), width=0.02, position='dodge')+ 
        theme_bw()+
        theme(panel.grid.major.x = element_blank())+
        theme(axis.title.x = element_text(size=12,vjust=-0.3),
              axis.title.y = element_text(size=12,vjust=1))+
        theme(legend.title = element_text(size=12))+
        theme(axis.text.x=element_text(size=10))+
        theme(axis.text.y=element_text(size=10))

plotmeltDifferenceAll # aesthetics missing

ggsave("Difference.pdf", plotmeltDifferenceAll, useDingbats=FALSE)


#pdf("plots.pdf", width=5, height=10, useDingbats=FALSE)
#panel <- grid.arrange(plotFishOnFoodSwarm, plotFishOnFoodInd, plotmeltDifferenceAll, ncol=1)
#dev.off() # not working as I want
