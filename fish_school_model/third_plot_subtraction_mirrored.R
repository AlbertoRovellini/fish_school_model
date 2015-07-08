library(reshape)
library(ggplot2)
library(gridExtra)
setwd("C:/Users/Alberto/Documents/swarm_runs/new_output_Std/mirrored")
listOfFiles <- list.files("C:/Users/Alberto/Documents/swarm_runs/new_output_Std/mirrored", 
                          pattern="*.csv", recursive=TRUE) # write list with all the filenames of the output
read.special<-function(x) { # file reader
        read.csv(x, header=T, sep=' ', dec='.') 
}
allData <- lapply(listOfFiles, read.special) # reads all the output files

differenceSchoolInd <- as.data.frame(allData[[2]][,c(2:6)]-allData[[1]][,c(2:6)])
differenceStdError <- as.data.frame(sqrt((allData[[2]][,c(7:11)])^2+(allData[[1]][,c(7:11)])^2))
differenceFrame <- data.frame(allData[[1]][,1], differenceSchoolInd, differenceStdError)
differenceFrame <- rename(differenceFrame, c(allData..1.....1.="Cellsize"))
meltDifference <- melt(differenceFrame, id.vars="Cellsize")

# mapping of the limits for the errorbar plot, each mean +- its se. one entry per class is necessary
limits1<-aes(ymax=differenceFrame$f20+differenceFrame$se20, ymin=differenceFrame$f20-differenceFrame$se20)
limits2<-aes(ymax=differenceFrame$f30+differenceFrame$se30, ymin=differenceFrame$f30-differenceFrame$se30)
limits3<-aes(ymax=differenceFrame$f40+differenceFrame$se40, ymin=differenceFrame$f40-differenceFrame$se40)
limits4<-aes(ymax=differenceFrame$f50+differenceFrame$se50, ymin=differenceFrame$f50-differenceFrame$se50)
limits5<-aes(ymax=differenceFrame$f60+differenceFrame$se60, ymin=differenceFrame$f60-differenceFrame$se60)
pd <- position_dodge(0.5) # set the dodge span for the errorbars in ggplot
cellsOrdered <- c(40, 80, 160, 320, 640, 1280)
cellSize <- c(500,250,125,62.500,31.250,15.625) # vector with the dimensions of the cells

plotmeltDifferenceAll <-ggplot(subset(meltDifference, variable=="f20" | variable=="f30" |
                                          variable== "f40" | variable== "f50" | variable== "f60"),
                           aes(x=Cellsize, y=value, fill=variable))+
        geom_line(aes(linetype=variable), size=0.2)+
        scale_linetype_manual(values=c("solid", "dashed", "dotted", "dotdash", "longdash"))+
        geom_point(aes(shape=variable, fill=variable), size=3)+
        #coord_trans(x="log2")+
        scale_shape_manual(values=c(25,22,23,24,21))+
        scale_fill_manual(values=rep("darkgrey",5))+
        scale_x_log10("Cell size", breaks=cellSize)+
        scale_y_continuous("Fish on food", breaks=seq(-0.2,1.4, by=0.2), 
                           limits=c(-0.2,1.4), expand=c(0,0))+
        geom_errorbar(limits1, data=subset(meltDifference, variable=="f20"), width=0.02)+
        geom_errorbar(limits2, data=subset(meltDifference, variable=="f30"), width=0.02, position=pd)+
        geom_errorbar(limits3, data=subset(meltDifference, variable=="f40"), width=0.02)+
        geom_errorbar(limits4, data=subset(meltDifference, variable=="f50"), width=0.02, position=pd)+
        geom_errorbar(limits5, data=subset(meltDifference, variable=="f60"), width=0.02)+        
        theme_bw()+
        theme(panel.grid.major.x = element_blank())+
        theme(axis.title.x = element_text(size=12,vjust=-0.3),
              axis.title.y = element_text(size=12,vjust=1))+
        theme(legend.title = element_text(size=12))+
        theme(axis.text.x=element_text(size=10))+
        theme(axis.text.y=element_text(size=10))

plotmeltDifferenceAll # aesthetics missing

ggsave("Difference_mirrored.pdf", plotmeltDifferenceAll, useDingbats=FALSE)


#pdf("plots.pdf", width=5, height=10, useDingbats=FALSE)
#panel <- grid.arrange(plotFishOnFoodSwarm, plotFishOnFoodInd, plotmeltDifferenceAll, ncol=1)
#dev.off() # not working as I want
