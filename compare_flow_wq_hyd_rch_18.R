## comapre flows for the Big Elk Creek hspf simulations using the meteorlogical input
## developed by The Cadmus Group and by Tetra Tech.
##
## load packages
library(ggplot2)
library(scales)
##
## work directories
dir.hydro <- "e:/pest/bigelkpest/hydro"
##
## conversion factors
cf.s <- 24*60*60 ## conversion factor from day to second
##
## graph file filename part for Run 00
png.fn <- "fdcs00"

##
## plot FDCs
## get common y.lims for flow axis
y.max <- 10^ceiling(log10(max(df.data$flow.cfs)))
y.min <- 10^floor(log10(min(df.data$flow.cfs)))
png(filename = paste0(dir.hydro,"/",png.fn,".png"), width = 11, height = 8.5,units = "in",res=300,bg = "white")
plot.fdc <- ggplot(data=df.data) + 
  scale_y_log10(breaks = trans_breaks("log10",function(x) 10^x)(c(y.min,y.max)),
  labels = trans_format("log10", math_format(10^.x))) +
  xlim(0,100) + ylab("Flow (cfs)") + xlab("Flow Exceedance (%)") +
  geom_line(stat="identity",aes(x=flow.cfs.exceed,y=flow.cfs,colour=run,linetype=run,size=run)) +
  scale_size_manual(values=c(1.25,1)) +
  scale_linetype_manual(values=c("solid","dashed")) +
  scale_colour_manual(values=c("blue","red"))
plot(plot.fdc)
dev.off()
##
## look at differences between the two FDCs
head(df.data[df.data$run=="wq",])
df.diff <- data.frame(flow.exceed=df.data[df.data$run=="wq","flow.cfs.exceed"],
                      diff=100*(df.data[df.data$run=="wq","flow.cfs"] - df.data[df.data$run=="hyd","flow.cfs"])/df.data[df.data$run=="hyd","flow.cfs"])
y.max <- 100
y.min <- -100
png(filename = paste0(dir.hydro,"/diff_",png.fn,".png"), width = 11, height = 8.5,units = "in",res=300,bg = "white")
plot.fdc.diff <- ggplot(data=df.diff) + ylim(y.min,y.max) +
  xlim(0,100) + ylab("% Difference in Flow (100*(wq-hyd)/hyd)") + xlab("Flow Exceedance (%)") +
  geom_line(stat="identity",aes(x=flow.exceed,y=diff))
plot(plot.fdc.diff)
dev.off()
##
## the flows match. Check the figures in fdcs00.png, diff_fdcs00.png, and diff_fdcs00_zm.png
##
## clean up
##rm(list=c(ls(pattern="^fl\\.*"),ls(pattern="^chr\\.*"),ls(pattern="^plot\\.*"),ls(pattern="^y\\.*"),ls(pattern="^png\\.*")))

