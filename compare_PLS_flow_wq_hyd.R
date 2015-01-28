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
png.fn <- "plsfdc00"

## get common y.lims for flow axis
y.max <- 10^ceiling(log10(with(df.data,max(pero,agwo,ifwo,suro))))
y.min <- 10^floor(log10(with(df.data,min(pero,agwo,ifwo,suro))))
png(filename = paste0(dir.hydro,"/",png.fn,".png"), width = 11, height = 8.5,units = "in",res=300,bg = "white")
plot.pero.fdc <- ggplot(data=df.data) + 
  scale_y_log10(breaks = trans_breaks("log10",function(x) 10^x)(c(y.min,y.max)),
  labels = trans_format("log10", math_format(10^.x))) +
  xlim(0,100) + ylab("outflow (ac-ft)") + xlab("Flow Exceedance (%)") +
  geom_line(stat="identity",aes(x=pero.exceed,y=pero,colour=run,linetype=run,size=run)) +
  scale_size_manual(values=c(1.25,1)) +
  scale_linetype_manual(values=c("solid","dashed")) +
  scale_colour_manual(values=c("blue","red")) +
  ggtitle("PERO outflow from PLSs")
plot.agwo.fdc <- ggplot(data=df.data) + 
  scale_y_log10(breaks = trans_breaks("log10",function(x) 10^x)(c(y.min,y.max)),
                labels = trans_format("log10", math_format(10^.x))) +
  xlim(0,100) + ylab("Outflow (ac-ft)") + xlab("Flow Exceedance (%)") +
  geom_line(stat="identity",aes(x=agwo.exceed,y=agwo,colour=run,linetype=run,size=run)) +
  scale_size_manual(values=c(1.25,1)) +
  scale_linetype_manual(values=c("solid","dashed")) +
  scale_colour_manual(values=c("blue","red"))  +
  ggtitle("AGWO outflow from PLSs")
plot.ifwo.fdc <- ggplot(data=df.data) + 
  scale_y_log10(breaks = trans_breaks("log10",function(x) 10^x)(c(y.min,y.max)),
                labels = trans_format("log10", math_format(10^.x))) +
  xlim(0,100) + ylab("outflow (ac-ft)") + xlab("Flow Exceedance (%)") +
  geom_line(stat="identity",aes(x=ifwo.exceed,y=ifwo,colour=run,linetype=run,size=run)) +
  scale_size_manual(values=c(1.25,1)) +
  scale_linetype_manual(values=c("solid","dashed")) +
  scale_colour_manual(values=c("blue","red"))  +
  ggtitle("IFWO outflow from PLSs")
plot.suro.fdc <- ggplot(data=df.data) + 
  scale_y_log10(breaks = trans_breaks("log10",function(x) 10^x)(c(y.min,y.max)),
                labels = trans_format("log10", math_format(10^.x))) +
  xlim(0,100) + ylab("outflow (ac-ft)") + xlab("Flow Exceedance (%)") +
  geom_line(stat="identity",aes(x=suro.exceed,y=suro,colour=run,linetype=run,size=run)) +
  scale_size_manual(values=c(1.25,1)) +
  scale_linetype_manual(values=c("solid","dashed")) +
  scale_colour_manual(values=c("blue","red"))  +
  ggtitle("SURO outflow from PLSs")
plot.flow.dist <- ggplot(data=df.data) + 
  xlim(0,100) + ylim(0,100) + ylab("% of PERO") + xlab("Flow Exceedance (%)") +
  geom_smooth(aes(x=pero.exceed,y=agwo.per,colour=run,linetype=run,size=run)) +
  geom_smooth(aes(x=pero.exceed,y=ifwo.per,colour=run,linetype=run,size=run)) +
  geom_smooth(aes(x=pero.exceed,y=suro.per,colour=run,linetype=run,size=run)) +
  annotate("text",x=95,y=quantile(df.data$agwo.per,probs=0.5),label="agwo") +
  annotate("text",x=0,y=max(df.data$ifwo.per),label="ifwo") +
  annotate("text",x=25,y=quantile(df.data$suro.per,probs=0.99),label="suro") +
  scale_size_manual(values=c(1.25,1)) +
  scale_linetype_manual(values=c("solid","dashed")) +
  scale_colour_manual(values=c("blue","red"))  +
  ggtitle("Flow paths for PLS")

##multiplot(plot.pero.fdc,plot.agwo.fdc,plot.ifwo.fdc,plot.suro.fdc,cols=2)
multiplot(plot.pero.fdc,plot.flow.dist,cols=1)
dev.off()
##
## look at differences between the two FDCs
df.diff <- data.frame(flow.exceed=df.wq$flow.exceed,diff=100*(df.wq$flow - df.hyd$flow)/df.hyd$flow)
y.max <- 100
y.min <- -100
png(filename = paste0(dir.hydro,"/diff_",png.fn,".png"), width = 11, height = 8.5,units = "in",res=300,bg = "white")
plot.fdc.diff <- ggplot(data=df.diff) + ylim(y.min,y.max) +
  xlim(0,100) + ylab("% Difference in Flow (100*(wq-hyd)/hyd)") + xlab("Flow Exceedance (%)") +
  geom_line(stat="identity",aes(x=flow.exceed,y=diff))
plot(plot.fdc.diff)
dev.off()
y.max <- 5
y.min <- -5
png(filename = paste0(dir.hydro,"/diff_",png.fn,"_zm.png"), width = 11, height = 8.5,units = "in",res=300,bg = "white")
plot.fdc.diff.zm <- ggplot(data=df.diff) + ylim(y.min,y.max) +
  xlim(0,100) + ylab("% Difference in Flow (100*(wq-hyd)/hyd)") + xlab("Flow Exceedance (%)") +
  geom_line(stat="identity",aes(x=flow.exceed,y=diff))
plot(plot.fdc.diff.zm)
dev.off()
##
## the flows match. Check the figures in fdcs00.png, diff_fdcs00.png, and diff_fdcs00_zm.png
##
## clean up
rm(list=c(ls(pattern="^fl\\.*"),ls(pattern="^chr\\.*"),ls(pattern="^plot\\.*"),ls(pattern="^y\\.*"))

