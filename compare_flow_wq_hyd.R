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
## wq output
fl.wq <- "beflwq.out"
chr.wq <- scan(paste0(dir.hydro,"/",fl.wq),what="character",skip=26,sep="\n")
##
## hyd output
##
fl.hyd <- "beflhyd.out"
chr.hyd <- scan(paste0(dir.hydro,"/",fl.hyd),what="character",skip=26,sep="\n")
## 
## create data frames
##
## wq
df.wq <- data.frame(run="wq",date=strptime(substr(chr.wq,start=7,stop=16),format="%Y %m %d"),
                    flow=as.numeric(substr(chr.wq,start=23,stop=37)),
                    stringsAsFactors=TRUE)
## hyd
df.hyd <- data.frame(run="hyd",date=strptime(substr(chr.hyd,start=7,stop=16),format="%Y %m %d"),
                    flow=as.numeric(substr(chr.hyd,start=23,stop=37)),
                    stringsAsFactors=TRUE)
##
## calculate flow duration curves
## add to the existing data frames
##
## wq
df.wq <- cbind(df.wq,flow.exceed=100 - 100*rank(df.wq$flow, na.last="keep",
                                                  ties.method="average") / length(df.wq$flow))
df.wq <- df.wq[ order(df.wq$flow.exceed), ]
row.names(df.wq) <- NULL
##
## hyd
df.hyd <- cbind(df.hyd,flow.exceed=100 - 100*rank(df.hyd$flow, na.last="keep",
                                                ties.method="average") / length(df.hyd$flow))
df.hyd <- df.hyd[ order(df.hyd$flow.exceed), ]
row.names(df.hyd) <- NULL
##
## plot FDCs
## create long format data frame with "run" variable as a factor  having values of "org" or "wq"
df.data <- rbind(df.hyd,df.wq)
## get common y.lims for flow axis
y.max <- 10^ceiling(log10(max(df.data$flow)))
y.min <- 10^floor(log10(min(df.data$flow)))
png(filename = paste0(dir.hydro,"/",png.fn,".png"), width = 11, height = 8.5,units = "in",res=300,bg = "white")
plot.fdc <- ggplot(data=df.data) + 
  scale_y_log10(breaks = trans_breaks("log10",function(x) 10^x)(c(y.min,y.max)),
  labels = trans_format("log10", math_format(10^.x))) +
  xlim(0,100) + ylab("Flow (cfs)") + xlab("Flow Exceedance (%)") +
  geom_line(stat="identity",aes(x=flow.exceed,y=flow,colour=run,linetype=run,size=run)) +
  scale_size_manual(values=c(1.25,1)) +
  scale_linetype_manual(values=c("solid","dashed")) +
  scale_colour_manual(values=c("blue","red"))
plot(plot.fdc)
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
##rm(list=c(ls(pattern="^fl\\.*"),ls(pattern="^chr\\.*"),ls(pattern="^plot\\.*"),ls(pattern="^y\\.*"),ls(pattern="^png\\.*")))

