## comapre flows for the Big Elk Creek hspf simulations using the meteorlogical input
## developed by The Cadmus Group and by Tetra Tech.
##
## load packages
library(ggplot2)
library(scales)
##
## work directories
dir.hydro <- "e:/pest/bigelkpest/hydro"
dir.tt <- paste0(dir.hydro,"/tetratech")
dir.cd <- paste0(dir.hydro,"/cadmus")
##
##
## first run
## WDM file: bigelk_in.wdm
## UCI files
## HYD: bigelk.uci (Cadmus Hydro Cal, I added PLTGEN tables to the cadmus UCI flow for Rch 18 is written to a plotgen file "becdfl.out". This is the daily total flow out of the rch (ROVOL) in 100 ml.)
## WQ: bigelkwq.uci (This is the uci I have been using in the wq. I removed the wq part to make comaprisons of the files easier)
## Supplementary files
## HYD: bigelk.sup
## WQ: bigelkwq.sup
##
## Note: wq out is from the UCI I setup for the bacteria simulations
## the ouput is written to a plotgen file "beflsum.out". This is the daily total
## flow out of the rch (ROVOL) in 100 ml. Flows will be comapred in cubic feet 
## per second using the follwing conversion factors
##
## conversion factors
cf.100ml <- 1.2335E+07 ## conversion of ac-ft to 100 ml
cf.ac <- 43560.0004435 ## conversion factor form ac-ft to cubic feet
cf.s <- 24*60*60 ## conversion factor from day to second
## Run 00
## check the flow for the WQ using after removing all the parts using in the bacteria simulations and leaving only the hydrology parts
##
## graph file filename part for Run 00
png.fn <- "fdcs00"
##
## orginial wq
fl.org <- "beflsum_org.out"
chr.org <- scan(paste0(dir.hydro,"/",fl.org),what="character",skip=26,sep="\n")
##
## modified wq
##
fl.wq <- "beflsum.out"
chr.wq <- scan(paste0(dir.hydro,"/",fl.wq),what="character",skip=26,sep="\n")
## 
## create data frames
## 
## orig
df.org <- data.frame(run="org",date=strptime(substr(chr.org,start=7,stop=16),format="%Y %m %d"),
                     flow=as.numeric(substr(chr.org,start=79,stop=93))*(1/cf.100ml)*(cf.ac)*(1/cf.s),
                     stringsAsFactors=TRUE)
##
## wq
df.wq <- data.frame(run="wq",date=strptime(substr(chr.wq,start=7,stop=16),format="%Y %m %d"),
                    flow=as.numeric(substr(chr.wq,start=23,stop=37))*(1/cf.100ml)*(cf.ac)*(1/cf.s),
                    stringsAsFactors=TRUE)
##
## calculate flow duration curves
## add to the existing data frames
##
## org
df.org <- cbind(df.org,flow.exceed=100 - 100*rank(df.org$flow, na.last="keep",
                                                  ties.method="average") / length(df.org$flow))
df.org <- df.org[ order(df.org$flow.exceed), ]
row.names(df.org) <- NULL
## wq
df.wq <- cbind(df.wq,flow.exceed=100 - 100*rank(df.wq$flow, na.last="keep",
                                                ties.method="average") / length(df.wq$flow))
df.wq <- df.wq[ order(df.wq$flow.exceed), ]
row.names(df.wq) <- NULL
##
## plot FDCs
## create long format data frame with "run" variable as a factor  having values of "org" or "wq"
df.data <- rbind(df.org,df.wq)
## get common y.lims for flow axis
y.max <- 10^ceiling(log10(max(df.data$flow)))
y.min <- 10^floor(log10(min(df.data$flow)))
png(filename = paste0(dir.hydro,"/",png.fn,".png"), width = 11, height = 8.5,units = "in",res=300,bg = "white")
plot.fdc <- ggplot(data=df.data) + 
  scale_y_log10(breaks = trans_breaks("log10",function(x) 10^x)(c(y.min,y.max)),
                labels = trans_format("log10", math_format(10^.x))) +
  xlim(0,100) + ylab("Flow (cfs)") + xlab("Flow Exceedance (%)") +
  geom_line(stat="identity",aes(x=flow.exceed,y=flow,colour=run)) +
  scale_colour_manual(values=c("red","blue"))
plot(plot.fdc)
dev.off()
##
## look at differences between the two FDCs
df.diff <- data.frame(flow.exceed=df.wq$flow.exceed,diff=100*(df.wq$flow - df.org$flow)/df.org$flow)
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
rm(list=c(ls(pattern="^fl\\.*"),ls(pattern="^chr\\.*"),ls(pattern="^df\\.*"),ls(pattern="^plot\\.*"),ls(pattern="^y\\.*"),ls(pattern="^png\\.*")))
##
## Run 01
## compare the wq and the hyd flows
##
## graph file filename part for Run 01
png.fn <- "fdcs01"
##
## hyd calibration output
fl.hyd <- "beflhyd.out"
chr.hyd <- scan(paste0(dir.hydro,"/",fl.hyd),what="character",skip=26,sep="\n")
##
## modified wq
##
fl.wq <- "beflsum.out"
chr.wq <- scan(paste0(dir.hydro,"/",fl.wq),what="character",skip=26,sep="\n")
## 
## create data frames
## 
## hyd
df.hyd <- data.frame(run="hyd",date=strptime(substr(chr.hyd,start=7,stop=16),format="%Y %m %d"),
                     flow=as.numeric(substr(chr.hyd,start=23,stop=37))*(1/cf.100ml)*(cf.ac)*(1/cf.s),
                     stringsAsFactors=TRUE)
##
## wq
df.wq <- data.frame(run="wq",date=strptime(substr(chr.wq,start=7,stop=16),format="%Y %m %d"),
                    flow=as.numeric(substr(chr.wq,start=23,stop=37))*(1/cf.100ml)*(cf.ac)*(1/cf.s),
                    stringsAsFactors=TRUE)
##
## calculate flow duration curves
## add to the existing data frames
##
## hyd
df.hyd <- cbind(df.hyd,flow.exceed=100 - 100*rank(df.hyd$flow, na.last="keep",
                                                  ties.method="average") / length(df.hyd$flow))
df.hyd <- df.hyd[ order(df.hyd$flow.exceed), ]
row.names(df.hyd) <- NULL
## wq
df.wq <- cbind(df.wq,flow.exceed=100 - 100*rank(df.wq$flow, na.last="keep",
                                                ties.method="average") / length(df.wq$flow))
df.wq <- df.wq[ order(df.wq$flow.exceed), ]
row.names(df.wq) <- NULL
##
## plot FDCs
## create long format data frame with "run" variable as a factor  having values of "hyd" or "wq"
df.data <- rbind(df.hyd,df.wq)
## get common y.lims for flow axis
y.max <- 10^ceiling(log10(max(df.data$flow)))
y.min <- 10^floor(log10(min(df.data$flow)))
png(filename = paste0(dir.hydro,"/",png.fn,".png"), width = 11, height = 8.5,units = "in",res=300,bg = "white")
plot.fdc <- ggplot(data=df.data) + 
  scale_y_log10(breaks = trans_breaks("log10",function(x) 10^x)(c(y.min,y.max)),
                labels = trans_format("log10", math_format(10^.x))) +
  xlim(0,100) + ylab("Flow (cfs)") + xlab("Flow Exceedance (%)") +
  geom_line(stat="identity",aes(x=flow.exceed,y=flow,colour=run)) +
  scale_colour_manual(values=c("red","blue"))
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
y.max <- -25
y.min <- -100
png(filename = paste0(dir.hydro,"/diff_",png.fn,"_zm.png"), width = 11, height = 8.5,units = "in",res=300,bg = "white")
plot.fdc.diff.zm <- ggplot(data=df.diff) + ylim(y.min,y.max) +
  xlim(0,100) + ylab("% Difference in Flow (100*(wq-hyd)/hyd)") + xlab("Flow Exceedance (%)") +
  geom_line(stat="identity",aes(x=flow.exceed,y=diff))
plot(plot.fdc.diff.zm)
dev.off()
##
## the flows don't match. Check the figures in fdcs01.png, diff_fdcs01.png, and diff_fdcs01_zm.png
##
## clean up
rm(list=c(ls(pattern="^fl\\.*"),ls(pattern="^chr\\.*"),ls(pattern="^df\\.*"),ls(pattern="^plot\\.*"),ls(pattern="^y\\.*"),ls(pattern="^png\\.*")))
##
## Run 02
## the hyd flows are greater than the wq flows. I found the following mult factor in the 
## SCHEMATIC of the wq UCI
## RCHRES   7                  1.00E-04       RCHRES   9      3
## this is a remenant of some of the parameter estimation runs I was doing
## I removed the facor and re-run the wq model for Run 02
##
## graph file filename part for Run 02
png.fn <- "fdcs02"
##
## hyd calibration output
fl.hyd <- "beflhyd.out"
chr.hyd <- scan(paste0(dir.hydro,"/",fl.hyd),what="character",skip=26,sep="\n")
##
## modified wq
##
fl.wq <- "beflsum.out"
chr.wq <- scan(paste0(dir.hydro,"/",fl.wq),what="character",skip=26,sep="\n")
## 
## create data frames
## 
## hyd
df.hyd <- data.frame(run="hyd",date=strptime(substr(chr.hyd,start=7,stop=16),format="%Y %m %d"),
                     flow=as.numeric(substr(chr.hyd,start=23,stop=37))*(1/cf.100ml)*(cf.ac)*(1/cf.s),
                     stringsAsFactors=TRUE)
##
## wq
df.wq <- data.frame(run="wq",date=strptime(substr(chr.wq,start=7,stop=16),format="%Y %m %d"),
                    flow=as.numeric(substr(chr.wq,start=23,stop=37))*(1/cf.100ml)*(cf.ac)*(1/cf.s),
                    stringsAsFactors=TRUE)
##
## calculate flow duration curves
## add to the existing data frames
##
## hyd
df.hyd <- cbind(df.hyd,flow.exceed=100 - 100*rank(df.hyd$flow, na.last="keep",
                                                  ties.method="average") / length(df.hyd$flow))
df.hyd <- df.hyd[ order(df.hyd$flow.exceed), ]
row.names(df.hyd) <- NULL
## wq
df.wq <- cbind(df.wq,flow.exceed=100 - 100*rank(df.wq$flow, na.last="keep",
                                                ties.method="average") / length(df.wq$flow))
df.wq <- df.wq[ order(df.wq$flow.exceed), ]
row.names(df.wq) <- NULL
##
## plot FDCs
## create long format data frame with "run" variable as a factor  having values of "hyd" or "wq"
df.data <- rbind(df.hyd,df.wq)
## get common y.lims for flow axis
y.max <- 10^ceiling(log10(max(df.data$flow)))
y.min <- 10^floor(log10(min(df.data$flow)))
png(filename = paste0(dir.hydro,"/",png.fn,".png"), width = 11, height = 8.5,units = "in",res=300,bg = "white")
plot.fdc <- ggplot(data=df.data) + 
  scale_y_log10(breaks = trans_breaks("log10",function(x) 10^x)(c(y.min,y.max)),
                labels = trans_format("log10", math_format(10^.x))) +
  xlim(0,100) + ylab("Flow (cfs)") + xlab("Flow Exceedance (%)") +
  geom_line(stat="identity",aes(x=flow.exceed,y=flow,colour=run)) +
  scale_colour_manual(values=c("red","blue"))
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
y.max <- 25
y.min <- -50
png(filename = paste0(dir.hydro,"/diff_",png.fn,"_zm.png"), width = 11, height = 8.5,units = "in",res=300,bg = "white")
plot.fdc.diff.zm <- ggplot(data=df.diff) + ylim(y.min,y.max) +
  xlim(0,100) + ylab("% Difference in Flow (100*(wq-hyd)/hyd)") + xlab("Flow Exceedance (%)") +
  geom_line(stat="identity",aes(x=flow.exceed,y=diff))
plot(plot.fdc.diff.zm)
dev.off()
##
## the flows don't match, but are much closer. Check the figures in fdcs02.png, diff_fdcs02.png, and diff_fdcs00_zm.png
##
## clean up
rm(list=c(ls(pattern="^fl\\.*"),ls(pattern="^chr\\.*"),ls(pattern="^df\\.*"),ls(pattern="^plot\\.*"),ls(pattern="^y\\.*"),ls(pattern="^png\\.*")))

##
## Run 03
## the hyd flows and wq flows are closer. I compared the sup files and found
## multiple differences between the hyd and wq. I changed the wq values in the sup file to those
## in the hyd sup file
## graph file filename part for Run 03
png.fn <- "fdcs03"
##
## hyd calibration output
fl.hyd <- "beflhyd.out"
chr.hyd <- scan(paste0(dir.hydro,"/",fl.hyd),what="character",skip=26,sep="\n")
##
## modified wq
##
fl.wq <- "beflsum.out"
chr.wq <- scan(paste0(dir.hydro,"/",fl.wq),what="character",skip=26,sep="\n")
## 
## create data frames
## 
## hyd
df.hyd <- data.frame(run="hyd",date=strptime(substr(chr.hyd,start=7,stop=16),format="%Y %m %d"),
                     flow=as.numeric(substr(chr.hyd,start=23,stop=37))*(1/cf.100ml)*(cf.ac)*(1/cf.s),
                     stringsAsFactors=TRUE)
##
## wq
df.wq <- data.frame(run="wq",date=strptime(substr(chr.wq,start=7,stop=16),format="%Y %m %d"),
                    flow=as.numeric(substr(chr.wq,start=23,stop=37))*(1/cf.100ml)*(cf.ac)*(1/cf.s),
                    stringsAsFactors=TRUE)
##
## calculate flow duration curves
## add to the existing data frames
##
## hyd
df.hyd <- cbind(df.hyd,flow.exceed=100 - 100*rank(df.hyd$flow, na.last="keep",
                                                  ties.method="average") / length(df.hyd$flow))
df.hyd <- df.hyd[ order(df.hyd$flow.exceed), ]
row.names(df.hyd) <- NULL
## wq
df.wq <- cbind(df.wq,flow.exceed=100 - 100*rank(df.wq$flow, na.last="keep",
                                                ties.method="average") / length(df.wq$flow))
df.wq <- df.wq[ order(df.wq$flow.exceed), ]
row.names(df.wq) <- NULL
##
## plot FDCs
## create long format data frame with "run" variable as a factor  having values of "hyd" or "wq"
df.data <- rbind(df.hyd,df.wq)
## get common y.lims for flow axis
y.max <- 10^ceiling(log10(max(df.data$flow)))
y.min <- 10^floor(log10(min(df.data$flow)))
png(filename = paste0(dir.hydro,"/",png.fn,".png"), width = 11, height = 8.5,units = "in",res=300,bg = "white")
plot.fdc <- ggplot(data=df.data) + 
  scale_y_log10(breaks = trans_breaks("log10",function(x) 10^x)(c(y.min,y.max)),
                labels = trans_format("log10", math_format(10^.x))) +
  xlim(0,100) + ylab("Flow (cfs)") + xlab("Flow Exceedance (%)") +
  geom_line(stat="identity",aes(x=flow.exceed,y=flow,colour=run)) +
  scale_colour_manual(values=c("red","blue"))
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
y.max <- 25
y.min <- -50
png(filename = paste0(dir.hydro,"/diff_",png.fn,"_zm.png"), width = 11, height = 8.5,units = "in",res=300,bg = "white")
plot.fdc.diff.zm <- ggplot(data=df.diff) + ylim(y.min,y.max) +
  xlim(0,100) + ylab("% Difference in Flow (wq-hyd)/hyd)") + xlab("Flow Exceedance (%)") +
  geom_line(stat="identity",aes(x=flow.exceed,y=diff))
plot(plot.fdc.diff.zm)
dev.off()
##
## the flows don't match, but are a little closer. Check the figures in fdcs03.png, diff_fdcs03.png, and diff_fdcs03_zm.png
##
## clean up
rm(list=c(ls(pattern="^fl\\.*"),ls(pattern="^chr\\.*"),ls(pattern="^df\\.*"),ls(pattern="^plot\\.*"),ls(pattern="^y\\.*"),ls(pattern="^png\\.*")))
##
## Run 04
## the hyd flows and wq flows are closer. I checked the UCI files and found the values
## not changed are the same in each UCI file. I checked the areas of the sub wtds and 
## the match. The one peculiarity is that PREC is send to PLS 222 twice in the hyd uci.
## I am removing the second application of PREC to PLS 222 and re-running the hyd uci
## graph file filename part for Run 04
png.fn <- "fdcs04"
##
## hyd calibration output
fl.hyd <- "beflhyd.out"
chr.hyd <- scan(paste0(dir.hydro,"/",fl.hyd),what="character",skip=26,sep="\n")
##
## modified wq
##
fl.wq <- "beflsum.out"
chr.wq <- scan(paste0(dir.hydro,"/",fl.wq),what="character",skip=26,sep="\n")
## 
## create data frames
## 
## hyd
df.hyd <- data.frame(run="hyd",date=strptime(substr(chr.hyd,start=7,stop=16),format="%Y %m %d"),
                     flow=as.numeric(substr(chr.hyd,start=23,stop=37))*(1/cf.100ml)*(cf.ac)*(1/cf.s),
                     stringsAsFactors=TRUE)
##
## wq
df.wq <- data.frame(run="wq",date=strptime(substr(chr.wq,start=7,stop=16),format="%Y %m %d"),
                    flow=as.numeric(substr(chr.wq,start=23,stop=37))*(1/cf.100ml)*(cf.ac)*(1/cf.s),
                    stringsAsFactors=TRUE)
##
## calculate flow duration curves
## add to the existing data frames
##
## hyd
df.hyd <- cbind(df.hyd,flow.exceed=100 - 100*rank(df.hyd$flow, na.last="keep",
                                                  ties.method="average") / length(df.hyd$flow))
df.hyd <- df.hyd[ order(df.hyd$flow.exceed), ]
row.names(df.hyd) <- NULL
## wq
df.wq <- cbind(df.wq,flow.exceed=100 - 100*rank(df.wq$flow, na.last="keep",
                                                ties.method="average") / length(df.wq$flow))
df.wq <- df.wq[ order(df.wq$flow.exceed), ]
row.names(df.wq) <- NULL
##
## plot FDCs
## create long format data frame with "run" variable as a factor  having values of "hyd" or "wq"
df.data <- rbind(df.hyd,df.wq)
## get common y.lims for flow axis
y.max <- 10^ceiling(log10(max(df.data$flow)))
y.min <- 10^floor(log10(min(df.data$flow)))
png(filename = paste0(dir.hydro,"/",png.fn,".png"), width = 11, height = 8.5,units = "in",res=300,bg = "white")
plot.fdc <- ggplot(data=df.data) + 
  scale_y_log10(breaks = trans_breaks("log10",function(x) 10^x)(c(y.min,y.max)),
                labels = trans_format("log10", math_format(10^.x))) +
  xlim(0,100) + ylab("Flow (cfs)") + xlab("Flow Exceedance (%)") +
  geom_line(stat="identity",aes(x=flow.exceed,y=flow,colour=run)) +
  scale_colour_manual(values=c("red","blue"))
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
y.max <- 25
y.min <- -50
png(filename = paste0(dir.hydro,"/diff_",png.fn,"_zm.png"), width = 11, height = 8.5,units = "in",res=300,bg = "white")
plot.fdc.diff.zm <- ggplot(data=df.diff) + ylim(y.min,y.max) +
  xlim(0,100) + ylab("% Difference in Flow (wq-hyd)/hyd)") + xlab("Flow Exceedance (%)") +
  geom_line(stat="identity",aes(x=flow.exceed,y=diff))
plot(plot.fdc.diff.zm)
dev.off()
##
## the flows don't match, but are a little closer than run 03. Check the figures in fdcs04.png, diff_fdcs04.png, and diff_fdcs04_zm.png
##
## clean up
rm(list=c(ls(pattern="^fl\\.*"),ls(pattern="^chr\\.*"),ls(pattern="^df\\.*"),ls(pattern="^plot\\.*"),ls(pattern="^y\\.*"),ls(pattern="^png\\.*")))
##
## Run 05
## the hyd flows and wq flows are closer. The reduction in the differences was mainn 
## in lower flows run 04. The problem seemd to be more higher flows and less lower flows
## for the wq run compared to the hyd. For the wq file, I combied the FLD and FLW because
## limitations in the PLS numbering. I used the Dry PREC for the combined FL PLSs. For 
## Run 05, I changed the PREC to Wet.
## graph file filename part for Run 05
png.fn <- "fdcs05"
##
## hyd calibration output
fl.hyd <- "beflhyd.out"
chr.hyd <- scan(paste0(dir.hydro,"/",fl.hyd),what="character",skip=26,sep="\n")
##
## modified wq
##
fl.wq <- "beflsum.out"
chr.wq <- scan(paste0(dir.hydro,"/",fl.wq),what="character",skip=26,sep="\n")
## 
## create data frames
## 
## hyd
df.hyd <- data.frame(run="hyd",date=strptime(substr(chr.hyd,start=7,stop=16),format="%Y %m %d"),
                     flow=as.numeric(substr(chr.hyd,start=23,stop=37))*(1/cf.100ml)*(cf.ac)*(1/cf.s),
                     stringsAsFactors=TRUE)
##
## wq
df.wq <- data.frame(run="wq",date=strptime(substr(chr.wq,start=7,stop=16),format="%Y %m %d"),
                    flow=as.numeric(substr(chr.wq,start=23,stop=37))*(1/cf.100ml)*(cf.ac)*(1/cf.s),
                    stringsAsFactors=TRUE)
##
## calculate flow duration curves
## add to the existing data frames
##
## hyd
df.hyd <- cbind(df.hyd,flow.exceed=100 - 100*rank(df.hyd$flow, na.last="keep",
                                                  ties.method="average") / length(df.hyd$flow))
df.hyd <- df.hyd[ order(df.hyd$flow.exceed), ]
row.names(df.hyd) <- NULL
## wq
df.wq <- cbind(df.wq,flow.exceed=100 - 100*rank(df.wq$flow, na.last="keep",
                                                ties.method="average") / length(df.wq$flow))
df.wq <- df.wq[ order(df.wq$flow.exceed), ]
row.names(df.wq) <- NULL
##
## plot FDCs
## create long format data frame with "run" variable as a factor  having values of "hyd" or "wq"
df.data <- rbind(df.hyd,df.wq)
## get common y.lims for flow axis
y.max <- 10^ceiling(log10(max(df.data$flow)))
y.min <- 10^floor(log10(min(df.data$flow)))
png(filename = paste0(dir.hydro,"/",png.fn,".png"), width = 11, height = 8.5,units = "in",res=300,bg = "white")
plot.fdc <- ggplot(data=df.data) + 
  scale_y_log10(breaks = trans_breaks("log10",function(x) 10^x)(c(y.min,y.max)),
                labels = trans_format("log10", math_format(10^.x))) +
  xlim(0,100) + ylab("Flow (cfs)") + xlab("Flow Exceedance (%)") +
  geom_line(stat="identity",aes(x=flow.exceed,y=flow,colour=run)) +
  scale_colour_manual(values=c("red","blue"))
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
y.max <- 25
y.min <- -50
png(filename = paste0(dir.hydro,"/diff_",png.fn,"_zm.png"), width = 11, height = 8.5,units = "in",res=300,bg = "white")
plot.fdc.diff.zm <- ggplot(data=df.diff) + ylim(y.min,y.max) +
  xlim(0,100) + ylab("% Difference in Flow (wq-hyd)/hyd)") + xlab("Flow Exceedance (%)") +
  geom_line(stat="identity",aes(x=flow.exceed,y=diff))
plot(plot.fdc.diff.zm)
dev.off()
##
## the flows don't match, but are a little closer than run 04. The flows for the wq are still
## larger for high flows and smaller for low flows. 
## Check the figures in fdcs05.png, diff_fdcs05.png, and diff_fdcs05_zm.png
##
## clean up
rm(list=c(ls(pattern="^fl\\.*"),ls(pattern="^chr\\.*"),ls(pattern="^df\\.*"),ls(pattern="^plot\\.*"),ls(pattern="^y\\.*"),ls(pattern="^png\\.*")))
##
## Run 06
## the hyd flows and wq flows are closer. The reduction in the differences was mainn 
## in lower flows run 05. The problem seemd to be more higher flows and less lower flows
## for the wq run compared to the hyd. I found a difference in the wq file for the FOREST IRC. 
## The value for the wq was higher which could cause the shft to higher flows.
## graph file filename part for Run 06
png.fn <- "fdcs06"
##
## hyd calibration output
fl.hyd <- "beflhyd.out"
chr.hyd <- scan(paste0(dir.hydro,"/",fl.hyd),what="character",skip=26,sep="\n")
##
## modified wq
##
fl.wq <- "beflsum.out"
chr.wq <- scan(paste0(dir.hydro,"/",fl.wq),what="character",skip=26,sep="\n")
## 
## create data frames
## 
## hyd
df.hyd <- data.frame(run="hyd",date=strptime(substr(chr.hyd,start=7,stop=16),format="%Y %m %d"),
                     flow=as.numeric(substr(chr.hyd,start=23,stop=37))*(1/cf.100ml)*(cf.ac)*(1/cf.s),
                     stringsAsFactors=TRUE)
##
## wq
df.wq <- data.frame(run="wq",date=strptime(substr(chr.wq,start=7,stop=16),format="%Y %m %d"),
                    flow=as.numeric(substr(chr.wq,start=23,stop=37))*(1/cf.100ml)*(cf.ac)*(1/cf.s),
                    stringsAsFactors=TRUE)
##
## calculate flow duration curves
## add to the existing data frames
##
## hyd
df.hyd <- cbind(df.hyd,flow.exceed=100 - 100*rank(df.hyd$flow, na.last="keep",
                                                  ties.method="average") / length(df.hyd$flow))
df.hyd <- df.hyd[ order(df.hyd$flow.exceed), ]
row.names(df.hyd) <- NULL
## wq
df.wq <- cbind(df.wq,flow.exceed=100 - 100*rank(df.wq$flow, na.last="keep",
                                                ties.method="average") / length(df.wq$flow))
df.wq <- df.wq[ order(df.wq$flow.exceed), ]
row.names(df.wq) <- NULL
##
## plot FDCs
## create long format data frame with "run" variable as a factor  having values of "hyd" or "wq"
df.data <- rbind(df.hyd,df.wq)
## get common y.lims for flow axis
y.max <- 10^ceiling(log10(max(df.data$flow)))
y.min <- 10^floor(log10(min(df.data$flow)))
png(filename = paste0(dir.hydro,"/",png.fn,".png"), width = 11, height = 8.5,units = "in",res=300,bg = "white")
plot.fdc <- ggplot(data=df.data) + 
  scale_y_log10(breaks = trans_breaks("log10",function(x) 10^x)(c(y.min,y.max)),
                labels = trans_format("log10", math_format(10^.x))) +
  xlim(0,100) + ylab("Flow (cfs)") + xlab("Flow Exceedance (%)") +
  geom_line(stat="identity",aes(x=flow.exceed,y=flow,colour=run)) +
  scale_colour_manual(values=c("red","blue"))
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
y.max <- 25
y.min <- -50
png(filename = paste0(dir.hydro,"/diff_",png.fn,"_zm.png"), width = 11, height = 8.5,units = "in",res=300,bg = "white")
plot.fdc.diff.zm <- ggplot(data=df.diff) + ylim(y.min,y.max) +
  xlim(0,100) + ylab("% Difference in Flow (wq-hyd)/hyd)") + xlab("Flow Exceedance (%)") +
  geom_line(stat="identity",aes(x=flow.exceed,y=diff))
plot(plot.fdc.diff.zm)
dev.off()
##
## the flows don't match, but are a little closer than run 04. The flows for the wq are still
## larger for high flows and smaller for low flows. 
## Check the figures in fdcs06.png, diff_fdcs06.png, and diff_fdcs06_zm.png
##
## clean up
rm(list=c(ls(pattern="^fl\\.*"),ls(pattern="^chr\\.*"),ls(pattern="^df\\.*"),ls(pattern="^plot\\.*"),ls(pattern="^y\\.*"),ls(pattern="^png\\.*")))
