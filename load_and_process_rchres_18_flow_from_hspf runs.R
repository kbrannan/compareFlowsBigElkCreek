## load and process flow from hspf runs
##
## work directories
lp.dir.hydro <- "e:/pest/bigelkpest/hydro"
##
## conversion factors
lp.cf.s <- 24*60*60 ## conversion factor from day to second
##
## wq output
lp.fl.wq <- "flwqR18.out"
lp.chr.wq <- scan(paste0(lp.dir.hydro,"/",lp.fl.wq),what="character",skip=26,sep="\n")
##
## hyd output
##
lp.fl.hyd <- "flhydR18.out"
lp.chr.hyd <- scan(paste0(lp.dir.hydro,"/",lp.fl.hyd),what="character",skip=26,sep="\n")
## 
## create data frames
##
## wq
lp.df.wq <- data.frame(run="wq",date=strptime(substr(lp.chr.wq,start=7,stop=16),format="%Y %m %d"),
                    flow.cfs=as.numeric(substr(lp.chr.wq,start=23,stop=37)),
                    stringsAsFactors=TRUE)
## hyd
lp.df.hyd <- data.frame(run="hyd",date=strptime(substr(lp.chr.hyd,start=7,stop=16),format="%Y %m %d"),
                     flow.cfs=as.numeric(substr(lp.chr.hyd,start=23,stop=37)),
                     stringsAsFactors=TRUE)
##
## calculate flow duration curves
## add to the existing data frames
##
## wq
lp.df.wq <- cbind(lp.df.wq,
               flow.cfs.exceed=with(lp.df.wq,100 - 100*rank(flow.cfs, na.last="keep",ties.method="average") / length(run))
)
lp.df.wq <- lp.df.wq[ order(lp.df.wq$flow.cfs.exceed), ]
row.names(lp.df.wq) <- NULL
##
## hyd
lp.df.hyd <- cbind(lp.df.hyd,
                flow.cfs.exceed=with(lp.df.hyd,100 - 100*rank(flow.cfs, na.last="keep",ties.method="average") / length(run))
)
lp.df.hyd <- lp.df.hyd[ order(lp.df.hyd$flow.cfs.exceed), ]
row.names(lp.df.hyd) <- NULL
##
## create long format data frame with "run" variable as a factor  having values of "org" or "wq"
df.data <- rbind(lp.df.hyd,lp.df.wq)
rm(list=ls(pattern="^lp."))
