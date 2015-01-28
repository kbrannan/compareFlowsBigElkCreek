library(ggplot2)

## create df that has the error for pero, agwo, ifwo, and suro
tmp.data <- df.data[with(df.data,order(run,date)),]
tmp.hyd <- tmp.data[tmp.data$run == "hyd",]
tmp.wq <- tmp.data[tmp.data$run == "wq",]
tmp.err <- data.frame(date=tmp.hyd$date,
                      pero=tmp.hyd$pero-tmp.wq$pero,agwo=tmp.hyd$agwo-tmp.wq$agwo,
                      ifwo=tmp.hyd$ifwo-tmp.wq$ifwo,suro=tmp.hyd$suro-tmp.wq$suro)
tmp.csum <- data.frame(date=tmp.err$date, pero=cumsum(tmp.err$pero),agwo=cumsum(tmp.err$agwo),
                       ifwo=cumsum(tmp.err$agwo),suro=cumsum(tmp.err$suro))
tmp.p1 <- ggplot(data=tmp.csum) + geom_line(aes(x=date,y=pero))
tmp.p2 <- ggplot(data=tmp.csum) + geom_line(aes(x=date,y=agwo))
tmp.p3 <- ggplot(data=tmp.csum) + geom_line(aes(x=date,y=ifwo))
tmp.p4 <- ggplot(data=tmp.csum) + geom_line(aes(x=date,y=suro))

multiplot(tmp.p1,tmp.p2,tmp.p3,tmp.p4,cols=2)
