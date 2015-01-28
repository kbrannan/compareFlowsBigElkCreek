## compare areas
## packages
library(doBy)
## work directories
dir.hydro <- "e:/pest/bigelkpest/hydro"
## uci files
fl.hyd <- "bigelk.uci"
fl.wq <- "bigelkwq.uci"
## read files
chr.hyd <- scan(file=paste0(dir.hydro,"/",fl.hyd),what="character",sep="\n")
chr.wq <- scan(file=paste0(dir.hydro,"/",fl.wq),what="character",sep="\n")
## get areas
## hyd
## get perland info
perlnd.st <- min(grep("^PERLND",chr.hyd))
perlnd.ed <- min(grep("^END PERLND",chr.hyd))
perlnd.chr <- chr.hyd[perlnd.st:perlnd.ed]
gi.st <- grep("^ {1,}GEN-INFO",perlnd.chr) + 1
gi.ed <- grep("^ {1,}END GEN-INFO",perlnd.chr) - 1
gi.chr <- perlnd.chr[gi.st:gi.ed]
gi.chr <- gi.chr[-grep("\\*{1,}",gi.chr)]
## Get pls nums and names use fixed format to get info
tmp.pls00 <- data.frame(ls="PERLND",num=gsub(" ","",substr(gi.chr,start=1,stop=5)),name=gsub("^ {1,} ","",substr(gi.chr,start=11,stop=30)), stringsAsFactors=FALSE)
## clean up
rm(list=ls(pattern="^gi\\.."))
## get impland info
implnd.st <- min(grep("^IMPLND",chr.hyd))
implnd.ed <- min(grep("^END IMPLND",chr.hyd))
implnd.chr <- chr.hyd[implnd.st:implnd.ed]
gi.st <- grep("^ {1,}GEN-INFO",implnd.chr) + 1
gi.ed <- grep("^ {1,}END GEN-INFO",implnd.chr) - 1
gi.chr <- implnd.chr[gi.st:gi.ed]
gi.chr <- gi.chr[-grep("\\*{1,}",gi.chr)]
## Get pls nums and names use fixed format to get info
tmp.ils00 <- data.frame(ls="IMPLND",num=gsub(" ","",substr(gi.chr,start=1,stop=5)),name=gsub("^ {1,} ","",substr(gi.chr,start=11,stop=30)), stringsAsFactors=FALSE)
## clean up
rm(list=ls(pattern="^gi\\.."))
## create list of land segments
df.ls <- cbind(rbind(tmp.pls00,tmp.ils00),stringsAsFactors=FALSE)
## clean up
rm(list=c(ls(pattern="^perlnd\\.."),ls(pattern="^implnd\\.."),ls(pattern="^tmp\\..")))
## parse SCHEMATIC block to get areas
sch.st <- grep("^SCHEMATIC",chr.hyd)
sch.ed <- grep("^END SCHEMATIC",chr.hyd)
sch.chr <- chr.hyd[sch.st:sch.ed]
tmp.sub <- gsub("[^0-9]{1,}","",sch.chr[grep("^\\*{3}.Reach.[0-9]{1,}$",sch.chr)])
tmp.st <- grep("^\\*{3}.Reach.[0-9]{1,}$",sch.chr)
tmp.ed <- c(tmp.st[-1]-1,length(sch.chr) -1)
for(ii in 1:length(tmp.sub)) {
  tmp.ck <- sch.chr[(tmp.st[ii]+1):tmp.ed[ii]]
  tmp.df <- data.frame(ls=gsub(" ","",substr(tmp.ck,start=1,stop=6)),num=gsub(" ","",substr(tmp.ck,start=7,stop=10)),area=as.numeric(gsub(" ","",substr(tmp.ck,start=29,stop=38))), stringsAsFactors=FALSE)
  if(! (exists("df.areas") && is.data.frame(df.areas))) {
    df.areas <- cbind(src="hyd",sub=tmp.sub[ii],merge(df.ls,tmp.df))
  } else {
    df.areas <- rbind(df.areas,cbind(src="hyd",sub=tmp.sub[ii],merge(df.ls,tmp.df)))
  }
  rm(tmp.ck,tmp.df)
}
rm(ii)
rm(list=c(ls(pattern="^tmp\\.."),ls(pattern="^sch\\..")))
##
## wq
## get perland info
perlnd.st <- min(grep("^PERLND",chr.wq))
perlnd.ed <- min(grep("^END PERLND",chr.wq))
perlnd.chr <- chr.wq[perlnd.st:perlnd.ed]
gi.st <- grep("^ {1,}GEN-INFO",perlnd.chr) + 1
gi.ed <- grep("^ {1,}END GEN-INFO",perlnd.chr) - 1
gi.chr <- perlnd.chr[gi.st:gi.ed]
gi.chr <- gi.chr[-grep("\\*{1,}",gi.chr)]
## Get pls nums and names use fixed format to get info
tmp.pls00 <- data.frame(ls="PERLND",num=gsub(" ","",substr(gi.chr,start=1,stop=5)),name=gsub("^ {1,} ","",substr(gi.chr,start=11,stop=30)), stringsAsFactors=FALSE)
##tmp.pls00$name <- gsub("^Sub[0-9]{2}","",tmp.pls00$name)
tmp.pls00$name <- gsub(" [0-1]*","",gsub(".*Sub [0-9]{2} ","",gi.chr))
## clean up
rm(list=ls(pattern="^gi\\.."))
## get impland info
implnd.st <- min(grep("^IMPLND",chr.wq))
implnd.ed <- min(grep("^END IMPLND",chr.wq))
implnd.chr <- chr.wq[implnd.st:implnd.ed]
gi.st <- grep("^ {1,}GEN-INFO",implnd.chr) + 1
gi.ed <- grep("^ {1,}END GEN-INFO",implnd.chr) - 1
gi.chr <- implnd.chr[gi.st:gi.ed]
gi.chr <- gi.chr[-grep("\\*{1,}",gi.chr)]
## Get pls nums and names use fixed format to get info
tmp.ils00 <- data.frame(ls="IMPLND",num=gsub(" ","",substr(gi.chr,start=1,stop=5)),name=gsub("^ {1,} ","",substr(gi.chr,start=11,stop=30)), stringsAsFactors=FALSE)
tmp.ils00$name <- gsub("^Sub[0-9]{2}","",tmp.ils00$name)
## clean up
rm(list=ls(pattern="^gi\\.."))
## create list of land segments
df.ls <- cbind(rbind(tmp.pls00,tmp.ils00),stringsAsFactors=FALSE)
## clean up
rm(list=c(ls(pattern="^perlnd\\.."),ls(pattern="^implnd\\.."),ls(pattern="^tmp\\..")))
## parse SCHEMATIC block to get areas
sch.st <- grep("^SCHEMATIC",chr.wq)
sch.ed <- grep("^END SCHEMATIC",chr.wq)
sch.chr <- chr.wq[sch.st:sch.ed]
tmp.sub <- gsub("[^0-9]{1,}","",sch.chr[grep("^\\*{3}.Reach.[0-9]{1,}$",sch.chr)])
tmp.st <- grep("^\\*{3}.Reach.[0-9]{1,}$",sch.chr)
tmp.ed <- c(tmp.st[-1]-1,length(sch.chr) -1)
for(ii in 1:length(tmp.sub)) {
  tmp.ck <- sch.chr[(tmp.st[ii]+1):tmp.ed[ii]]
  tmp.df <- data.frame(ls=gsub(" ","",substr(tmp.ck,start=1,stop=6)),num=gsub(" ","",substr(tmp.ck,start=7,stop=10)),area=as.numeric(gsub(" ","",substr(tmp.ck,start=29,stop=38))), stringsAsFactors=FALSE)
  if(! (exists("df.areas") && is.data.frame(df.areas))) {
    df.areas <- cbind(src="wq",sub=tmp.sub[ii],merge(df.ls,tmp.df))
  } else {
    df.areas <- rbind(df.areas,cbind(src="wq",sub=tmp.sub[ii],merge(df.ls,tmp.df)))
  }
  rm(tmp.ck,tmp.df)
}
rm(ii,df.ls)
rm(list=c(ls(pattern="^tmp\\.."),ls(pattern="^sch\\..")))
df.areas$name <- gsub(" ","",df.areas$name)
##
## summaries
## areas by general PLS category
tmp.name <- merge(summaryBy(area ~ name+src, data=df.areas[with(df.areas, src=="hyd" & ls=="PERLND"),],FUN=sum),summaryBy(area ~ name+src, data=df.areas[with(df.areas, src=="wq" & ls=="PERLND"),],FUN=sum),by="name")
tmp.name <- cbind(tmp.name,diff=tmp.name$area.sum.y-tmp.name$area.sum.x,per.diff=((tmp.name$area.sum.y-tmp.name$area.sum.x)/tmp.name$area.sum.x)*100)
## areas by general PLS category
tmp.sub <- merge(summaryBy(area ~ sub+src, data=df.areas[with(df.areas, src=="hyd" & ls=="PERLND"),],FUN=sum),tmp.wq  <- summaryBy(area ~ sub+src, data=df.areas[with(df.areas, src=="wq" & ls=="PERLND"),],FUN=sum),by="sub")
tmp.sub$sub <- as.numeric(tmp.sub$sub)
tmp.sub <- tmp.sub[order(tmp.sub$sub),]
tmp.sub <- cbind(tmp.sub,diff=tmp.sub$area.sum.y-tmp.sub$area.sum.x,per.diff=((tmp.sub$area.sum.y-tmp.sub$area.sum.x)/tmp.sub$area.sum.x)*100)

tmp.name
tmp.sub
