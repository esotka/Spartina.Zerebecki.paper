## PCA plots of each popn (short vs tall)
### and histograms of k=2 for Short vs tall.

rm(list=ls())
pdf('output/shortVStall-pca&admix_allDATA.pdf',width=12,height=12)
par(mfrow=c(3,4))
library(RColorBrewer)

### PCA
meta <- read.csv('data/Spartina_SNP_SiteID.csv')
gprob<- read.table('data/spartinaNov2017.called.subset.mpgl')
ids <- read.csv('data/inds/allpops_individualIDs_subset.csv')[,2]
pop <- substr(ids,1,3)
reg <- meta$State[match(pop,meta$Site_ID)]

gmat <- gprob[,-1]
colnames(gmat) <- ids

sh <- c("SWS","WES","RIS","SBI","SFB","FLS")#,"FJS")
ta <- c("SWT","WET","RIT","TBI","TFB","FLT")#,"SCT")
site <- c("Sweeney, MA","West Marsh, MA","Cole State Park, RI",
          "Bowens Island, SC", "Folly Beach, SC","St. Theresa, FL")
a <- meta$State[match(ta,meta$Site_ID)]

siteNICE.sh <- c("MASS","MAWS","RIS","SCBS","SCFS","FLS")
siteNICE.ta <- c("MAST","MAWT","RIT","SCBT","SCFT","FLT")

### histogram of k=2
k=c(2,3,2,2,2,2)
f <- c("k02run01_FLSFLT.qopt",
       "k02run01_SFBTFB.qopt",
       "k02run01_SBITBI.qopt",
       "k02run01_RISRIT.qopt",
       "k03run01_WESWET.qopt", ### WESWET best k is 3
       "k02run01_SWSSWT.qopt")
f <- f[6:1] # north to south
fid <- c("inds.FL","inds.SFBTFB","inds.SBITBI","inds.RISRIT","inds.WESWET","inds.SWSSWT")
fid <- fid[6:1] # north to south
site <- c("Sweeney, MA","West Marsh, MA","Cole State Park, RI",
          "Bowens Island, SC", "Folly Beach, SC","St. Theresa, FL")
#tim6equal = c("#00008F", "#005AFF", "#23FFDC", "#ECFF13", "#FF4A00", "#800000")
col.order <- list(c(1,2),c(1,2,3),c(1,2),c(2,1),c(2,1),c(1,2))
#all <- c()

for (i in 1:6)
{
  ### PCA
  tmp <- gmat[,pop==sh[i] | pop==ta[i]]
  pop.tmp <- substr(colnames(tmp),1,3); pop.tmp <- factor(pop.tmp)
  pc.cr <- prcomp(t(tmp))
  col.sub <- c("red","black")[pop.tmp]
  print(pop.tmp)
  par(mar=c(5,5,5,2))
  plot(pc.cr$x[,1],pc.cr$x[,2],cex=0,xlab="",ylab="")
  points(pc.cr$x[,1],pc.cr$x[,2],pch=20,cex=2,col=col.sub)
  for (j in 1:2){
    xbar.x <- mean(pc.cr$x[pop.tmp==levels(pop.tmp)[j],1])
    xbar.y <- mean(pc.cr$x[pop.tmp==levels(pop.tmp)[j],2])
    tmp.pc.cr <- pc.cr$x[pop.tmp==levels(pop.tmp)[j],]
    segments(x0 = xbar.x,y0 = xbar.y,x1 = tmp.pc.cr[,1],y1 = tmp.pc.cr[,2],
             col=c("red","black")[j],lwd=.5)
    }
  ### histogram
  tmp <- c()
  print(paste("data/admix.runs.TvS.POPS.byPOP/",f[i],sep=""))
  tmp <- read.delim(paste("data/admix.runs.TvS.POPS.byPOP/",f[i],sep=""),sep=" ",header = F)[,-(k[i]+1)]
  tmpids <- read.delim(paste("data/inds/",fid[i],sep=""),sep=" ",header = F)
  site1 <- substr(as.character(tmpids[,1]),1,3)
  siteNICE <- c(siteNICE.sh[i],siteNICE.ta[i])
  par(mar=c(3,4,3,3))
  if(f[i]=="k03run01_WESWET.qopt")
  {
    barplot(t(tmp),col=c("lightgrey","darkgrey","black"),border=NA,ylim=c(-.1,1.1),names.arg = rep("",dim(tmp)[1]),space = 0,ylab="Proportion",cex.lab=1.2)
  }else{ 
    barplot(t(tmp),col=c("black","darkgrey")[col.order[[i]]],border=NA,ylim=c(-.1,1.1),names.arg = rep("",dim(tmp)[1]),space = 0,ylab="Proportion",cex.lab=1.2)
    }
  ## annotations
  mtext(site[i],at=-3,cex=2)
  x <- 1:dim(tmp)[1]
  x1 <- x[site1==unique(site1)[1]]
  x2 <- x[site1==unique(site1)[2]]
  segments(c(0,max(x1),max(x2)),0,c(0,max(x1),max(x2)),-.3,lwd=1,col="black")
  mtext(side=1,at=mean(x1),siteNICE[1],cex=1,line=-1)
  mtext(side=1,at=mean(x2),siteNICE[2],cex=1,line=-1)
}

dev.off()
