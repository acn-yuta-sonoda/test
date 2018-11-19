# 環境リセット----
rm(list = ls()); sapply(X = 1:9, FUN = gc)
memory.size(max = T)
# memory.limit()
memory.limit(size=56000)
options(java.parameters = "-Xmx1g")
set.seed(1019)

library(rstan)
library(dplyr)

# config
SET_RESPOSITORY_PATH <- "C:/Users/yuta.sonoda/Desktop/Programming/R"
setwd(SET_RESPOSITORY_PATH)

# data import
d <- read.table("./../data/conflict_sample.txt", header=T)
d$cv<-as.numeric(d$cv)-1
dat <- rowdat %>%
  dplyr::mutate(cv=as.numeric(cv)-1)

d.dat<-list(N=dim(d)[1],M=dim(d)[2]-1,X=d[,-8],y=d$cv)

d.fit<-stan(file='d.stan',data=d.dat,iter=1000,chains=4)

d.fit

d.ext<-extract(d.fit,permuted=T)
N.mcmc<-length(d.ext$beta0)

require(ggplot2)
require(reshape2)
require(plyr)

b1<-d.ext$beta[1:2000]
b2<-d.ext$beta[2001:4000]
b3<-d.ext$beta[4001:6000]
bs<-data.frame(b1=b1,b2=b2,b3=b3)
bs.melt<-melt(bs,id=c(),variable.name="param")
bs.qua.melt<-ddply(bs.melt,.(param),summarize,
                   median=median(value),
                   ymax=quantile(value,prob=0.975),
                   ymin=quantile(value,prob=0.025))
colnames(bs.qua.melt)[2]<-"value"

bs.melt<-data.frame(bs.melt,ymax=rep(0,N.mcmc),ymin=rep(0,N.mcmc))
p<-ggplot(bs.melt,aes(x=param,y=value,group=param,ymax=ymax,ymin=ymin,color=param))
p<-p+geom_violin(trim=F,fill="#5B423D",linetype="blank",alpha=I(1/3))
p<-p+geom_pointrange(data=bs.qua.melt,size=0.75)
p<-p+labs(x="",y="")+theme(axis.text.x=element_text(size=14),axis.text.y=element_text(size=14))
ggsave(file="d.png",plot=p,dpi=300,width=4,height=3)



b1<-d.ext$beta[1:2000]
b2<-d.ext$beta[2001:4000]
b3<-d.ext$beta[4001:6000]
b4<-d.ext$beta[6001:8000]
b5<-d.ext$beta[8001:10000]
b6<-d.ext$beta[10001:12000]
b7<-d.ext$beta[12001:14000]
bs2<-data.frame(b1=b1,b2=b2,b3=b3,b4=b4,b5=b5,b6=b6,b7=b7)
bs2.melt<-melt(bs2,id=c(),variable.name="param")
bs2.qua.melt<-ddply(bs2.melt,.(param),summarize,
                    median=median(value),
                    ymax=quantile(value,prob=0.975),
                    ymin=quantile(value,prob=0.025))
colnames(bs2.qua.melt)[2]<-"value"

bs2.melt<-data.frame(bs2.melt,ymax=rep(0,N.mcmc),ymin=rep(0,N.mcmc))
p<-ggplot(bs2.melt,aes(x=param,y=value,group=param,ymax=ymax,ymin=ymin,color=param))
p<-p+geom_violin(trim=F,fill="#5B423D",linetype="blank",alpha=I(1/3))
p<-p+geom_pointrange(data=bs2.qua.melt,size=0.4)
p<-p+labs(x="",y="")+theme(axis.text.x=element_text(size=11),axis.text.y=element_text(size=11))
ggsave(file="d7.png",plot=p,dpi=300,width=4,height=3)

