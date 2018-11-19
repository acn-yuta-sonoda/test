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
SET_RESPOSITORY_PATH <- getwd()
setwd(SET_RESPOSITORY_PATH)

DATA_PATH <- "./../data/hb_trend_sample.txt"
STAN_GLM_PATH <- "./05_BayesianStatistics/stan/GeneralLinearModel.stan"

# data import
d <- read.table(DATA_PATH, header=T)
d$cv<-as.numeric(d$cv)-1
dat <- rowdat %>%
  dplyr::mutate(cv=as.numeric(cv)-1)

d.dat<-list(N=dim(d)[1],x=d$x1,y=d$y)

d.dat<-list(N=dim(d)[1],M=dim(d)[2]-1,X=d[,-4],y=d$y)

d.fit<-stan(file=STAN_GLM_PATH,data=d.dat,iter=1000,chains=1)

d.fit
