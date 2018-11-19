# initialize environment
rm(list = ls()); sapply(X = 1:9, FUN = gc)
memory.size(max = T)
# memory.limit()
memory.limit(size=56000)

# config
SET_REPOSITORY_PATH <- "C:/Users/yuta.sonoda/Desktop/Programming/R"
setwd(SET_RESPOSITORY_PATH)
DATA_2D_PATH <- "./../data/kde2d_sample_data_01.csv"
DATA_3D_PATH <- "./../data/kde3d_sample_data_01.csv"


# confirm and install package
if(!require("misc3d")){
  install.packages("misc3d")
}

if(!require("rgl")){
  install.packages("rgl")
}

# read package
library(MASS)
library(misc3d)
library(rgl)

############################ In case of 2D data ############################
# read and process test data
rowdata_kde2d <- read.table(DATA_2D_PATH, sep=",", header=T)
ncol_2d <- ncol(rowdata_kde2d)
nrow_2d <- nrow(rowdata_kde2d)

vx_2d = c()
vy_2d = c()

for(i in 1:nrow_2d) {
  for(j in 1:rowdata_kde2d$s[i]) {
    vx_2d=c(vx_2d, rowdata_kde2d$x[i])
    vy_2d=c(vy_2d, rowdata_kde2d$y[i])
  }
}

dat4kde2d <- matrix(c(vx_2d,vy_2d),ncol=2)

# plot data
plot(dat4kde2d[,1],
     dat4kde2d[,2],
     xlab="x", 
     ylab="y", 
     main="Scatter plot")

# plot
kd1 <- kde2d(dat4kde2d[,1], 
             dat4kde2d[,2], 
             c(bandwidth.nrd(dat4kde2d[,1]),
               bandwidth.nrd(dat4kde2d[,2])), 
             n=200)
image(kd1, xlab="x", ylab="y", main="Kernel Density")
contour(kd1, add=TRUE, col=1)

############################ In case of 3D data ############################

# read and process test data
rowdata_kde3d <- read.table(DATA_3D_PATH, sep=",", header=T)
ncol_3d <- ncol(rowdata_kde3d)
nrow_3d <- nrow(rowdata_kde3d)

vx_3d = c()
vy_3d = c()
vz_3d = c()

for(i in 1:nrow_3d) {
  for(j in 1:rowdata_kde3d$s[i]) {
    vx_3d=c(vx_3d, rowdata_kde3d$x[i])
    vy_3d=c(vy_3d, rowdata_kde3d$y[i])
    vz_3d=c(vz_3d, rowdata_kde3d$z[i])
  }
}

dat4kde3d <- matrix(c(vx_3d,vy_3d,vz_3d),ncol=3)

# plot
kd2 <- kde3d(dat4kde3d[,1], 
             dat4kde3d[,2],
             dat4kde3d[,3],
             c(bandwidth.nrd(dat4kde3d[,1]), 
               bandwidth.nrd(dat4kde3d[,2]),
               bandwidth.nrd(dat4kde3d[,3])), 
             n=50)
# plot data
plot3d(dat4kde3d[,1],
       dat4kde3d[,2],
       dat4kde3d[,3],
       col="blue", 
       xlab="x",
       ylab="y",
       zlab="z")
image3d(kd2$d,kd2$x,kd2$y,kd2$z,add=TRUE,vlim=c(0,0.011))