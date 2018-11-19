# 環境リセット----
rm(list = ls()); sapply(X = 1:9, FUN = gc)
memory.size(max = T)
# memory.limit()
memory.limit(size=56000)

if(!require("mvtnorm")){
  install.packages("mvtnorm")
}

if(!require("matlab")){
  install.packages("matlab")
}

library(mvtnorm)
library(MASS)
library(matlab)

# config
SET_RESPOSITORY_PATH <- "C:/Users/yuta.sonoda/Desktop/Programming/R"
setwd(SET_RESPOSITORY_PATH)

#initialization
x0 <- c(-5,2)
iter <- 1500  
x <- matrix(0, iter, 2)
dratio <- rep(0, iter)
out <- rep(0, iter)
x[1,] <- x0

# correlation of 2-dimentional Normal distribution
rho <- 0.7
sig <- matrix(c(1, rho, rho, 1), 2)
mu <- c(4, 5)
 
# Metropolis-Hestings
for(i in 2:iter){
  walk <- runif(2, -0.5, 0.5)
  v <- x[i-1,] + walk
  dratio[i-1] <- dmvnorm(v, mean = mu, sigma = sig) / dmvnorm(x[i-1,], mean = mu, sigma = sig)
  out[i-1] <- rbinom(1, 1, min(1, dratio[i-1]))
  x[i,] <- x[i-1,] + out[i-1] * walk
}

kd1 <- kde2d(x[,1],
             x[,2],
             c(bandwidth.nrd(x[,1]),
               bandwidth.nrd(x[,2])),
             n=1000)
cols <- jet.colors(iter)
plot(x, type="p", pch=16, cex=0.6, col=2, 
     xlab="x1", ylab="x2", main="Metropolis-Hastings sampling")

for(i in 2:iter){
  segments(x[i-1,1], x[i-1,2], x[i,1], x[i,2], col=cols[i])
}

points(x[1,1], x[1,2], pch="★", col=6)
points(mu[1], mu[2], pch="★", col=5)
contour(kd1, add=TRUE, col=1)

# Gibbs
X <- matrix(x0, nr=1)

sig[-1,1]

for(j in 2:iter){
  x <- X[j-1,]
  for(i in seq(mu)){
    s <- sig[-i, i] %*% solve(sig[-i, -i])   # Σ_ab Σ_bb ^ -1
    # (PRML 2.81) μ_a|b = μ_a + Σ_ab Σ_bb ^ -1 (x_b - μ_b)
    mu_a_b <- mu[i] + s %*% (x[-i] - mu[-i])
    # (PRML 2.82) Σ_a|b = Σ_aa - Σ_ab Σ_bb ^ -1 Σ_ba
    sig_a_b <- sig[i, i] - s %*% sig[i, -i]
    # [Gibbs] x_a ～ p(x_a|x_{-a}) = N(μ_a|b, Σ_a|b)
    x[i] <- rnorm(1, mu_a_b, sqrt(sig_a_b))
  }
  X <- rbind(X, x)
}
colnames(X) <- sprintf("V%d", seq(ncol(X)))

kd2 <- kde2d(X[,1], X[,2], c(bandwidth.nrd(X[,1]), bandwidth.nrd(X[,2])), n=1000)
cols <- jet.colors(iter)
plot(X, type="p", pch=16, cex=0.6, col=2, xlab="x1", ylab="x2", main="Gibbs sampling")
#lines(x, col=cols)
for(i in 2:iter){
  for(j in seq(ncol(X))){
    y <- rbind(replace(X[i-1,], 0:(j-1), X[i, 0:(j-1)]), replace(X[i-1,], 1:j, X[i, 1:j]))
    segments(y[1,1], y[1,2], y[2,1], y[2,2], col=cols[i])
  }
}
points(X[1,1], X[1,2], pch="★", col=6)
points(mu[1], mu[2], pch="★", col=5)
contour(kd2, add=TRUE, col=1)
