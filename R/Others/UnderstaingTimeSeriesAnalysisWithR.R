# 環境リセット----
rm(list = ls()); sapply(X = 1:9, FUN = gc)
memory.size(max = T)
# memory.limit()
memory.limit(size=56000)

# config
SET_RESPOSITORY_PATH <- getwd()
setwd(SET_RESPOSITORY_PATH)
SET_CO2_PATH <- "./../data/CO2.csv"

Ryori <- read.csv(SET_CO2_PATH)

y_all <- ts(data = Ryori$CO2, start = c(1987,1), frequency = 12)
y <- window(y_all, end = c(2018, 4))

plot(y)

Nile2 <- ts(Nile, start = 1866)
ts.union(Nile, Nile2)

Nile2 <- 2 * Nile

ts.plot(cbind(Nile, Nile2),
        lty = c("solid", "dashed"),
        main = "Fig.3.1 Plotting two ts class objects")
#####################################
# 3.2 'ts' Object Data Handling
#####################################
# Start point, finish point, period
tsp(Nile)
# time series
time(Nile)

# 'Date' class object
day <- as.Date("2000-01-01")
str(day) # confirm data structure

days <- seq(from = as.Date("2000-01-01"),
            to = as.Date("2000-01-31"),
            by = "day")
days

# extract info of day of the week
weekdays(days)

##################################################################
# 4. INTRODUCTION OF TIME SERIES ANALYSIS
#
##################################################################

### Initialization ###
rm(list = ls()); sapply(X = 1:9, FUN = gc)
memory.size(max = T)
# memory.limit()
memory.limit(size=56000)

# config
SET_RESPOSITORY_PATH <- getwd()
setwd(SET_RESPOSITORY_PATH)
SET_CO2_PATH <- "./../data/CO2.csv"
SET_NON_LINEAR_PATH <- "./00_RData/BenchmarkNonLinearModel.RData"

#######

# Plot various data on the same 
# preprocessing od plot
oldpar <- par(no.readonly = TRUE)
par(mfrow = c(2,2)); par(oma = c(0, 0, 0, 0)); par(mar = c(4, 4, 2, 1))

# (a) annual rate of Nile River
plot(Nile, main = "(a)")

# (b) concentration of CO2 in the atmosphere
Ryori <- read.csv(SET_CO2_PATH)
y_all <- ts(data = Ryori$CO2, start = c(1987, 1), frequency = 12)
y <- window(y_all, end = c(2014, 12))
y_CO2 <- y
plot(y_CO2, main = "(b)")

# (c) consumption of gas in UK
plot(UKgas, main = '(c)')

# (d) nonlinear model data
# Loading 'BenchmarkNonLinearModel.RData' generates 'y'
load(SET_NON_LINEAR_PATH)
y_nonlinear <- ts(y)
plot(y_nonlinear, main = "(d)")

par(oldpar)

# histogram
oldpar <- par(no.readonly = TRUE)
par(mfrow = c(2,2)); par(oma = c(0,0,0,0)); par(mar = c(4,4,2,1))

hist(Nile, main = "(a)", xlab = "Annual rate")
summary(Nile)

hist(y_CO2, main = "(b)", xlab = "Concentration of CO2")
summary(y_CO2)

hist(log(UKgas), main = "(c)", xlab = "Log(Consumption of gas)")
summary(log(UKgas))

hist(y_nonlinear, main = "(d)", xlab = "Data value")
summary(y_nonlinear)

par(oldpar)

# NA process
NA.point <- which(is.na(y_CO2))
NA.point
y_CO2[NA.point] <- (y_CO2[NA.point - 1] + y_CO2[NA.point + 1]) / 2

# autocorrelation
oldpar <- par(no.readonly = TRUE)
par(mfrow = c(2,2)); par(oma = c(0,0,0,0)); par(mar = c(4,4,3,1))

acf(Nile, main = "(a)")
acf(y_CO2, main = "(b)")
acf(log(UKgas), main = "(c)")
acf(y_nonlinear, main = "(d)")

par(oldpar)

# Frequency Domain Transformation
plot.spectrum <- function(dat, lab = "", main = "",
                          y_max = 1, tick = c(8,4), unit = 1){
  
  dat_FFT <- abs(fft(as.vector(dat)))
  
  data_len <- length(dat_FFT)
  freq_tick <- c(data_len, tick, 2)
  
  plot(dat_FFT/max(dat_FFT), type = "l", main = main,
       ylab = "Standarized Frequency Spectrum", ylim = c(0, y_max),
       xlab = sprintf("Frequency[1/%s]", lab), xlim = c(1, data_len/2), xaxt = "n")
  axis(side = 1, at = data_len/freq_tick * unit + 1,
       labels = sprintf("1/%d", freq_tick), cex.axis = 0.7)
  
}

oldpar <- par(no.readonly = TRUE)
par(mfrow = c(2,2)); par(oma = c(0,0,0,0)); par(mar = c(4,4,3,1))

# スペクトルのピーク部分を周期とする。
plot.spectrum(Nile, lab = "Year", main = "(a)")
plot.spectrum(y_CO2, lab = "Month", main = "(b)", tick = c(12, 6))
plot.spectrum(log(UKgas), lab = "Month", main = "(c)", tick = c(12, 6), unit = 3)
plot.spectrum(y_nonlinear, lab = "Point", main = "(d)")

# Holt-Winters method
# Exponentially Weighted Moving Average (EWMA)

HW_Nile <- HoltWinters(Nile, beta = FALSE, gamma = FALSE)
str(HW_Nile)

HW_CO2 <- HoltWinters(y_CO2)

HW_UKgas_log <- HoltWinters(log(UKgas))

HW_nonlinear <- HoltWinters(y_nonlinear, gamma = FALSE)

oldpar <- par(no.readonly = TRUE)
par(mfrow = c(2,2)); par(oma = c(0,0,0,0)); par(mar = c(4,4,3,1))
mygray <- "#80808080"

plot(HW_Nile, main = "(a)", col = mygray, col.predicted = "black", lty.predicted = "dashed")
plot(HW_CO2, main = "(b)", col = mygray, col.predicted = "black", lty.predicted = "dashed")
plot(HW_UKgas_log, main = "(c)", col = mygray, col.predicted = "black", lty.predicted = "dashed")
plot(HW_nonlinear, main = "(d)", col = mygray, col.predicted = "black", lty.predicted = "dashed")

par(oldpar)

oldpar <- par(no.readonly = TRUE)
par(mfrow = c(2,2)); par(oma = c(0,0,0,0)); par(mar = c(4,4,3,1))

#
HW_out <- HW_Nile
HW_decomp <- ts.union(y = HW_out$x,
                      Level = HW_out$fitted[,"level"],
                      Residulas = residuals(HW_out))
plot(HW_decomp, main = "(a)")

#
HW_out <- HW_CO2
HW_decomp <- ts.union(y = HW_out$x,
                      Level = HW_out$fitted[,"level"] + HW_out$fitted[,"trend"],
                      Season = HW_out$fitted[, "season"],
                      Residuals = residuals(HW_out))
plot(HW_decomp, main = "(b)")

#
HW_out <- HW_UKgas_log
HW_decomp <- ts.union(y = HW_out$x,
                      Level = HW_out$fitted[,"level"] + HW_out$fitted[,"trend"],
                      Season = HW_out$fitted[, "season"],
                      Residuals = residuals(HW_out))
plot(HW_decomp, main = "(c)")

#
HW_out <- HW_nonlinear
HW_decomp <- ts.union(y = HW_out$x,
                      Level = HW_out$fitted[,"level"] + HW_out$fitted[,"trend"],
                      Residuals = residuals(HW_out))
plot(HW_decomp, main = "(d)")

# Holt-Winters predictions
HW_predict <- predict(HW_CO2, n.ahead = 12)
str(HW_predict)

plot(HW_CO2, HW_predict, main = "'Holt-Winters' filtering and predictions",
     col = mygray, col.predicted = "black", lty.predicted = "dashed")
y_CO2_2015 <- window(y_all, start = 2015)
lines(y_CO2_2015, col = mygray)

oldpar <- par(no.readonly = TRUE)
par(mfrow = c(2,2)); par(oma = c(0,0,0,0)); par(mar = c(4,4,3,1))

acf(residuals(HW_Nile), main = "(a)")
acf(residuals(HW_CO2), main = "(b)")
acf(residuals(HW_UKgas_log), main = "(c)")
acf(residuals(HW_nonlinear), main = "(d)")

# Mean Absolute Percentage Error: MAPE
MAPE <- function(true, pred) {
  mean(abs(pred - true)/ true)
}

MAPE(true = y_CO2_2015, pred = HW_predict)

##################################################################
# 5. INTRODUCTION OF TIME SERIES ANALYSIS
#
##################################################################

### Initialization ###
rm(list = ls()); sapply(X = 1:9, FUN = gc)
memory.size(max = T)
# memory.limit()
memory.limit(size=56000)

# config
SET_RESPOSITORY_PATH <- getwd()
setwd(SET_RESPOSITORY_PATH)
set.seed(23)

if(!require(dlm)){
  install.packages("dlm")
  library(dlm)
} else {
  library(dlm)
}

# AR(1)を含む状態空間の設定
W <- 1
V <- 2
phi <- 0.98
mod <- dlmModPoly(order = 1, dW = W, dV = V, C0 = 100)
mod

##################################################################
# 8. Kalman Filter
#
##################################################################

### Initialization ###
rm(list = ls()); sapply(X = 1:9, FUN = gc)
memory.size(max = T)
# memory.limit()
memory.limit(size=56000)

# config
SET_RESPOSITORY_PATH <- getwd()
setwd(SET_RESPOSITORY_PATH)

# ナイル川の流量データを観測値に設定
y <- Nile
t_max <- length(y)

# 1時点分のカルマンフィルタリングを行う関数
Kalman_filtering <- function(m_t_minus_1, C_t_minus_1, t) {
  # 1期先予測分布
  a_t <- G_t %*% m_t_minus_1
  R_t <- G_t %*% C_t_minus_1 %*% t(G_t) + W_t
  
  # 1期先予測尤度
  f_t <- F_t %*% a_t
  Q_t <- F_t %*% R_t %*% t(F_t) + V_t
  
  # カルマン利得
  K_t <- R_t %*% t(F_t) %*% solve(Q_t)
  
  # 状態の更新
  m_t <- a_t + K_t %*% (y[t] - f_t)
  C_t <- (diag(nrow(R_t)) - K_t %*% F_t ) %*% R_t
  
  return(list(m = m_t, C = C_t,
              a = a_t, R = R_t))
}

G_t <- matrix(1, ncol = 1, nrow = 1); W_t <- matrix(exp(7.29), ncol = 1, nrow = 1)
F_t <- matrix(1, ncol = 1, nrow = 1); V_t <- matrix(exp(9.62), ncol = 1, nrow = 1)
m0 <- matrix(0, ncol = 1, nrow = 1); C0 <- matrix(1e+7, ncol = 1, nrow = 1)

m <- rep(NA_real_, t_max); C <- rep(NA_real_, t_max)
a <- rep(NA_real_, t_max); R <- rep(NA_real_, t_max)

# 時点 t = 1
KF <- Kalman_filtering(m0, C0, t = 1)
m[1] <- KF$m; C[1] <- KF$C
a[1] <- KF$a; R[1] <- KF$R

for (t in 2:t_max) {
  KF <- Kalman_filtering(m[t-1], C[t-1], t = t)
  m[t] <- KF$m; C[t] <- KF$C
  a[t] <- KF$a; R[t] <- KF$R
}

# Kalman predictions
# Premise: already finish Kalman Filtering
t <- t_max
nAhead <- 10

Kalman_prediction <- function(a_t0, R_t0) {
  # one-step ahead prediction
  a_t1 <- G_t_plus_1 %*% a_t0
  R_t1 <- G_t_plus_1 %*% R_t0 %*% t(G_t_plus_1) + W_t_plus_1
  
  return(list(a = a_t1, R = R_t1))
}

G_t_plus_1 <- G_t; W_t_plus_1 <- W_t

a_ <- rep(NA_real_, t_max + nAhead); R_ <- rep(NA_real_, t_max + nAhead)

a_[t + 0] <- m[t]; R_[t + 0] <- C[t]

# k: 1 to nAhead
for (k in 1:nAhead){
  KP <- Kalman_prediction(a_[t + k-1], R_[t + k-1])
  a_[t + k] <- KP$a; R_[t + k] <- KP$R
}


# Kalman smoothing

Kalman_smoothing <- function(s_t_plus_1, S_t_plus_1, t) {
  # Smoothing gain
  A_t <- C[t] %*% t(G_t_plus_1) %*% solve(R[t + 1])
  
  # Update state
  s_t <- m[t] + A_t %*% (s_t_plus_1 - a[t + 1])
  S_t <- C[t] + A_t %*% (S_t_plus_1 - R[t + 1]) %*% t(A_t)
  
  return(list(s = s_t, S = S_t))
}

s <- rep(NA_real_, t_max); S <- rep(NA_real_, t_max)

s[t_max] <- m[t_max]; S[t_max] <- C[t_max]

for(t in (t_max - 1):1) {
  KS <- Kalman_smoothing(s[t + 1], S[t + 1], t = t)
  s[t] <- KS$s; S[t] <- KS$S
}

oldpar <- par(no.readonly = TRUE)
par(mfrow = c(2,2)); par(oma = c(0,0,0,0)); par(mar = c(4,4,3,1))

m_sdev <- sqrt(C)
m_quant <- list(m + qnorm(0.025, sd = m_sdev), m + qnorm(0.975, sd = m_sdev))

ts.plot(cbind(y, m, do.call("cbind", m_quant)),
        col = c("lightgray", "black", "black", "black"),
        lty = c("solid", "solid", "dashed", "dashed"))

legend(legend = c("Observations", "Mean", "95% CI"),
       lty = c("solid", "solid", "dashed"),
       col = c("lightgray", "black", "black"),
       x = "topright", cex = 0.6)

a_ <- ts(a_, start = 1871)
a_sdev <- sqrt(R_)
a_quant <- list(a_ + qnorm(0.025, sd = a_sdev), a_ + qnorm(0.975, sd = a_sdev))

# 結果のプロット
ts.plot(cbind(y, a_, do.call("cbind", a_quant)),
        col = c("lightgray", "black", "black", "black"),
        lty = c("solid", "solid", "dashed", "dashed"))
# 凡例
legend(legend = c("Observations", "Mean (Prediction Dist.)", "95% CI (Prediction Dist.)"),
       lty = c("solid", "solid", "dashed"),
       col = c("lightgray", "black", "black"),
       x = "topright", cex = 0.6)

# フィルタリング分布の95%区間のために、2.5%値と97.5%値を求める
s_sdev <- sqrt(S)
s_quant <- list(s + qnorm(0.025, sd = s_sdev), s + qnorm(0.975, sd = s_sdev))
# 結果のプロット
ts.plot(cbind(y, s, do.call("cbind", s_quant)),
        col = c("lightgray", "black", "black", "black"),
        lty = c("solid", "solid", "dashed", "dashed"))
# 凡例
legend(legend = c("Observations", "Mean (Smoothing Dist.)", "95% CI (Smoothing Dist.)"),
       lty = c("solid", "solid", "dashed"),
       col = c("lightgray", "black", "black"),
       x = "topright", cex = 0.6)

