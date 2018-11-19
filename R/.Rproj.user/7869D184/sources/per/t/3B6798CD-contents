# 環境リセット----
rm(list = ls()); sapply(X = 1:9, FUN = gc)
memory.size(max = T)
# memory.limit()
memory.limit(size=56000)

# config
SET_RESPOSITORY_PATH <- "C:/Users/yuta.sonoda/Desktop/Programming/R"
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
SET_RESPOSITORY_PATH <- "C:/Users/yuta.sonoda/Desktop/Programming/R"
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
SET_RESPOSITORY_PATH <- "C:/Users/yuta.sonoda/Desktop/Programming/R"
setwd(SET_RESPOSITORY_PATH)
SET_CO2_PATH <- "./../data/CO2.csv"
SET_NON_LINEAR_PATH <- "./00_RData/BenchmarkNonLinearModel.RData"

#######