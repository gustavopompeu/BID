# Libraries utilized
library(sidrar)
library(ipeadatar)
library(fredr) # needs API key
library(tidyverse)
library(readxl)
library(seasonal)
library(mFilter)
library(randomForest)
library(e1071)
library(glmnet)
library(forecast)
library(splines)

Bloomberg <- read_excel("D:/Gu/BID/Bloomberg.xlsx", 
                        col_types = c("date", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric"), 
                        na = "#N/A N/A")


CPI_BR <- get_sidra(1737, period="all", variable = 2266)
CPI_BR <- CPI_BR[which(CPI_BR$Mês == "julho 1994"):which(CPI_BR$Mês == "abril 2020"),]

ER_IPEA <- ipeadata("BM12_ERCF12", "br")
ER_IPEA <- ER_IPEA[which(ER_IPEA$date=="1994-07-01"):which(ER_IPEA$date=="2020-05-01"),]

CPI_US <- fredr(
  series_id = "CPIAUCNS",
  observation_start = as.Date("1993-12-01"),
  observation_end = as.Date("2020-04-01")
)

PPI_US <- fredr(
  series_id = "PPIACO",
  observation_start = as.Date("1994-07-01"),
  observation_end = as.Date("2020-04-01")
)

M1_US <- fredr(
  series_id = "M1NS",
  observation_start = as.Date("1994-07-01"),
  observation_end = as.Date("2020-04-01")
)

IPI_US <- fredr(
  series_id = "INDPRO",
  observation_start = as.Date("1994-07-01"),
  observation_end = as.Date("2020-04-01")
)

fedfunds_US <- fredr(
  series_id = "FEDFUNDS",
  observation_start = as.Date("1994-07-01"),
  observation_end = as.Date("2020-04-01")
)

SELIC <- rbcb::get_series(c(SELIC = 4390), start_date = as.Date("1994-07-01"), end_date = as.Date("2020-04-01"))

# Exchange Rate St
ERt <- ts(ER_IPEA$value[1:310])

# Exchange Rate St+1
ERt1 <- ts(ER_IPEA$value[2:311])


# Inflation - Producer Price Index
# indexing PPI in US as avg 2010 = 100, which is BR index
avg2010 <- mean(PPI_US$value[187:198])
PPI_US$value <- PPI_US$value*100/avg2010

# PPI - not necessary to seasonally adjust
PPI_US_NS <- ts(PPI_US$value)
PPI_BR_NS <- ts(Bloomberg$PPI)

# Inflation - Consumer Price Index
# indexing CPI in US as dec 1993 = 100, which is BR index
dec1993 <- CPI_US$value[1]
CPI_US$value <- CPI_US$value*100/dec1993
CPI_US <- CPI_US[-c(1:7),]

# Seasonally adjusting
cpi.us.sa <- seas(ts(CPI_US$value, start=c(1994,7), frequency = 12))
CPI_US_SA <- cpi.us.sa$data[,1]

cpi.br.sa <- seas(ts(CPI_BR$Valor, start=c(1994,7), frequency = 12))
CPI_BR_SA <- cpi.br.sa$data[,1]


# Money stock - M1
# Seasonally adjusting
m1.us.sa <- seas(ts(M1_US$value, start=c(1994,7), frequency = 12))
M1_US_SA <- m1.us.sa$data[,1]

m1.br.sa <- seas(ts(Bloomberg$M1, start=c(1994,7), frequency = 12))
M1_BR_SA <- m1.br.sa$data[,1]

# M1 BR in dollars
M1_BR_inD <- Bloomberg$M1/ER_IPEA$value[1:310]
m1.brd.sa <- seas(ts(M1_BR_inD, start=c(1994,7), frequency = 12))
M1_BRD_SA <- m1.brd.sa$data[,1]


# Output - Industrial Production Index
# both already seasonally adjusted
# both index avg 2017 = 100
IPI_US_SA <- ts(IPI_US$value)
IPI_BR_SA <- ts(Bloomberg$IPI_WB)

# Hodrick-Prescott filter to calculate the potential output to obtain the output gap
hpus <- hpfilter(IPI_US_SA)
cycle_US <- hpus$cycle
trend_US <- hpus$trend
gap_US <- ts(as.vector((cycle_US/trend_US)*100))

hpbr <- hpfilter(IPI_BR_SA)
cycle_BR <- hpbr$cycle
trend_BR <- hpbr$trend
gap_BR <- ts(as.vector((cycle_BR/trend_BR)*100))


# Interest rate - Bond
#  not necessary to seasonally adjust
SELIC_BR <- ts(SELIC$SELIC)
FED_FUNDS_US <- ts(fedfunds_US$value)

# Differences
dER <- ERt1 - ERt

dPPI <- PPI_US_NS - PPI_BR_NS
dCPI <- CPI_US_SA - CPI_BR_SA

dM1 <- M1_US_SA - M1_BR_SA
dM1d <- M1_US_SA - M1_BRD_SA

dIPI <- IPI_US_SA - IPI_BR_SA
dgap <- gap_US - gap_BR

dIR <- FED_FUNDS_US - SELIC_BR

# Logs
logERt1 <- log(ERt1)
logERt <- log(ERt)

logPPI_US <- log(PPI_US_NS)
logPPI_BR <- log(PPI_BR_NS)
logCPI_US <- log(CPI_US_SA)
logCPI_BR <- log(CPI_BR_SA)

logM1_US <- log(M1_US_SA)
logM1_BR <- log(M1_BR_SA)
logM1_BRD <- log(M1_BRD_SA)

logIPI_US <- log(IPI_US_SA)
logIPI_BR <- log(IPI_BR_SA)

log_hpus <- hpfilter(logIPI_US)
log_cycle_US <- log_hpus$cycle
log_trend_US <- log_hpus$trend
log_gap_US <- ts(as.vector((log_cycle_US/log_trend_US)*100))

log_hpbr <- hpfilter(logIPI_BR)
log_cycle_BR <- log_hpbr$cycle
log_trend_BR <- log_hpbr$trend
log_gap_BR <- ts(as.vector((log_cycle_BR/log_trend_BR)*100))

logFED_FUNDS_US <- log(FED_FUNDS_US)
logSELIC_BR <- log(SELIC_BR)


# Log Difference
dlogER <- logERt1 - logERt

dlogPPI <- logPPI_US - logPPI_BR
dlogCPI <- logCPI_US - logCPI_BR

dlogM1 <- logM1_US - logM1_BR
dlogM1d <- logM1_US - logM1_BRD

dlogIPI <- logIPI_US - logIPI_BR
dloggap <- log_gap_US - log_gap_BR

dlogIR <- logFED_FUNDS_US - logSELIC_BR



# Bases

data_full94 <- data.frame(resp = ERt1, ERt, PPI_US_NS, PPI_BR_NS, CPI_US_SA, CPI_BR_SA, M1_US_SA, M1_BR_SA, M1_BRD_SA, 
                          IPI_US_SA, IPI_BR_SA, gap_US, gap_BR, FED_FUNDS_US, SELIC_BR)

data_full94_respd <- data.frame(resp = dER, ERt, PPI_US_NS, PPI_BR_NS, CPI_US_SA, CPI_BR_SA, M1_US_SA, M1_BR_SA, M1_BRD_SA, 
                                IPI_US_SA, IPI_BR_SA, gap_US, gap_BR, FED_FUNDS_US, SELIC_BR)


data_diff94 <- data.frame(resp = dER, ERt, dPPI, dCPI, dM1, dM1d, dIPI, dgap, dIR)

data_diff94_respt1 <- data.frame(resp = ERt1, ERt, dPPI, dCPI, dM1, dM1d, dIPI, dgap, dIR)

data_logfull94 <- data.frame(resp = logERt1, logERt, logPPI_US, logPPI_BR, logCPI_US, logCPI_BR, logM1_US, logM1_BR, logM1_BRD, 
                             logIPI_US, logIPI_BR, log_gap_US, log_gap_BR, logFED_FUNDS_US, logSELIC_BR)

data_logfull94_respd <- data.frame(resp = dlogER, logERt, logPPI_US, logPPI_BR, logCPI_US, logCPI_BR, logM1_US, logM1_BR, logM1_BRD, 
                                   logIPI_US, logIPI_BR, log_gap_US, log_gap_BR, logFED_FUNDS_US, logSELIC_BR)

data_logdiff94 <- data.frame(resp = dlogER, logERt, dlogPPI, dlogCPI, dlogM1, dlogM1d, dlogIPI, dloggap, dlogIR)

data_logdiff94_respt1 <- data.frame(resp = logERt1, logERt, dlogPPI, dlogCPI, dlogM1, dlogM1d, dlogIPI, dloggap, dlogIR)


################## Functions

# RMSE
RMSE <- function(pred, act){
  sqrt(mean((pred-act)^2))
}

# test statistics and p-value
tests <- function(pred, dat, ini){
  dmt <- dm.test(0 - dat$resp[ini:nrow(dat)], pred - dat$resp[ini:nrow(dat)], alternative = "greater")
  dir <- (pred > 0) == (dat$resp[ini:nrow(dat)] > 0)
  n <- length(dir)
  x <- sum(dir)
  pro <- binom.test(x,n,p=0.5,alternative = "greater")
  return(c(dmt$statistic,dmt$p.value,mean(dir),pro$p.value))
}

# Random forest
rf_pred <- function(data, lin){
  pr <- c()
  k <- 1
  pr2 <- c()
  res <- list()
  for(j in 1:(ncol(data)-1)){
    for(i in (lin-1):(nrow(data)-1)){
      mod <- randomForest(resp~., data=data[k:i,], mtry=j)
      pr[k] <- predict(mod, newdata=data[(i+1),])
      k <- k+1
    }
    res[[j]] <- pr
    names(res)[j] <- paste("mtry=", j, sep="")
    pr2[j] <- RMSE(pr, data$resp[lin:nrow(data)])
    k <- 1
  }
  names(pr2) <- 1:j
  res[[j+1]] <- pr2
  names(res)[j+1] <- "RMSE's"
  res[[j+2]] <- min(pr2)
  names(res)[j+2] <- "min_RMSE"
  return(res)
}

# SVM
svm_pred <- function(data, lin){
  nu <- seq(0.1, 0.25, length.out=6)
  gamma <- seq(0.001, 0.025, length.out=6)
  cost <- seq(30, 50, length.out=6)
  # nu <- c(0.18, 0.19, 0.2)
  # gamma <- c(0.014, 0.015, 0.016)
  # cost <- c(46,49,52)
  pr <- c()
  k <- 1
  m <- 1
  pr2 <- c()
  res <- list()
  for(n in 1:length(cost)){
    for(l in 1:length(nu)){
      for(j in 1:length(gamma)){
        for(i in (lin-1):(nrow(data)-1)){
          mod <- svm(resp~., data=data[k:i,], type= "nu-regression", nu=nu[l], gamma=gamma[j], cost=cost[n])
          pr[k] <- predict(mod, newdata=data[(i+1),])
          k <- k+1
        }
        res[[m]] <- pr
        names(res)[m] <- paste0("nu=", nu[l], ", gamma=", gamma[j], ", cost=", cost[n])
        pr2[m] <- RMSE(pr, data$resp[lin:nrow(data)])
        k <- 1
        m <- m+1
      }
    }
  }
  res[[m]] <- pr2
  names(res)[m] <- "RMSE's"
  res[[m+1]] <- min(pr2)
  names(res)[m+1] <- paste0("min_RMSE ", names(res)[which.min(res[[m]])]) 
  return(res)
}

# Regularized regression splines
sh_pred <- function(data, lin){
  alpha <- c(0, 1)
  lambda <- seq(0.002,0.009,length.out=100)
  pr <- c()
  k <- 1
  m <- 1
  pr2 <- c()
  res <- list()
  aa <- names(data)
  ini <- "resp ~ "
  for(p in 2:length(aa)){
    if(p==length(aa)){
      ini <- paste0(ini, "bs(", aa[p], ",knots=2)")
    }else{
      ini <- paste0(ini, "bs(", aa[p], ",knots=2):")
    }
  }
  for(j in 1:length(alpha)){
    for(l in 1:length(lambda)){
      for(i in (lin-1):(nrow(data)-1)){
        x <- model.matrix(as.formula(ini), data=data[k:i,])
        y <- data$resp[k:i]
        mod <- glmnet(x, y, alpha=alpha[j], lambda=lambda[l], nlambda=1)
        pr[k] <- predict(mod, newx = model.matrix(as.formula(ini), data=data[(i+1),]))
        k <- k+1
      }
      res[[m]] <- pr
      names(res)[m] <- paste0("alpha=", alpha[j], ", lambda=", lambda[l])
      pr2[m] <- RMSE(pr, data$resp[lin:nrow(data)])
      k <- 1
      m <- m+1
    }
  }
  res[[m]] <- pr2
  names(res)[m] <- "RMSE's"
  res[[m+1]] <- min(pr2)
  names(res)[m+1] <- paste0("min_RMSE ", names(res)[which.min(res[[m]])]) 
  return(res)
}

########### Vectors for diff94 bases
# standard monetary model: M1, IPI
v1 <- c(1,2,5,7)
# standard monetary model with sticky prices: CPI, M1, IPI
v2 <- c(1,2,4,5,7)
# standard monetary model with sticky prices and UIP deviations: CPI, M1, IPI, interest
v3 <- c(1,2,4,5,7,9)
# taylor rule model: CPI, gap, interest
v4 <- c(1,2,4,8,9)
# standard monetary model with sticky prices: PPI, M1, IPI
v5 <- c(1,2,3,5,7)
# standard monetary model with sticky prices and UIP deviations: PPI, M1, IPI, interest
v6 <- c(1,2,3,5,7,9)
# taylor rule model: PPI, gap, interest
v7 <- c(1,2,3,8,9)
sq <- paste("v", seq(1:7), sep="")

### data_logdiff94

# random forest
dmp <- matrix(0, ncol=5, nrow=7)
for(i in 1:length(sq)){
  a <- rf_pred(data_logdiff94[54:310,get(sq[i])], 241) # change 241 for 229, 217
  dmp[i,] <- c(a[[length(a)]], tests(a[[which.min(a$`RMSE's`)]], data_logdiff94[54:310,], 241)) # change 241 for 229, 217
}
rmdm <- as.data.frame(dmp)
rmdm

# svm
dmp <- matrix(0, ncol=5, nrow=7)
for(i in 1:length(sq)){
  a <- svm_pred(data_logdiff94[54:310,get(sq[i])], 217) # change 241 for 229, 217
  dmp[i,] <- c(a[[length(a)]], tests(a[[which.min(a$`RMSE's`)]], data_logdiff94[54:310,], 217)) # change 241 for 229, 217
}
rmdm <- as.data.frame(dmp)
rmdm

# regression splines
dmp <- matrix(0, ncol=5, nrow=7)
for(i in 1:length(sq)){
  a <- sh_pred(data_logdiff94[54:310,get(sq[i])], 241) # change 241 for 229, 217
  dmp[i,] <- c(a[[length(a)]], tests(a[[which.min(a$`RMSE's`)]], data_logdiff94[54:310,], 241)) # change 241 for 229, 217
}
rmdm <- as.data.frame(dmp)
rmdm

### data_diff94

# svm
dmp <- matrix(0, ncol=5, nrow=7)
for(i in 1:length(sq)){
  a <- svm_pred(data_diff94[54:310,get(sq[i])], 241) # change 241 for 229, 217
  dmp[i,] <- c(a[[length(a)]], tests(a[[which.min(a$`RMSE's`)]], data_diff94[54:310,], 241)) # change 241 for 229, 217
}
rmdm <- as.data.frame(dmp)
rmdm


######### Vectors for full94 bases
# standard monetary model: M1, IPI
v1 <- c(1,2,7,8,10,11)
# standard monetary model with sticky prices: CPI, M1, IPI
v2 <- c(1,2,5,6,7,8,10,11)
# standard monetary model with sticky prices and UIP deviations: CPI, M1, IPI, interest
v3 <- c(1,2,5,6,7,8,10,11,14,15)
# taylor rule model: CPI, gap, interest
v4 <- c(1,2,5,6,12,13,14,15)
# standard monetary model with sticky prices: PPI, M1, IPI
v5 <- c(1,2,3,4,7,8,10,11)
# standard monetary model with sticky prices and UIP deviations: PPI, M1, IPI, interest
v6 <- c(1,2,3,4,7,8,10,11,14,15)
# taylor rule model: PPI, gap, interest
v7 <- c(1,2,3,4,12,13,14,15)
sq <- paste("v", seq(1:7), sep="")

# data_logfull94_respd

# random forest
dmp <- matrix(0, ncol=5, nrow=7)
for(i in 1:length(sq)){
  a <- rf_pred(data_logfull94_respd[54:310,get(sq[i])], 241) # change 241 for 229, 217
  dmp[i,] <- c(a[[length(a)]], tests(a[[which.min(a$`RMSE's`)]], data_logfull94[54:310,], 241)) # change 241 for 229, 217
}
rmdm <- as.data.frame(dmp)
rmdm

# svm
dmp <- matrix(0, ncol=5, nrow=7)
for(i in 1:length(sq)){
  a <- svm_pred(data_logfull94_respd[54:310,get(sq[i])], 241) # change 241 for 229, 217
  dmp[i,] <- c(a[[length(a)]], tests(a[[which.min(a$`RMSE's`)]], data_logfull94_respd[54:310,], 241))  # change 241 for 229, 217
}
rmdm <- as.data.frame(dmp)
rmdm

# regression splines
dmp <- matrix(0, ncol=5, nrow=7)
for(i in 1:length(sq)){
  a <- sh_pred(data_logfull94_respd[54:310,get(sq[i])], 241) # change 241 for 229, 217
  dmp[i,] <- c(a[[length(a)]], tests(a[[which.min(a$`RMSE's`)]], data_logdiff94[54:310,], 241)) # change 241 for 229, 217
}
rmdm <- as.data.frame(dmp)
rmdm


############### Plots

## SVM
a <- svm_pred(data_logdiff94[54:310,get(sq[1])], 217)
svm1_41 <- a[[which.min(a$`RMSE's`)]]
b <- svm_pred(data_logdiff94[54:310,get(sq[7])], 217)
svm7_41 <- b[[which.min(b$`RMSE's`)]]
plot(data_logdiff94$resp[270:310],type="l", xlab="t", ylab="Log-differential exchange rate")
lines(svm1_41, col=2,lty=2,lwd=2)
lines(svm7_41, col=3,lty=2,lwd=2)
lines(rep(0,41), col=4,lty=2,lwd=2)
legend("topleft", legend=c("Actual values", "SVM1", "SVM7",  "RW"), lty=c(1,2,2,2), col=1:4, lwd=c(1,2,2,2))

## Random forest
a <- rf_pred(data_logfull94_respd[54:310,get(sq[4])], 217)
rf4_41 <- a[[which.min(a$`RMSE's`)]]
plot(data_logfull94_respd$resp[270:310],type="l", xlab="t", ylab="Log-differential exchange rate")
lines(rf4_41, col=2,lty=2,lwd=2)
lines(rep(0,41), col=3,lty=2,lwd=2)
legend("topleft", legend=c("Actual values", "RF4",  "RW"), lty=c(1,2,2), col=1:3, lwd=c(1,2,2))

## Regression splines
a <- sh_pred(data_logdiff94[54:310,get(sq[7])], 217)
sp7_41 <- a[[which.min(a$`RMSE's`)]]
plot(data_logdiff94$resp[270:310],type="l", xlab="t", ylab="Log-differential exchange rate")
lines(sp7_41, col=2,lty=2,lwd=2)
lines(rep(0,41), col=3,lty=2,lwd=2)
legend("topleft", legend=c("Actual values", "SP7",  "RW"), lty=c(1,2,2), col=1:3, lwd=c(1,2,2))
