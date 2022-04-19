library(tseries) 
library(vars)
library(forecast)
library(nlWaldTest) 
library(lmtest) 
library(PoEdata) 
library(car) 
library(sandwich)
library(knitr) 
library(urca)
library(zoo)
library(mFilter)
library(forecast)
library(fpp2)
library(devtools)
library(broom)
library(stargazer)
library(tidyr)
library(dynlm)
library(tsDyn)
library(writexl)
library("Ecdat") 
library("gdata")
library("rugarch")

rm(list=ls())

setwd('/Users/vsergeev/Downloads')
x <- read.csv("data_gmu.csv", header = TRUE)
print(x)


national_defence = ts(x[,"nat_defence"], frequency=4, start=2013, end=2022)
national_security = ts(x[,"nat_security"], frequency=4, start=2013, end=2022)
comm_economy = ts(x[,"house_service"], frequency=4, start=2013, end=2022) 
education = ts(x[,"education"], frequency=4, start=2013, end=2022) 
health = ts(x[,"health"], frequency=4, start=2013, end=2022)
real_gdp = ts(x[,"gdp_real"], frequency=4, start=2013, end=2022)
tax_income = ts(x[,"taxes_income"], frequency=4, start=2013, end=2022)

price = ts(x[,"price"], frequency=4, start=2013, end=2022)
rate = ts(x[,"rate"], frequency=4, start=2013, end=2022)
oil_income = ts(x[,"oil_income"], frequency=4, start=2013, end=2022)


endog_var <- as.matrix(cbind(
  national_defence,
  national_security,
  comm_economy,
  education,
  health,
  real_gdp,
  tax_income
  ))

a1 <- diag(7)

exog_var<- as.matrix(cbind(
  price,
  rate,
  oil_income
))

a1[2, 1] <- NA

a1[3, 1] <- NA
a1[3, 2] <- NA


a1[4, 1] <- NA
a1[4, 2] <- NA
a1[4, 3] <- NA

a1[5, 1] <- NA
a1[5, 2] <- NA
a1[5, 3] <- NA
a1[5, 4] <- NA

a1[6, 1] <- NA
a1[6, 2] <- NA
a1[6, 3] <- NA
a1[6, 4] <- NA
a1[6, 5] <- NA

a1[7, 1] <- NA
a1[7, 2] <- NA
a1[7, 3] <- NA
a1[7, 4] <- NA
a1[7, 5] <- NA
a1[7, 6] <- NA

b1 <- diag(7)
diag(b1) <- NA

VARselect(endog_var, lag.max = 10, type = "none", exogen = exog_var)
p=2
var <- VAR(endog_var, p = p, type = "none", exogen = exog_var)
summary(var)


svar1 <- SVAR(var, estmethod = "direct", Amat = a1, Bmat = b1, max.iter = 100)
summary(svar1)

first11 <- irf(svar1, response = "real_gdp", impulse = "real_gdp", n.ahead = 12, ortho = TRUE, boot = TRUE)

dev.off()
dev.new(width = 10, height = 6, unit = "cm", noRStudioGD = TRUE)
plot(first11)

library(forecast)
vth1 <- fevd(svar1, n.ahead=3)
summary(vth1)
dev.off()
dev.new(width = 10, height = 6, unit = "cm", noRStudioGD = TRUE)
plot(vth1)


