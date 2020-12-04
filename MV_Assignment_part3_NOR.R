## Clean work space and command window
cat("\014")        # clear command window
rm(list = ls())

## Open data set
library(R.matlab)
library(ggplot2)
library(urca)
library(tseries)
library(quantmod)
library(lmtest)
library("readxl")
library(vars)
library(tsDyn)
library(bvartools)
library(ecm)
library(cointReg)
library(dynlm)
library(stargazer)

data <- read_excel("MVE_assignment_2020_dataset .xlsx")

## Create variables from data set
#year <- data$year[1:59]
pre2 <- diff(data$mean_pre[1:59])
rad2 <- diff(data$mean_rad[1:59])
tmp2 <- diff(data$mean_tmp[1:59])
GDP_cap2 <- diff(data$NY.GDP.PCAP.KD[1:59])
cpi2 <- diff(as.numeric(data$AG.PRD.CROP.XD[1:59]))
cpi2[is.na(cpi2)] <- 0

pre <- pre2[2:58]
rad <- rad2[2:58]
tmp <- tmp2[2:58]
GDP_cap <- GDP_cap2[2:58]
cpi <- cpi2[2:58]

pre1 <- diff(pre2)
rad1 <- diff(rad2)
tmp1 <- diff(tmp2)
GDP_cap1 <- diff(GDP_cap2)
cpi1 <- diff(cpi2)

GDP_df <- data.frame(tmp,rad,pre,cpi)
tmp_df <- data.frame(GDP_cap,rad,pre,cpi)
rad_df <- data.frame(tmp,GDP_cap,pre,cpi)
pre_df <- data.frame(tmp,rad,GDP_cap,cpi)
cpi_df <- data.frame(tmp,rad,pre,GDP_cap)

GDP_df1 <- data.frame(tmp1,rad1,pre1,cpi1)
tmp_df1 <- data.frame(GDP_cap1,rad1,pre1,cpi1)
rad_df1 <- data.frame(tmp1,GDP_cap1,pre1,cpi1)
pre_df1 <- data.frame(tmp1,rad1,GDP_cap1,cpi1)
cpi_df1 <- data.frame(tmp1,rad1,pre1,GDP_cap1)

## dataframes

###### PART 3: Cointegration analysis ######

## dataframes

## OLS: Precipitation, Radiation, Average Temp, GDP per Capita, CPI
OLS1 <- lm(GDP_cap~., data=GDP_df)
OLS2 <- lm(tmp~., data=tmp_df)
OLS3 <- lm(rad~., data=rad_df)
OLS4 <- lm(pre~., data=pre_df)
OLS5 <- lm(cpi~., data=cpi_df)

## DOLS: Precipitation, Radiation, Average Temp, GDP per Capita, CPI
DOLS1 <- cointReg(method = c("D"), GDP_df, GDP_cap, GDP_df1, n.lag=2, n.lead=0)
DOLS2 <- cointReg(method = c("D"), tmp_df, tmp, tmp_df1, n.lag=2, n.lead=0)
DOLS3 <- cointReg(method = c("D"), rad_df, rad, rad_df1, n.lag=2, n.lead=0)
DOLS4 <- cointReg(method = c("D"), pre_df, pre, pre_df1, n.lag=2, n.lead=0)
DOLS5 <- cointReg(method = c("D"), cpi_df, cpi, cpi_df1, n.lag=2, n.lead=0)


## FMOLS: Precipitation, Radiation, Average Temp, GDP per Capita, CPI
FMOLS1 <- cointReg(method = c("FM"), GDP_df, GDP_cap, GDP_df1, n.lag=2, n.lead=0)
FMOLS2 <- cointReg(method = c("FM"), tmp_df, tmp, tmp_df1, n.lag=2, n.lead=0)
FMOLS3 <- cointReg(method = c("FM"), rad_df, rad, rad_df1, n.lag=2, n.lead=0)
FMOLS4 <- cointReg(method = c("FM"), pre_df, pre, pre_df1, n.lag=2, n.lead=0)
FMOLS5 <- cointReg(method = c("FM"), cpi_df, cpi, cpi_df1, n.lag=2, n.lead=0)


## ECM
E1 <- ecm(GDP_cap, GDP_df, GDP_df, lags=1)
E2 <- ecm(tmp, tmp_df, tmp_df, lags=1)
E3 <- ecm(rad, rad_df, rad_df, lags=1)
E4 <- ecm(pre, pre_df, pre_df, lags=1)
E5 <- ecm(cpi, cpi_df, cpi_df, lags=1)

## VECM
## Create data panel
q <- ts(cbind(GDP_cap2,cpi2,pre2,rad2,tmp2))

# Estimate VECM model
VECM <- VECM(q, lag = 1, r = 1, estim = "2OLS")
summary(VECM)

## Conduct Eigen test (cointegration)
cointest <- ca.jo(q, K = 2, type = "eigen", ecdet = "none", spec = "transitory")
summary(cointest)

## Weak exogeneity: ALR test
DA <- matrix(c(1,0,0,0,0), c(5,1))
alrtest(cointest, A=DA, r=1)

## Latex output

stargazer(OLS1, OLS2, OLS3,OLS4,OLS5, title="OLS Regression Results",
          align=TRUE,
          column.separate = c(1,1,1,1,1),
          omit.stat=c("LL","ser","f"),
          no.space=TRUE)


stargazer(E1, E2, E3,E4,E5, title="ECM Regression Results",
          align=TRUE,
          column.separate = c(1,1,1,1,1),
          omit.stat=c("LL","ser","f"),
          no.space=TRUE)

