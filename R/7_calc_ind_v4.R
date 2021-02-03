########################################################################################
###                                                                                   ##
###  R-codes - Calculate three performance indicators with R                          ##
###  ------------------------------------------------------------------------------   ##
###  X.Gong, 2021-01-17                                                               ##
###                                                                                   ##
########################################################################################
### Install and load in libraries                                         ##############
########################################################################################
rm(list=ls())
getwd()
if(!require(PerformanceAnalytics)) install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)

########################################################################################
### Read in and process the dataset                                       ##############
########################################################################################
setwd("C:/Users/xgong/Documents/R/DL_Dataset/Excel")
df1 <- read.csv("wideformat_MF_data2.csv",na.strings = " ")
symbols <- names(df1)
n_symbols <- which(symbols=="ZS")
df1$Date <- as.Date(df1$Date, "%m/%d/%Y")
df2 <- df1
rownames(df2) = df2[,1]
df2 <- df2[,c(-1)]
df2 <- as.xts(df2)     # a xts file with missing values #
# Calculate the average returns of the portfolio thus the returns of the portfolio #
dat1 <- df2            # whole set of stocks including GSPC
dat2 <- df2[,c(-1)]    # all others except GSPC
dat3 <- df2[,c(1)]     # GSPC and will include ret_portfolio as in the following
dat3$ret_port = rowMeans(dat2, na.rm = TRUE)
dat3 <- dat3[,c(2,1)]

########################################################################################
### Active Premium = portfolio annualized return - benchmark's annualized return #######
########################################################################################
ActivePremium(dat3[,1, drop = FALSE], dat3[,2, drop = FALSE])

########################################################################################
### Compare the performance of the portfolio with that of GSPC in a chart ##############
########################################################################################
scenario1 <- dat3
title <- " "
# title <- "Performance of the return of the portfolio vs. SP500"
assets <- ncol(scenario1)
scenarios <- nrow(scenario1)
cat("\n", paste0(names(scenario1), "\n"))
charts.PerformanceSummary(
  scenario1,
  main = title,
  legend.loc = "topleft")

########################################################################################
### Traditional or modified Sharpe Ratio of Return over StdDev or VaR or ES ############
########################################################################################
scenario2 <- dat3
SharpeRatio(scenario2[, 1:2, drop = FALSE], FUN="VaR")
SharpeRatio(scenario2[, 1:2, drop = FALSE], Rf = .04/12, FUN="VaR")
SharpeRatio(scenario2[, 1:2, drop = FALSE], Rf = .04/12, FUN="VaR", method="gaussian")
SharpeRatio(scenario2[, 1:2, drop = FALSE], FUN="ES")

# and all the methods
SharpeRatio(scenario2[,1:2],Rf = .04/12)

########################################################################################
### Treynor Ratio or modified Treynor Ratio of excess return over CAPM beta ############
########################################################################################
scenario3 <- dat3
round(TreynorRatio(scenario3[,1], scenario3[,2], Rf=.035/12),4)
print(TreynorRatio(scenario3[,1], scenario3[,2], modified = TRUE)) 

########################################################################################
### Jensen's alpha adjusted for specific risk (instead of total risk).      ############
########################################################################################
scenario4 <- dat3
scenario4 <- na.omit(scenario4)
print(AppraisalRatio(scenario4[,1], scenario4[,2], method="appraisal")) 
print(AppraisalRatio(scenario4[,1], scenario4[,2], method="modified"))
print(AppraisalRatio(scenario4[,1], scenario4[,2], method="alternative"))

########################################################################################
### Construct subset optimized portfolio with R                                      ###
########################################################################################
# rm(list=ls())
getwd()
setwd("C:/Users/xgong/Documents/R/DL_Dataset/Data")
ts <- readRDS(file = "subset_ts2010.rds")           ### Read .rds file    ###
ts1 <- ts["2002-01-01::2010-03-02",]   ### dataset used for the portfolio ###

library(zoo)
ts <- na.omit(ts)                                 ### Remove 'NA' ###
ts1 <- na.omit(ts1)

scenario.set1 <- ts1                                ### Using ts1 ###
symbols <- names(ts1)
n_symbols <- which(symbols=="WU")
assets1 <- ncol(scenario.set1)
scenarios1 <- nrow(scenario.set1)

m <- model(); m$variable(portfolio, lb=0)

m$maximize   ( omega(portfolio) )
m$subject_to ( cardinality(portfolio) <= 7 )
m$subject_to ( cvar(portfolio, 0.95) <= 0.02 ) 

opt <- optimize(m, solver="glpk", data=list(returns = as.matrix(scenario.set1)))  
x <- as.numeric(round(opt$solution[grep("portfolio", 
                                        names(opt$solution))]/ 
                        opt$solution[grep("z", names(opt$solution))], 3))
x                                                                           
portfolio.pie(x, scenario.set1)

########################################################################################
### To the end                                                                       ###
########################################################################################