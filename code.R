library(readxl)
library(dynlm)
library(ggplot2)
library(AER)
library(Hmisc)
library(forecast)
library(x12)
library(dLagM)
library(TSA)
library(readr)
library(dplyr)
library(tseries)
library(FitAR)

source('GoFVals.R')
source('GoFVals_damped.R')
source('MASEvalues.R')
source('MASEvalues_damped.R')
source('MASE.forecast.R')
source('MATH1307_utilityFunctions.R')



## ======== Yearly data analysis ======== ##

##loading yearly data
yearly <- read_excel("M3C_reduced_2019.xlsx", sheet = "M3Year")

##converting to time series and bifurcating into train and test set
year.complete<-list()
year.training<-list()
yearly.testing<-list()
for (i in 1:333) {
  year.complete[[i]] = ts(t(yearly[i,6:(5+yearly$N[i])]), 
                          start = c(yearly$`Starting Year`[i]))
  year.training[[i]] = ts(t(yearly[i,6:(5+trunc(0.95*yearly$N[i]))]), 
                          start = c(yearly$`Starting Year`[i]))
  yearly.testing[[i]] = ts(t(yearly[i,((6+trunc(0.95*(yearly$N[i])))):(5+yearly$N[i])]), 
                           start = c(yearly$`Starting Year`[i]+trunc(0.95*(yearly$N[i]))))
}


##testing stationarity using PP test
yearly_pp_test <- 0
for (i in 1:333) {
  if (pp.test(year.training[[i]], lshort = TRUE)$p.value < 0.05){
    yearly_pp_test <- yearly_pp_test + 1
  }
}

yearly_pp_test

##applying box-cox transformation to handle changing variance
year_lambda <- list()
BC.year.complete <- list()
BC.year.train <- list()
BC.year.test <- list()
for (i in 1:333) {
  year_lambda[[i]] <- BoxCox.lambda(year.complete[[i]])
  BC.year.complete[[i]] = ((year.complete[[i]]^(year_lambda[[i]])) - 1) / year_lambda[[i]]
  BC.year.train[[i]] = ((year.training[[i]]^(year_lambda[[i]])) - 1) / year_lambda[[i]]
  BC.year.test[[i]] = ((yearly.testing[[i]]^(year_lambda[[i]])) - 1) / year_lambda[[i]]
}

## checking for any 0 or negtive values
year_neg<-0
for (i in 1:332) {
  if (BC.year.train[[i]] <0)
  {year_neg = year_neg+1}
}

year_neg

##Checking the information criteria and frequency of best model fittings
year_H=6

# Models with undamped trend 
year_models=c("ANN","MNN","MMN","MAN","AAN")
year.govals<-GoFVals(BC.year.train,H = year_H,models = year_models)
year.govals.df<-as.data.frame(year.govals)
year.GoF.Best<-year.govals.df %>% group_by(GoF.series) %>% slice(which.min(GoF.MASE))
table(year.GoF.Best$GoF.FittedModels)
year_IC <- year.govals$GoF %>%  group_by(FittedModels) %>% summarise(AIC = mean(AIC), BIC = mean(BIC), HQIC = mean(HQIC), MASE = mean(MASE))

## comparing IC for models with undamped trend
year_IC

## models with damped trend
year_models_damped=c("AAN", "MMN")
year.govals_damped<-GoFVals_damped(BC.year.train,H = year_H,models = year_models_damped)
year.govals.df.damped<-as.data.frame(year.govals_damped)
year.GoF.Best.damped<-year.govals.df.damped %>% group_by(GoF.series) %>% slice(which.min(GoF.MASE))
table(year.GoF.Best.damped$GoF.FittedModels)
year_IC_damped <- year.govals_damped$GoF %>%  group_by(FittedModels) %>% summarise(AIC = mean(AIC), BIC = mean(BIC), HQIC = mean(HQIC), MASE = mean(MASE))

## comparing models with damped trend
year_IC_damped

## comparing mean MASE and MASE rank
year_MASEs = year.govals$GoF$MASE
MASEvalues(data = BC.year.train, H = year_H, model = "AAN", MASEs = year_MASEs)
year_MASEs_damped = year.govals_damped$GoF$MASE
MASEvalues_damped(data = BC.year.train, H = year_H, model = "AAN", MASEs = year_MASEs_damped)

## forcasting train over test and calculating the accuracy of model chosen
year.model=vector("list",333)
for (i in 1:333) {
  year.model[[i]]<-forecast(ets(BC.year.train[[i]],model = "AAN", damped = TRUE),h=nrow(BC.year.test[[i]]))
}

## calculating mean MASE test set 
year_MASE<-vector("list", 333)
for (i in 1:333) {
  year_MASE[[i]] <- MASE.forecast(BC.year.train[[i]], BC.year.test[[i]], year.model[[i]]$mean)  
}

year_MASE_list = 0
for (j in 1:333){
  year_MASE_list = year_MASE_list + year_MASE[[j]]
}
year.MASE <- year_MASE_list/333
year.MASE

## residual diagnostics
year_non.Norm.Std.R <- 0
year_corr.Std.R <-0
for(i in 1:333){
  if (shapiro.test(year.model[[i]]$residuals)$p.value < 0.05){
    year_non.Norm.Std.R <- year_non.Norm.Std.R + 1
  }  
  if (Box.test(year.model[[i]]$residuals, lag = 1, type = "Ljung-Box", fitdf = 0)$p.value < 0.05){
    year_corr.Std.R <- year_corr.Std.R + 1
  }
  
}

## calculating percentage of non normal standard residuals
100*year_non.Norm.Std.R/333
## calculating percentage of correlated standard residuals
100*year_corr.Std.R/333


## ======== End of Yearly data analysis ======== ##



## ======== Quarterly data analysis ======== ##

##loading quarterly data
quarterly <- read_excel("M3C_reduced_2019.xlsx", sheet = "M3Quart")

##converting to time series and bifurcating into train and test set
quart.complete<-list()
quarterly.training<-list()
quarterly.testing<-list()
for (i in 1:332) {
  quart.complete[[i]] = ts(t(quarterly[i,6:(5+quarterly$N[i])]), 
                           start = c(quarterly$`Starting Year`[i],quarterly$`Starting Quarter`[i]), 
                           frequency = 4)
  quarterly.training[[i]] = ts(t(quarterly[i,6:(5+trunc(0.95*quarterly$N[i]))]), 
                               start = c(quarterly$`Starting Year`[i],quarterly$`Starting Quarter`[i]), 
                               frequency = 4)
  quarterly.testing[[i]] = ts(t(quarterly[i,((6+trunc(0.95*(quarterly$N[i])))):(5+quarterly$N[i])]), 
                              start = c(trunc(quarterly$`Starting Year`[i]+(quarterly$`Starting Quarter`[i]/4)+((0.95*(quarterly$N[i]))/4)),floor((quarterly$`Starting Quarter`[i] + (0.95*quarterly$N[i])))%%4), 
                              frequency = 4)
}

##testing stationarity using PP Test
quarterly_pp_test <- 0
for (i in 1:332) {
  if (pp.test(quart.complete[[i]], lshort = TRUE)$p.value < 0.05){
    quarterly_pp_test <- quarterly_pp_test + 1
  }
}

quarterly_pp_test

##applying box-cox transformation to handle changing variance
quarter_lambda <- list()
BC.quarter.complete <- list()
BC.quarter.train <- list()
BC.quarter.test <- list()

for (i in 1:332) {
  quarter_lambda[[i]] <- BoxCox.lambda(quart.complete[[i]])
  BC.quarter.complete[[i]] = ((quart.complete[[i]]^(quarter_lambda[[i]])) - 1) / quarter_lambda[[i]]
  BC.quarter.train[[i]] = ((quarterly.training[[i]]^(quarter_lambda[[i]])) - 1) / quarter_lambda[[i]]
  BC.quarter.test[[i]] = ((quarterly.testing[[i]]^(quarter_lambda[[i]])) - 1) / quarter_lambda[[i]]
}

## checking for any 0 or negtive values
quarter_neg<-0
for (i in 1:332) {
  if (BC.quarter.train[[i]] <0)
  {quarter_neg = quarter_neg+1}
}

quarter_neg

##Checking the information criteria and frequency of best model fittings
quarter_H=8

# Models with undamped trend 
quarter_models=c("ANA","MNA","MNM","ANN","MMN","MAN","MAM","MMM","AAN","MNN","MAA","AAA")
quarter.govals<-GoFVals(BC.quarter.train,H = quarter_H,models = quarter_models)
quarter.govals.df<-as.data.frame(quarter.govals)
quarter.GoF.Best<-quarter.govals.df %>% group_by(GoF.series) %>% slice(which.min(GoF.MASE))
table(quarter.GoF.Best$GoF.FittedModels)
quarter_IC <- quarter.govals$GoF %>%  group_by(FittedModels) %>% summarise(AIC = mean(AIC), BIC = mean(BIC), HQIC = mean(HQIC), MASE = mean(MASE))

## comparing models with undamped trend
quarter_IC

# Models with damped trend 
quarter_models_damped=c("MMN","MAN","MAM","MMM","AAN","MAA","AAA")
quarter.govals_damped<-GoFVals_damped(BC.quarter.train,H = quarter_H,models = quarter_models_damped)
quarter.govals.df.damped<-as.data.frame(quarter.govals_damped)
quarter.GoF.Best.damped<-quarter.govals.df.damped %>% group_by(GoF.series) %>% slice(which.min(GoF.MASE))
table(quarter.GoF.Best.damped$GoF.FittedModels)
quarter_IC_damped <- quarter.govals_damped$GoF %>%  group_by(FittedModels) %>% summarise(AIC = mean(AIC), BIC = mean(BIC), HQIC = mean(HQIC), MASE = mean(MASE))

## comparing models with damped trend
quarter_IC_damped

## comparing mean MASE and MASE rank
quarter_MASEs = quarter.govals$GoF$MASE
MASEvalues(data = BC.quarter.train, H = quarter_H, model = "AAA", MASEs = quarter_MASEs)
quarter_MASEs_damped = quarter.govals_damped$GoF$MASE
MASEvalues_damped(data = BC.quarter.train, H = quarter_H, model = "AAA", MASEs = quarter_MASEs_damped)


## forcasting train over test and calculating the accuracy of model chosen
quarter.model=vector("list",332)
for (i in 1:332) {
  quarter.model[[i]]<-forecast(ets(BC.quarter.train[[i]],model = "AAA", damped = TRUE),h=nrow(BC.quarter.test[[i]]))
}

## calculating mean MASE on test set 
quarter_MASE<-vector("list", 332)
for (i in 1:332) {
  quarter_MASE[[i]] <- MASE.forecast(BC.quarter.train[[i]], BC.quarter.test[[i]], quarter.model[[i]]$mean)  
}

quarter_MASE_list = 0
for (j in 1:332){
  quarter_MASE_list = quarter_MASE_list + quarter_MASE[[j]]
}
quarter.MASE <- quarter_MASE_list/332
quarter.MASE

## residual diagnostics
quarter_non.Norm.Std.R <- 0
quarter_corr.Std.R <-0
for(i in 1:332){
  if (shapiro.test(quarter.model[[i]]$residuals)$p.value < 0.05){
    quarter_non.Norm.Std.R <- quarter_non.Norm.Std.R + 1
  }  
  if (Box.test(quarter.model[[i]]$residuals, lag = 1, type = "Ljung-Box", fitdf = 0)$p.value < 0.05){
    quarter_corr.Std.R <- quarter_corr.Std.R + 1
  }
  
}

## calculating percentage of non normal standard residuals
100*quarter_non.Norm.Std.R/332
## calculating percentage of correlated standard residuals
100*quarter_corr.Std.R/332


## ======== End of Quarterly data analysis ======== ##



## ======== Monthly data analysis ======== ##

##loading monthly data
monthly <- read_excel("M3C_reduced_2019.xlsx", sheet = "M3Month")

##converting to time series and bifurcating into train and test set
month.complete<-list()
month.training<-list()
month.testing<-list()
for (i in 1:332) {
  month.complete[[i]] = ts(t(monthly[i,6:(5+monthly$N[i])]), 
                           start = c(monthly$`Starting Year`[i],monthly$`Starting Month`[i]), 
                           frequency = 12)
  month.training[[i]] = ts(t(monthly[i,6:(5+trunc(0.95*monthly$N[i]))]), 
                           start = c(monthly$`Starting Year`[i],monthly$`Starting Month`[i]), 
                           frequency = 12)
  month.testing[[i]] = ts(t(monthly[i,((6+trunc(0.95*(monthly$N[i])))):(5+monthly$N[i])]), 
                          start = c(trunc(monthly$`Starting Year`[i]+(monthly$`Starting Month`[i]/12)+((0.95*(monthly$N[i]))/12)),floor((monthly$`Starting Month`[i] + (0.95*monthly$N[i])))%%12), 
                          frequency = 12)
}

##testing stationarity using PP test
month_pp_test <- 0
for (i in 1:332) {
  if (pp.test(month.complete[[i]], lshort = TRUE)$p.value < 0.05){
    month_pp_test <- month_pp_test + 1
  }
}

month_pp_test

##applying box-cox transformation to handle changing variance
month_lambda <- list()
BC.month.complete <- list()
BC.month.train <- list()
BC.month.test <- list()

for (i in 1:332) {
  month_lambda[[i]] <- BoxCox.lambda(month.complete[[i]])
  BC.month.complete[[i]] = ((month.complete[[i]]^(month_lambda[[i]])) - 1) / month_lambda[[i]]
  BC.month.train[[i]] = ((month.training[[i]]^(month_lambda[[i]])) - 1) / month_lambda[[i]]
  BC.month.test[[i]] = ((month.testing[[i]]^(month_lambda[[i]])) - 1) / month_lambda[[i]]
}

## checking for any 0 or negtive values
month_neg<-0
for (i in 1:332) {
  if (BC.month.train[[i]] <0)
  {month_neg = month_neg+1}
}

month_neg

##Checking the information criteria and frequency of best model fittings
month_H=18

# Models with undamped trend 
month_models=c("ANA","MNA","MNM","ANN","MMN","MAN","MAM","MMM","AAN","MNN","MAA","AAA")
Month.govals<-GoFVals(BC.month.train,H = month_H,models = month_models)
Month.govals.df<-as.data.frame(Month.govals)
Month.GoF.Best<-Month.govals.df %>% group_by(GoF.series) %>% slice(which.min(GoF.MASE))
table(Month.GoF.Best$GoF.FittedModels)
month_IC <- Month.govals$GoF %>%  group_by(FittedModels) %>% summarise(AIC = mean(AIC), BIC = mean(BIC), HQIC = mean(HQIC), MASE = mean(MASE))

## comparing models with undamped trend
month_IC

# Models with damped trend 
month_models_damped=c("MMN","MAN","MAM","MMM","AAN","MAA","AAA")
month.govals_damped<-GoFVals_damped(BC.month.train,H = month_H,models = month_models_damped)
month.govals.df.damped<-as.data.frame(month.govals_damped)
month.GoF.Best.damped<-month.govals.df.damped %>% group_by(GoF.series) %>% slice(which.min(GoF.MASE))
table(month.GoF.Best.damped$GoF.FittedModels)
month_IC_damped <- month.govals_damped$GoF %>%  group_by(FittedModels) %>% summarise(AIC = mean(AIC), BIC = mean(BIC), HQIC = mean(HQIC), MASE = mean(MASE))

## comparing models with damped trend
month_IC_damped

## comparing mean MASE and MASE rank
month_MASEs = month.govals$GoF$MASE
MASEvalues(data = BC.month.train, H = month_H, model = "AAA", MASEs = month_MASEs)
month_MASEs_damped = month.govals_damped$GoF$MASE
MASEvalues_damped(data = BC.month.train, H = month_H, model = "AAA", MASEs = month_MASEs_damped)


## forcasting train over test and calculating the accuracy of model chosen
month.model <- vector("list",332)
for (i in 1:332) {
  month.model[[i]]<-forecast(ets(BC.month.train[[i]],model = "AAA", damped = TRUE),h=nrow(BC.month.test[[i]]))
}

## calculating mean MASE on test set 
month_MASE<-vector("list", 332)
for (i in 1:332) {
  month_MASE[[i]] <- MASE.forecast(BC.month.train[[i]], BC.month.test[[i]], month.model[[i]]$mean)  
}

month_MASE_list = 0
for (j in 1:332){
  month_MASE_list = month_MASE_list + month_MASE[[j]]
}
month.MASE <- month_MASE_list/332
month.MASE

## residual diagnostics
month_non.Norm.Std.R <- 0
month_corr.Std.R <-0
for(i in 1:332){
  if (shapiro.test(month.model[[i]]$residuals)$p.value < 0.05){
    month_non.Norm.Std.R <- month_non.Norm.Std.R + 1
  }  
  if (Box.test(month.model[[i]]$residuals, lag = 1, type = "Ljung-Box", fitdf = 0)$p.value < 0.05){
    month_corr.Std.R <- month_corr.Std.R + 1
  }
  
}

## calculating percentage of non normal standard residuals
100*month_non.Norm.Std.R/332
## calculating percentage of correlated standard residuals
100*month_corr.Std.R/332

## ======== End of Monthly data analysis ======== ##


## ======== Computing penalty value ======== ##

Total_non.Norm.Std.R <- month_non.Norm.Std.R + quarter_non.Norm.Std.R + year_non.Norm.Std.R
Total_non.Norm.Std.R

Total_corr.Std.R <- month_corr.Std.R + quarter_corr.Std.R + year_corr.Std.R
Total_corr.Std.R

## ======== Forecasting for 6(yearly), 8(quarterly) and 18(monthly) periods ======== ##

year.forecasts <- vector("list",333)
quarter.forecasts <- vector("list",332)
month.forecasts <- vector("list",332)

for (i in 1:333) { 
  for (j in 1:332) { 
    for (k in 1:332) {
      year.forecasts[[i]]<-forecast(ets(BC.year.complete[[i]],model = "AAN", damped = TRUE),h=6)
      quarter.forecasts[[j]]<-forecast(ets(BC.quarter.complete[[j]],model = "AAA", damped = TRUE),h=8)
      month.forecasts[[k]]<-forecast(ets(BC.month.complete[[k]],model = "AAA", damped = TRUE),h=18)
    }
  }
}

final.year.forecasts <- list()
final.quarter.forecasts <- list()
final.month.forecasts <- list()
for (i in 1:333) { 
  for (j in 1:332) { 
    for (k in 1:332) {
      final.year.forecasts[[i]]<-((year_lambda[[i]])*(year.forecasts[[i]]$mean) + 1)^(-year_lambda[[i]])
      final.quarter.forecasts[[j]]<-(quarter_lambda[[j]]*quarter.forecasts[[j]]$mean + 1)^(-quarter_lambda[[j]])
      final.month.forecasts[[k]]<-(month_lambda[[k]]*month.forecasts[[k]]$mean + 1)^(-month_lambda[[k]])
    }
  }
}

