# Forecasting-For-M3C-Series

# Repository guide:

* Data => M3C_reduced_2019.xlsx
* Code => code.R
* Analysis and Reporting => Forecasting for M3C Series
* Supporting script => GoFVals.R, GoFVals_damped.R, LEIC.R, MASE.forecast.R, MASEvalues.R, MASEvalues_damped.R, MATH1307_utilityFunctions.R, mdl9_scripts.R, pVal.R

## Introduction

The M3C dataset (International Institute of Forecasters, 2019) consists of 997 time series which is segregated under 3 time periods i.e. yearly, quarterly and monthly and 5 primary categories which are micro-economic, macro-economic, industrial, financial, and demographic. Each of the series have different lengths and time range.

Phillips-Perron Unit Root Test, executed for the 3 periods, confirmed that only 37% of series were stationary in terms of statistical mean and variance. 63% of the time series pertaining to monthly period was observed to be stationary. Majority of the time series belonging to quarterly and yearly data was found to be non-stationary. 

## Objective

The primary purpose of this analysis was to find the best fitting model of the given 997 time series by estimating and comparing the accuracy of different state-space models of forecasting for each time series in the dataset.

## Methodology 	

Each series in the dataset was bifurcated into training set (95%) and test set (5%). The following steps were performed thereafter:

### Data Transformation

Since, majority of the time series were found to be non-stationary, box-cox transformation was applied on the time series to handle the changing variance over time.

The value of lambda was calculated using the BoxCox.lambda() function, and used in the following formula of transformation:

Z =  (Y^λ-1)/λ
 
where, Z = Transformed value and Y = Original data

The new datasets were checked for any zero or negative values, post transformation.


### Comparing Information Criteria on Train Set

State-space (Error-Trend-Seasonality) models were used for the modelling purpose. The ETS models were fit to each series considering the presence of trend and seasonality in the training sets, whose information criteria values were compared. Subsequently, the model with highest model-fit frequency, along with the least AIC-BIC-HQIC values, was considered for further analysis.  This task was iterated for damped and undamped Trend component using GoFVals() function.

### MASE Comparison for Train Set

The MASE values and rank were computed for the shortlisted models using MASEvalues() function.
For each of the 3 type of time series, the model with the least MASE value and MASE rank was selected as the best fit.

### Model Fitting on Test Set

The training set, containing 95% of the data, was used to forecast for the next 5% of time period using the best-fit model realized in the previous step. The quality of forecasts for each series was then checked against the 5% test set. Average of MASE values of model-fitting on train and test set was calculated for each of the 997 time series. 

### Residual Diagnostics

For the chosen models, the total number of non-normal standardized residuals was computed using Shapiro-Wilk’s normality test. Similarly, the total number of standardized residuals with significant serial correlation was calculated using Ljung-Box test.

### Forecasting 

Best fitted models were used to calculate the MASE values over the forecasts for 6, 8 and 18 times-ahead forecasts for yearly, quarterly and monthly periods, respectively. The forecasts obtained were converted to their non-transformed version based on the equation below:

Y=〖(λZ+1)〗^(-λ)
where, Y = actual forecast and Z = transformed forecasts

## Conclusion

The best-fit ETS models, chosen for forecasting for the respective groups of time series are as follows:

* Monthly Time Series – Additive Error, Additive Damped Trend, Additive Seasonality (A,Ad,A)
* Quarterly Time Series – Additive Error, Additive Damped Trend, Additive Seasonality (A,Ad,A)
* Yearly Time Series – Additive Error, Additive Damped Trend, Additive Seasonality (A,Ad,A)

With the ETS models chosen for the 997 time series, over 66% of the standardized residuals were found to be normally distributed, with approximately 85% of the standardized residuals showing no significant serial correlation.
