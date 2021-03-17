GoFVals_damped = function(data, H, models){
  M = length(models) # The number of competing models
  N = length(data) # The number of considered time series
  fit.models = list()
  series = array(NA, N*M)
  FittedModels = array(NA, N*M)
  AIC = array(NA, N*M)
  AICc = array(NA, N*M)
  BIC = array(NA, N*M)
  HQIC = array(NA, N*M)
  MASE = array(NA, N*M)
  # mean.MASE = array(NA, N)
  # median.MASE = array(NA, N)
  GoF = data.frame(series, FittedModels, AIC, AICc, BIC, HQIC, MASE)
  count = 0
  for ( j in 1:N){
    # sum.MASE = 0
    # sample.median = array(NA, M)
    for ( i in 1: M){
      count = count + 1
      fit.models[[count]] = ets(data[[j]], model = models[i], damped = TRUE)
      GoF$AIC[count] = fit.models[[count]]$aic
      GoF$AICc[count] = fit.models[[count]]$aicc
      GoF$BIC[count] = fit.models[[count]]$bic
      q = length(fit.models[[count]]$par)
      GoF$HQIC[count] = -2*fit.models[[count]]$loglik+ 2*q*log(log(length(data[[j]])))
      GoF$MASE[count] = accuracy(fit.models[[count]])[6]
      # sum.MASE = sum.MASE + GoF$MASE[count]
      # sample.median[i] = GoF$MASE[count]
      GoF$series[count] = j
      GoF$FittedModels[count] = models[i]
    }
    # mean.MASE[j] = sum.MASE / N
    # median.MASE[j] = median(sample.median)
  }
  return(list(GoF = GoF))#, mean.MASE = mean.MASE, median.MASE = median.MASE))
}



