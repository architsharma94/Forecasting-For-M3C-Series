pVal = function(data, H, models){
  M = length(models) # The number of competing models
  N = length(data) # The number of considered time series
  n = array(NA, N) # Array to hold the length of each series
  for ( j in 1:N){ # Find the length of each series
    n[j] = length(data[[j]]) 
  }
  n.max = max(n)
  n.star = n - H
  
  # Fit the models
  fit.models = list()
  forecasts = list()
  MASEvalues = array(NA, N*M)
  count = 0
  for ( j in 1:N ){ # For each series of length nj*
    for ( i in 1:M){ # Fit all models
      count = count + 1
      fit.models[[count]] = ets(ts(data[[j]][1:n.star[j]],frequency = frequency(data[[j]])), model = models[i])
      forecasts[[count]] = forecast(fit.models[[count]])$mean
      MASEvalues[count] = accuracy(fit.models[[count]])[6]
    }
  }
  
  ASE = array(NA, dim = c(H, M, N))
  MASE.1 = array(NA, dim = c(M, N))
  MASE = array(NA, N)
  MASE.model = array(NA, N)
  
  summ = 0
  for (h in 1:H){
    summ = 0
    count = 0
    for ( j in 1:N){
      MAE = 0
      for ( t in 2:n[j] ){
        MAE = MAE + abs(data[[j]][t] - data[[j]][t-1])
      }
      for ( i in 1:M ){
        count = count + 1
        ASE[h, i , j] = sum( abs(data[[j]][(n.star[j] + 1):(n.star[j]+h)] - forecast(fit.models[[count]], h = h)$mean) / MAE )
      }
    }
  }
  
  for ( j in 1:N){
    for ( i in 1:M ){
      MASE.1[i , j] = 0
      for (h in 1:H){
        MASE.1[i , j] = MASE.1[i , j] + ASE[h, i , j] 
      }
      MASE.1[i , j] = MASE.1[i , j]  / H
    }
    MASE[j] = min(MASE.1[ , j])
    MASE.model[j] = models[which(MASE[j] == MASE.1[ , j] )]
  }
  
  return(list(MASE = MASE, best.model = MASE.model, MASEs = MASEvalues))
}