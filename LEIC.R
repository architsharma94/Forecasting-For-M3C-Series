LEIC = function(data, H, models){
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
  count = 0
  for ( j in 1:N ){ # For each series of length nj*
    for ( i in 1:M){ # Fit all models
      count = count + 1
      fit.models[[count]] = ets(ts(data[[j]][1:n.star[j]],frequency = frequency(data[[j]])), model = models[i])
    }
  }
  
  c = seq(0.15 , 2*log(n.max) , 0.05)
  ASE = array(NA, dim = c(H, length(c), N))
  MASE = array(NA, dim = c(H, length(c)))
  leic = array(NA, count)
  for ( k in 1:length(c)){
    count = 0
    for ( j in 1:N ){
      for ( i in 1:M){
        count = count + 1
        q = length(fit.models[[count]]$par) # the number of parameters + the number of free states
        leic[count] = -2*fit.models[[count]]$loglik + q * c[k]
      }
    }

    best.model = fit.models[[which(leic == min(leic))]]# gives the order of the model with miniumum leic
    summ = 0
    for (h in 1:H){
      summ = 0
      for ( j in 1:N){
        summ.2 = 0
        for ( t in 2:n[j] ){
          summ.2 = summ.2 + abs(data[[j]][t] - data[[j]][t-1])
        }
        ASE[h, k , j] = sum( abs(data[[j]][(n.star[j] + 1):(n.star[j]+h)] - forecast(best.model, h = h)$mean) / summ.2 )
        summ = summ + ASE[h, k , j]
      }
      MASE[h, k] = summ / N
    }
  }
  ch = array(NA, H)
  for ( h in 1:H){
    ch[h] = MASE[h, min( which(min(MASE[h,]) == MASE[h, ]) )] # the first min is to take minimum of minimums
  }
  c.opt = mean(ch)
  
  series = array(NA, N*M)
  FittedModels = array(NA, N*M)
  values = array(NA, N*M)
  leic = data.frame(series, FittedModels, values)
  count = 0
  for ( j in 1:N ){
    for ( i in 1:M){
      count = count + 1
      q = length(fit.models[[count]]$par) # the number of parameters + the number of free states
      leic$series[count] = j
      leic$FittedModels[count] = models[i]
      leic$values[count] = -2*fit.models[[count]]$loglik + q * c.opt
    }
  }

  return(list(leic = leic, c.opt = c.opt))
}
