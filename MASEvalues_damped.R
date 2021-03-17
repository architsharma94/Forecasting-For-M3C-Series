MASEvalues_damped = function(data, H, model, MASEs){
  # MASEs: All MASE values resulting from GoFVals() function
  N = length(data) # The number of considered time series
  MASEs = sort(MASEs)
  MASE.model = array(NA, N)
  MASE.rank = array(NA, N)
  fit.models = list()
  for ( j in 1:N){
    fit.models[[j]] = ets(data[[j]], model = model, damped = TRUE)
    MASE.model[j] = accuracy(fit.models[[j]])[6]
    MASE.rank[j] = which(MASE.model[j] == MASEs)
  }
  mean.rank.MASE = mean(MASE.rank)
  mean.MASE = mean(MASE.model) # O procedure ile secilen modelin j tane veri kumesi uzerinden ortalamasi
  median.MASE = median(MASE.model)
  return(list(mean.rank.MASE = mean.rank.MASE, mean.MASE = mean.MASE, median.MASE = median.MASE))
}