MASEvalues = function(data, H, model, MASEs){
  # MASEs: All MASE values resulting from a model selection procedure
  N = length(data) # The number of considered time series
  MASEs = sort(MASEs)
  MASE.model = array(NA, N)
  MASE.rank = array(NA, N)
  fit.models = list()
  for ( j in 1:N){
    fit.models[[j]] = ets(data[[j]], model = model, damped = FALSE)
    MASE.model[j] = accuracy(fit.models[[j]])[6]
    MASE.rank[j] = which(MASE.model[j] == MASEs)
  }
  mean.rank.MASE = mean(MASE.rank)
  # Mean of MASE values over all considered datasets based on the best model
  # which is selected by a particular model selection procedure
  mean.MASE = mean(MASE.model)
  median.MASE = median(MASE.model)
  return(list(mean.rank.MASE = mean.rank.MASE, mean.MASE = mean.MASE, median.MASE = median.MASE))
}