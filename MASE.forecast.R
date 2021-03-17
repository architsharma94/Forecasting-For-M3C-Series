MASE.forecast = function(training, test, forecasts){
  # training: Training set, should be vector. 
  # test: Test set, should be vector. 
  # forecasts: Forecasts obtained by the best model, should be vector. 
  # The number of forecasts should be the same as the lenght of test set.
  n = length(training)
  e.t = test - forecasts
  sum = 0 
  for (i in 2:n){
    sum = sum + abs(training[i] - training[i-1] )
  }
  q.t = e.t / (sum/(n-1))
  MASE = mean(abs(q.t))
  return(MASE = MASE)
}