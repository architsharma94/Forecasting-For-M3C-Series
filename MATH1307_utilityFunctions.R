

residual.analysis <- function(model, std = TRUE, Ljung.Box = FALSE, start = 2, class = c("LM")[1]){
  # If you have an output from arima() function use class = "ARIMA"
  # If you have an output from garch() function use class = "GARCH"
  # If you have an output from ugarchfit() function use class = "ARMA-GARCH"
  library(TSA)
  library(FitAR)
  if (class == "LM"){
    if (std == TRUE){
      res.model = scale(residuals(model),center= TRUE, scale = TRUE)
      res.type = " standardised"
    }else{
      res.model = residuals(model)
      res.type = ""
    }
  }else if (class == "GARCH"){
    res.model = model$residuals[start:model$n.used]
  }else if (class == "ARMA-GARCH"){
    res.model = model@fit$residuals
  }else {
    stop("The argument 'class' must be either 'ARIMA' or 'GARCH' ")
  }
  par(mfrow=c(3,2))
  plot(res.model,type='o',ylab=paste0(res.type,"residuals"), main=paste0("Time series plot of", res.type, " residuals"))
  abline(h=0)
  hist(res.model,main=paste0("Histogram of", res.type, " residuals"))
  acf(res.model,main=paste0("ACF of", res.type, " residuals"))
  pacf(res.model,main=paste0("PACF of", res.type, " residuals"))
  qqnorm(res.model,main=paste0("QQ plot of", res.type, " residuals"))
  qqline(res.model, col = 2)
  print(shapiro.test(res.model))
  if (Ljung.Box){
    k=0
    LBQPlot(res.model, lag.max = 30, StartLag = k + 1, k = 0, SquaredQ = FALSE)
  }
}

# AIC and BIC sorting function by Cameron Doyle
sort.score <- function(x, score = c("bic", "aic")){
  if (score == "aic"){
    x[with(x, order(AIC)),]
  } else if (score == "bic") {
    x[with(x, order(BIC)),]
  } else {
    warning('score = "x" only accepts valid arguments ("aic","bic")')
  }
}
# AIC and BIC sorting function by Cameron Doyle