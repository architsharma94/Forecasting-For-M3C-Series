library(forecast)
library(expsmooth)
library(readr)

data("ausgdp")
fit.ausgdp <- ets(ausgdp,"AAN" , damped=FALSE , upper=rep(1,4))
n = length(ausgdp)
q = (length(fit.ausgdp$par)+1)
AIC = -2*fit.ausgdp$loglik + 2*q
BIC = -2*fit.ausgdp$loglik + q*log(n)
AICc = -2*fit.ausgdp$loglik + 2*q*n/(n-q-1)
HQIC = -2*fit.ausgdp$loglik+ 2*q*log(log(n))
summary(fit.ausgdp)


m3Annual <- read_csv("~/Documents/Haydar/MATH1307_Forecasting/notes/Module 9/m3Annual.csv")
m3AnnualInfo <- read_csv("~/Documents/Haydar/MATH1307_Forecasting/notes/Module 9/m3AnnualInfo.csv")


data("ausgdp")
data("usgdp")

usGDP = window(usgdp,start = c(1971,3), end=c(1998,1))

plot(ausgdp)

plot(usGDP)

data = list()
data[[1]] = ts(ausgdp, start = c(1971,3), frequency = 4)
data[[2]] = ts(usGDP, start = c(1971,3), frequency = 4)

H = 5

models = c("ANN", "MNN", "AAN", "AAA")

a = GoFVals(data = data, H = H, models = models)

MASEs = a$GoF$MASE

MASEvalues(data = data, H = H, model = "AAN", MASEs = MASEs)

LEIC(data = data, H = H, models = models)

b = pVal(data = data, H = H, models = models)

MASEs = b$MASEs

MASEvalues(data = data, H = H, model = "AAN", MASEs = MASEs)

meanMaxTempMel <- read_csv("~/Documents/MATH1307_Forecasting/presentations/Module 9/meanMaxTempMel.csv", col_names = FALSE) 
meanMaxTempMel = ts(meanMaxTempMel,start=c(1971,1), end = c(2017, 8), frequency=12)

meanMaxTempSyd <- read_csv("~/Documents/MATH1307_Forecasting/presentations/Module 9/meanMaxTempSyd.csv", col_names = FALSE)
meanMaxTempSyd = stack(as.data.frame(t(meanMaxTempSyd)))
meanMaxTempSyd = ts(meanMaxTempSyd$values,start=c(1971,1), end = c(2017, 8),frequency=12)

meanMaxTempPer <- read_csv("~/Documents/MATH1307_Forecasting/presentations/Module 9/meanMaxTempPerth.csv", col_names = FALSE)
meanMaxTempPer = stack(as.data.frame(t(meanMaxTempPer)))
meanMaxTempPer = ts(meanMaxTempPer$values,start=c(1971,1), end = c(2017, 8), frequency=12)

meanMaxTempDar <- read_csv("~/Documents/MATH1307_Forecasting/presentations/Module 9/meanMaxTempDarwin.csv", col_names = FALSE)
meanMaxTempDar = stack(as.data.frame(t(meanMaxTempDar)))
meanMaxTempDar = ts(meanMaxTempDar$values,start=c(1971,1), end = c(2017, 8), frequency=12)

meanMaxTempAde <- read_csv("~/Documents/MATH1307_Forecasting/presentations/Module 9/meanMaxTempAdel.csv", col_names = FALSE)
meanMaxTempAde = stack(as.data.frame(t(meanMaxTempAde)))
meanMaxTempAde = ts(meanMaxTempAde$values,start=c(1971,1), end = c(2017, 8), frequency=12)

par(mfrow=c(1,1))
plot(meanMaxTempMel, ylab = "Degrees C",  main = "Monthly mean maximum temperature in Melbourne")
par(mfrow=c(1,2))
acf(meanMaxTempMel, main = "Sample ACF of monthly mean maximum temperature in Melbourne")
pacf(meanMaxTempMel, main = "Sample PACF of monthly mean maximum temperature in Melbourne")

par(mfrow=c(1,1))
plot(meanMaxTempSyd, ylab = "Degrees C",   main = "Monthly mean maximum temperature in Sydney")
par(mfrow=c(1,2))
acf(meanMaxTempSyd, main = "Sample ACF of monthly mean maximum temperature in Sydney")
pacf(meanMaxTempSyd, main = "Sample PACF of monthly mean maximum temperature in Sydney")

par(mfrow=c(1,1))
plot(meanMaxTempPer, ylab = "Degrees C",   main = "Monthly mean maximum temperature in Perth")
par(mfrow=c(1,2))
acf(meanMaxTempPer, main = "Sample ACF of monthly mean maximum temperature in Perth")
pacf(meanMaxTempPer, main = "Sample PACF of monthly mean maximum temperature in Perth")

par(mfrow=c(1,1))
plot(meanMaxTempDar, ylab = "Degrees C",   main = "Monthly mean maximum temperature in Darwin")
par(mfrow=c(1,2))
acf(meanMaxTempDar, main = "Sample ACF of monthly mean maximum temperature in Darwin")
pacf(meanMaxTempDar, main = "Sample PACF of monthly mean maximum temperature in Darwin")

par(mfrow=c(1,1))
plot(meanMaxTempAde, ylab = "Degrees C",   main = "Monthly mean maximum temperature in Adelaide")
par(mfrow=c(1,2))
acf(meanMaxTempAde, main = "Sample ACF of monthly mean maximum temperature in Adelaide")
pacf(meanMaxTempAde, main = "Sample PACF of monthly mean maximum temperature in Adelaide")

data = list()
data[[1]] = meanMaxTempMel
data[[2]] = meanMaxTempSyd
data[[3]] = meanMaxTempPer
data[[4]] = meanMaxTempDar
data[[5]] = meanMaxTempAde

H = 5

models = c("ANN", "MNN", "ANA", "MNA", "MNM")

GoFVals(data = data, H = H, models = models)

LEIC(data = data, H = H, models = models)

pVal(data = data, H = H, models = models)
