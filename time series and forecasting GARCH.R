# Time Series and Forecasting 


#1  Check your working directory
getwd()

#2  Set your working directory to "ANLY 580/RScript". 
setwd("~/Desktop/ANLY 580:RScript")

#3  Download "Inflation.xls" data file and set the "observation_date" 
#   variable to the date format and the "CPI" variable to the numeric format.
#   The "CPI" variable represents the consumer price index,
#   which indicates the relative prices of a consumer basket. 
library(readxl)
infl_raw <- read_excel("~/Downloads/Inflation.xls")
str(infl_raw)
head(infl_raw)

infl_raw$observation_date <- as.Date(infl_raw$observation_date)
infl_raw$CPI <- as.numeric(infl_raw$CPI)


#4  Create two stand alone variables: "date" and "cpi". 
#   "date" variable should represent the values of the "observation_date" 
#   variable from the "Inflation" data set, while, "cpi" variable should represent 
#   values of the "cpi" variable from the "Inflation" data set.

date <- infl_raw$observation_date
cpi  <- infl_raw$CPI
head(date)
head(cpi)


#5 Transform "cpi" variable from numeric format to the time series format 
#   by using ts() function. Label the new variable as "cpits".  
start_yr  <- as.integer(format(min(date), "%Y"))
start_mon <- as.integer(format(min(date), "%m"))
cpits <- ts(cpi, start = c(start_yr, start_mon), frequency = 12)



#6 Please construct the following three graphs:
#  1)time series plot, 2) autocorrelation
#  and 3) partial autocorrelation functions for the "cpits" variable. 
#  Based on the signature of these graphs, does the variable appear
#  stationary? Explain

##the variable does not appear to be stationary,as it shows an upward trend

##1)

par(mfrow = c(1,1)); plot.ts(cpits, main = "CPI (cpits) – Time Series",
                             xlab = "Time",
                             ylab = "Index")
##2)
acf(cpits, main = "ACF of cpits")

##3)
pacf(cpits, main = "PACF of cpits")


#7  Use "cpits" variable and window() function to create 2 new variables 
#   called "cpi.pre", "cpi.post". 
#   The "cpi.pre" should include all observations for the period starting from 
#   January of 1990 and up until October 2018.
#   The "cpi.post" should include all observations starting from November 2018.
#   and up until the last month in the dataset.

cpi.pre  <- window(cpits, start = c(1990, 1), end = c(2018, 10))
cpi.post <- window(cpits, start = c(2018, 11))


#8  Use time() function and "cpi.pre" variable to create a variable called "Time".
#   Moreover, use "cpi.pre" variable and cycle() function to create 
#   a factor variable titled "Seas".

Time <- time(cpi.pre) 
Seas <- factor(cycle(cpi.pre))  

#9  Use lm() function to estimate parameter values of a linear regression model 
#   by regressing "Time", and "Seas" on "cpi.pre". 
#   Save these estimates as "cpi.lm".
#   Set the value of the intercept to 0, in order to interpret the 
#   coefficients of the seasonal dummy variables as seasonal intercepts. 
#   (Setting intercept to 0 ensures that for each season there is a unique intercept) 
#   Save these estimates as cpi.lm

cpi.lm <- lm(cpi.pre ~ 0 + Time + Seas)


#10 Create the following new items: 
#   "new.Time"- sequence of 12 values starting from 2018.75+1/12
#    and each number going up by 1/12
#   "new.Seas"- a vector with the following values c(11,12,1,2,3,4,5,6,7,8,9,10)
#   "new.data"- a data frame that combines the "new.Time" and "new.Seas" variables.

new.Time <- seq(2018.75 + 1/12, by = 1/12, length.out = 12)
new.Seas <- factor(c(11,12,1,2,3,4,5,6,7,8,9,10), levels = levels(Seas))
new.data <- data.frame(Time = new.Time, Seas = new.Seas)


#11 Use predict() function and cpi.lm model to create a 12 month ahead forecast 
#   of the consumer price index. Save this forecast as "predict.lm"

predict.lm <- predict(cpi.lm, newdata = new.data)


#12 Collect residuals from the "cpi.lm" model and save them as "cpi.lm.resid".
#   Moreover, construct acf and pacf for the "cpi.lm.resid" series. 
#   Is the series stationary?

## the series is non-stationary

#   Is there autocorrelation in the residual series?

##There is autocorrelation in the residual series

cpi.lm.resid <- resid(cpi.lm)
acf(cpi.lm.resid, main = "ACF of cpi.lm.resid")
pacf(cpi.lm.resid, main = "PACF of cpi.lm.resid")

#13  Based on the AIC, identify the best order of ARMA model 
#   (without the seasonal component) for the cpi.lm.resid time series 
#   and estimate the value of the parameter coefficients. 
#   Please, consider any ARMA model with up to 3 AR and/or MA terms.
#   Save these estimates as resid.best.arma.
#   What is the order of resid.best.arma?
## [1] 1 0 2, the order of resid.best.arma is (1,0,2)
best.aic <- Inf
best.order <- c(0,0,0)
best.fit <- NULL

for (p in 0:3) {
  for (q in 0:3) {
    fit <- try(arima(cpi.lm.resid, order = c(p,0,q)), silent = TRUE)
    if (!inherits(fit, "try-error")) {
      aic <- AIC(fit)
      if (aic < best.aic) {
        best.aic <- aic
        best.order <- c(p,0,q)
        best.fit <- fit}}}}
resid.best.arma <- best.fit
resid.best.arma_order <- best.order
resid.best.arma_order

#14 Use predict() function and resid.best.arma to 
#   create a 12 period ahead forecast of cpi.lm.resid series.
#   Save the forecasted values as resid.best.arma.pred

resid.best.arma.pred <- predict(resid.best.arma, n.ahead = 12)


#15 Use ts() function to combine the cpi values forecaseted by cpi.lm model
#   and the residual values forecasted by resid.best.arma.
#   Lable this time series as cpi.pred
cpi.pred <- ts(as.numeric(predict.lm) + as.numeric(resid.best.arma.pred$pred),
               start = c(2018, 11), frequency = 12)


#16 Use ts.plot() function to plot cpi.pre and cpi.pred together on one graph.
#   What do you expect will happen to the CPI during the next 12 month?

##The trend goes upwards in the plot,so CPI is expected to increase during the next 12 month

ts.plot(cpi.pre, cpi.pred, lty = c(1,2), main = "CPI: History vs 12-month Forecast",
        ylab = "Index")

legend("topleft", legend = c("Historical (to 2018-10)","Forecast (2018-11..2019-10)"),
       lty = c(1,2), bty = "n")

#17 Please calculate mean absolute percentage error for the cpi.pred
#   forecast for the first three month (November 2018, December 2018, January 2019)
#   How accurate is the model? 
## [1] 0.5375443,with a smaller MAPE,the model is very accurate

actual_3 <- window(cpits, start = c(2018,11), end = c(2019,1))
pred_3   <- window(cpi.pred, start = c(2018,11), end = c(2019,1))
mape_3   <- mean(abs((actual_3 - pred_3) / actual_3)) * 100
mape_3

#18 What is the forecasted rate of inflation between December 2018 and January 2019?
#   Hint: Inflation = % change in CPI

## The forecasted rate of inflation is 0.2837
str(cpi.pred)
diff((log(cpi.pred[1:3]))*(100))

##diff((log(cpi.pred[1:3]))*(100))
##[1] -0.0837804  0.2837307

#19 Policy makers often care more about inflation rather than cpi.
#   Create a new stand alone variable that would represent 
#   the first log difference of the the cpits variable. 
#   Label this variable  "pi", which represents monthly inflation rate in the US.
#   If percentage change is positive there is inflation (prices go up), 
#   and if the percentage change is negative there is deflation (prices fall). 
#   What was the lowest monthly rate of inflation(deflation) recorded in US
#   during the time sample? What about was the highest?
##The lowest monthly rate of inflation(deflation) was August 1921
##The highest was May of 1946

pi <- diff(log(cpits)) * 100
pi_min_val <- min(pi, na.rm=TRUE)
pi_min_when <- time(pi)[which.min(pi)]
pi_max_val <- max(pi, na.rm=TRUE)
pi_max_when <- time(pi)[which.max(pi)]
pi_min_val; pi_min_when
pi_max_val; pi_max_when

##[1] -3.208831
##[1] 1921.083
##> pi_max_val; pi_max_when
##[1] 5.715841
##[1] 1946.5

#20 Please construct the time series plot, the autocorrelation
#  and partial autocorrelation functions for the "pi" variable. 
#  Based on the signature of these graphs, does the variable appear
#  stationary? Explain

##The variable appear non-stationary
##the plots show decreasing ACF and PACF patterns indicates strong autocorrelation.

par(mfrow = c(1,1)); plot.ts(pi, main="Monthly Inflation (pi)", ylab="%")
acf(pi, main="ACF of pi")
pacf(pi, main="PACF of pi")

#21  Use "pi" variable and window() function to create 2 new variables 
#   called "pi.pre", "pi.post". 
#   The "pi.pre" should include all observations for the period starting from 
#   January of 1990 and up until October 2018.
#   The "pi.post" should include all observations starting from November 2018.
#   and up until the last month in the dataset.

pi.pre  <- window(pi, start = c(1990,1), end = c(2018,10))
pi.post <- window(pi, start = c(2018,11))


#22 Please create a function that takes a time series as input, 
#   and then uses AIC to identify the best SARIMA model. 
#   The function should return the following:
#   - the order of the best SARIMA, 
#   - its AIC
#   - and the estimates of its coefficient values
#   Lable this formula get.best.sarima

get.best.sarima <- function(x, max.order = list(p=2,d=2,q=2,P=2,D=2,Q=2),
                            seasonal.period = 12) {
  best.aic <- Inf
  best.fit <- NULL
  best.order <- NULL
  
  for (p in 0:max.order$p) for (d in 0:max.order$d) for (q in 0:max.order$q) {
    for (P in 0:max.order$P) for (D in 0:max.order$D) for (Q in 0:max.order$Q) {
      fit <- try(Arima(x,
                       order = c(p,d,q),
                       seasonal = list(order = c(P,D,Q), period = seasonal.period),
                       include.constant = TRUE),
                 silent = TRUE)
      if (!inherits(fit, "try-error")) {
        aic <- AIC(fit)
        if (aic < best.aic) {
          best.aic <- aic
          best.fit <- fit
          best.order <- list(order = c(p,d,q), seasonal = c(P,D,Q))
        }
      }
    }
  }
  return(list(model = best.fit,
              order = best.order,
              aic = best.aic,
              coefficients = coef(best.fit)))}


#23 By using get.best.sarima() function please identify the best SARIMA model
#   for pi.pre time series. 
#   Please cosider SARIMA(2,2,2,2,2,2) as the maximum order of the model. 
#   Save the results of the get.best.sarima() function as "pi.best.sarima"
#   What is the order of the best SARIMA model?

## the order of the best SARIMA model is (0,0,2) and seasonal(1,0,2).

pi.best.sarima <- get.best.sarima(pi.pre, max.order = list(p=2,d=2,q=2,P=2,D=2,Q=2))
pi.best.sarima$order
pi.best.sarima$aic
pi.best.sarima$coefficients
print(pi.best.sarima)


# 24 Please use predict() function and the best.sarima.pi model to forecast
#    monthly rate of inflation in the US during November 2018, December 2018
#    and January 2019.
#    Save these predictions as pi.sarima.pred

library(forecast)
pi.sarima.pred <- forecast(pi.best.sarima$model, h = 3)
pi.sarima.pred

#25 Please calculate mean absolute percentage error of the best.sarima model.
#   How accurate is the model? 

##The model is reasonably accurate
pi.sarima.forecast <- predict(pi.best.sarima$model, n.ahead = 3)$pred
actual_pi <- window(pi, start = c(2018, 11), end = c(2019, 1))
mape_sarima <- mean(abs((actual_pi - pi.sarima.forecast) / actual_pi)) * 100

#26 Extract the residual series from the pi.best.sarima model,
#   and save them as sarima.resid.

sarima.resid <- residuals(pi.best.sarima$model)
sarima.resid <- na.omit(sarima.resid)

#27 Plot the acf of the sarima.resid series and acf of the sarima.resid^2 series 
#   What can you conclude based on these graphs?
## There is autocorrelation in the residuals,so GARCH model is required

par(mfrow=c(1,2))
acf(sarima.resid, main="ACF of SARIMA residuals")
acf(sarima.resid^2, main="ACF of squared SARIMA residuals")

#28 Download fGarch package and upload it to the library

install.packages("fGarch")
library(fGarch)

#29 Use garchFit() function from the fGarch package 
#   to estimate garch(1,1) model of the sarima.resid time series. 
#   By doing so you will be able to analyze the volatility of the 
#   inflation, or, in other words,  how stable it is.
#   Save the estimated coefficients as resid.garch

library(fGarch)

resid.garch <- garchFit(~ garch(1,1),
                        data = sarima.resid,
                        include.mean = FALSE,
                        trace = FALSE)
summary(resid.garch)

#30 The main priority of the monetary authority (Federal Reserve)
#   in the United States is to ensure stable value of currency. 
#   Simply put, Fed wants to keep inflation stable (no volatility). 
#   To maintain stability the Fed depends on a number of tools, 
#   and its effectiveness is judged based on the forecasting model of volatility.
#   Please use resid.garch variable and predict function 
#   to forecast two period ahead inflation volatility, which is measured by 
#   a square of the forecasted standard deviation.
#   How stable will be the currency incurrency in February and March of 2019?

##February 2019:0.1297701,March 2019: 0.1358448.
##The currency is relatively stable with a slight increase from Feb to March of 2019

library(fGarch)

garch.forecast <- predict(resid.garch, n.ahead = 2)
volatility.forecast <- garch.forecast$standardDeviation^2
volatility.forecast
