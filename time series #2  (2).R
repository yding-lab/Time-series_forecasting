# Time Series and Forecasting 



#1  Check you working directory
getwd()
list.files()


#2  Set your working directory to "ANLY 565/RScript". 
#   Upload "nlme" library

install.packages("nlme")
library(nlme)

#3  Download "trade.xls" data file and set the "date" 
#   variable to the date format and the "trade" variable to
#   the numeric format. The "trade" variable represents 
#   the Ratio of Exports to Imports for China expressed in percentages.
library(readxl)
trade_data <- read_excel("~/Downloads/trade.xls")
trade_data$date <- as.Date(trade_data$date) 
trade_data$trade <- as.numeric(trade_data$trade)

#4  Create two stand alone variables: "datev" and "tradev". 
#   "datev" variable should represent values of the "date" variable 
#   from the "trade" data set, while, "tradev" variable should represent 
#   values of the "trade" variable from the "trade" data set.
datev <- trade_data$date
tradev <- trade_data$trade


#5  Use the "datev" variable and the range() function to check the time sample
#   covered by the "trade" data set. What time period is covered?
##[1] "1992-01-01" "2019-04-01",time period covered is from 1st day of 1992 to 1st of April of 2019

#   What is the frequency of the data?
##The data has a monthly frequency as the difference is about one month (28–31 days)
range(datev)

diff(datev)

#6  Transform "tradev" variable from numeric format to the time series format 
#   by using ts() function. Label the new variable as "tradets".  
start_year <- as.numeric(format(min(datev), "%Y"))
start_month <- as.numeric(format(min(datev), "%m"))
tradets <- ts(tradev, start = c(start_year, start_month), frequency = 12)



#7  Plot the time series graph of the "tradets"variable.
#   Please label all axis correctly, and make sure to label the graph. 
#   Based on this graph does the Ratio of Exports to Imports for China exhibit a trend? 
##The graph does not exhibit a certain trend,does not show any continuous increase or decrease

#   What about a regular seasonal fluctuation? 
##The plot does not show a regular seasonal fluctuation,but the statistical analysis later indicates a regular seasonal fluctuation

plot(tradets, main="China export/import ratio (%)", 
     xlab="Year", ylab="Trade Ratio (%)")


#8  Use "tradets" variable and window() function to create 2 new variables 
#   called "tradepre", "tradepost". 
#   The "tradepre" should include all observations for the period 
#   up until December 2018.(Last observation should be December 2018)
#   The "tradepost" should include all observations starting from January 2019.
#   and up until the last month in the dataset.
tradepre <- window(tradets, end = c(2018, 12))
tradepost <- window(tradets, start = c(2019, 1))


#9  Estimate autocorrelation function and partial autocorrelation function for 
#   the "tradepre" variable. Does the trade ratio for China exhibit autocorrelation?  
##Yes,it shows short term positive autocorrelation with the first few lags

#   What process can explain this time series (white noise, random walk, AR, etc..)?
##AR can explain this time series

acf(tradepre)
pacf(tradepre)


#10 Estimate AR(q) model for the "tradepre" time series. 
#   Use ar() function (set aic=FALSE) and rely on the corellologram 
#   to determine q, the order of the model. Moreover, use maximum likelihood method.
#   After that, set aic=TRUE and estimate ar() again to see if you have identified 
#   the order correctly.
#   Save the estimates as "trade.ar".
trade.ar <- ar(tradepre, aic=FALSE, method="mle")
trade.ar.aic <- ar(tradepre, aic=TRUE, method="mle")
trade.ar


#11 For each of the AR coefficients estimate 95% confidence interval
#   To find 95% confidence intervals you need to add and subtract 2
#   standard deviations of the coefficient estimates. 
#   Hint you can obtain these standard deviations by applying sqrt()
#   function to the diagonal elements of the asymptotic-theory variance 
#   matrix of the coefficient estimates
se <- sqrt(diag(trade.ar$asy.var.coef))
coef <- trade.ar$ar
ci_lower <- coef - 2*se
ci_upper <- coef + 2*se
cbind(coef, ci_lower, ci_upper)

#12 Extract the residuals from the trade.ar model and estimate 
#   the autocorrelation function. Based on this correlogram would you say 
#   trade.ar model does a good job of explaining the trade ratio in China?

##Yes,the trade.ar model is a good model to explain the trade ratio in China as its residuals are uncorrelated.
resid_ar <- trade.ar$resid
acf(resid_ar, na.action=na.pass)


#13 Use trade.ar model and predict() function to create a 4 period ahead forecast
#   of the trade ratio in China. Save these predicted values as "trade.ar.forc"
trade.ar.forc <- predict(trade.ar, n.ahead=4)$pred


#14 Use ts.plot() function to plot side-by-side actual values of the trade ratio
#   from January 2019-April 2019 period and their forecasted counterparts. 
#   (tradepost and trade.ar.forc)
#   Please designate red color to represent the actual observed values, 
#   and blue doted lines to represent forecasted values. 
#   How does the ability to predict future trade ratio depends on the 
#   time horizon of the forecast?

##The ability to predict the future trade ratio is less accurate as the time horizon of the forecast increases.

ts.plot(tradepost[1:4], trade.ar.forc, col=c("red","blue"), lty=c(1,2))
legend("topleft", legend=c("Actual","Forecast"), col=c("red","blue"), lty=c(1,2))


#15 Please calculate forecast's mean absolute percentage error 
#   for the trade.ar.forc forecasting model. Why is it important to calculate 
#   mean absolute percentage error rather than mean percentage error?

##the absolute percentage error make sure that the result reflects the actual value of the error
##when using mean percentage error,positive and negative errors can cancel each other

actual <- tradepost[1:4]
mape_ar <- mean(abs((actual - trade.ar.forc)/actual))*100
mape_ar
## [1] 6.467586

#16 Use time() function and tradepre variable to create a variable called "Time".

Time <- time(tradepre)


#17 Estimate linear regression model by regressing "Time" on "tradepre" variable.
#   USE OLS. Save this regression model as "trade.lmt". 
#   By using confint() function calculate 95% confidence intervals for the estimated 
#   model coeficients.
#   What can you conclude based on the estimates of the model coeficients?
##The coefficient is positive and statistically significant, so there is a positive trend
#   What is the direction of the time trend?
##The direction of the time trend is upward

trade.lmt <- lm(tradepre ~ Time)
confint(trade.lmt)
summary(trade.lmt)

Residuals:
#  Min      1Q  Median      3Q     Max 
#-29.906  -7.849  -2.364   6.662  57.431 

Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept) -1.113e+03  1.749e+02  -6.363 6.79e-10 ***
#  Time         6.130e-01  8.724e-02   7.027 1.26e-11 ***
  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

  
#Residual standard error: 12.24 on 322 degrees of freedom
#Multiple R-squared:  0.133,	Adjusted R-squared:  0.1303 
#F-statistic: 49.38 on 1 and 322 DF,  p-value: 1.263e-11

#18 By visually inspecting a time series plot of the "tradepre" variable, 
#   and given the seasonal nature of the trade relationships it is reasonable to assume 
#   that there are regular seasonal fluctuations in the trade ratio for China. 
#   Use "tradepre" variable and cycle() function to create a factor variable titled "Seas".

  Seas <- factor(cycle(tradepre))

#19 Use lm() function to estimate linear regression model by regressing 
#   "Time" and "Seas" on "tradepre". Save this regression model as "trade.lmts".
#   Set the value of the intercept to 0, in order to interpret the 
#   coefficients of the seasonal dummy variables as seasonal intercepts. 
#   (Setting intercept to 0 ensures that for each season there is a unique intercept)
#   What can you conclude based on the estimates of the model coefficients?

##With R-squared= 0.9892, p-value: < 2.2e-16 show that the model is a good fit for the data 

#   What is the direction of the time trend? Is there a seasonal component?
##The direction of the time trend is upward.

##There is a seasonal component as the coefficient is statistically significant

#   During which month should you expect the trade ratio to be the largest?
##Seas2 

trade.lmts <- lm(tradepre ~ 0 + Time + Seas)
confint(trade.lmts)
summary(trade.lmts)


#20 Extract the residual series from the "trade.lmts" model and save them as 
#   "trade.lmts.resid". Then, estimate autocorrelation function to check the 
#   goodness of the fit. What is the value of autocorrelation at lag 1?

## around 0.9

#   What can you conclude based on the correlogram of the residual series?
##The residuals show strong autocorrelation,so the model is not a good fit for this.

trade.lmts.resid <- residuals(trade.lmts)
acf(trade.lmts.resid)


#21 Fit linear model by regressing "Time" and "Seas" on "tradepre"
#   by utilizing generalized least squares (gls() function).
#   Set the value of the intercept to 0, in order to interpret the 
#   coefficients of the seasonal dummy variables as seasonal intercepts.
#   Save this model's estimates as "trade.gls".
trade.gls <- gls(tradepre ~ 0 + Time + Seas)
summary(trade.gls)



#22 Compute Akaike's An Information Criterion for "trade.lmts" and "trade.gls".
#   Which model performs better?

##The trade.gls model performs better,because lower AIC value indicates better model fit 


AIC(trade.lmts)
##[1] 2565.278

AIC(trade.gls)
##[1] 2525.645


#23 Create the following new variables: 
#   "new.Time"- sequence of 4 values starting from 2019 and each number going up by 1/12
#   "alpha" - assumes value of the Time coefficient from the trade.gls model
#   "beta" - takes on values of the first, second, third, and fourth seasonal coefficients 
#            from the trade.gls model.

new.Time <- seq(2019, 2019+3/12, by=1/12)
alpha <- coef(trade.gls)["Time"]
beta <- coef(trade.gls)[grep("Seas", names(coef(trade.gls)))]


#24 By using the forecasting equation of x_(t+1)<-0+alpha*Time_(t+1)+beta
#   create a 4 period ahead forecast of the trade ratio for China. 
#   Label this forecast as "trade.gls.forc"
trade.gls.forc <- alpha*new.Time + beta[1:4]


#25 Use ts.plot() function to plot side-by-side actual values of the trade ratio
#   from January 2019-April 2019 period and their forecasted counterparts. 
#   (tradepost and trade.gls.forecast)
#   Please designate red color to represent the actual observed values, 
#   and blue doted lines to represent forecasted values.

ts.plot(tradepost, trade.gls.forc, col=c("red", "blue"), lty=c(1,3))

#26 Please calculate forecast mean absolute percentage error 
#   for the "trade.gls.forc" forecasting model. Based on the 
#   forecast's mean absolute percentage error, which of the two models, 
#   "trade.ar.forc" and trade.gls.forc" performs better?

##trade.ar.forc performs better with lower MAPE of 6.46%

actual <- tradepost[1:4]

mape_gls <- mean(abs((actual - trade.gls.forc[1:4]) / actual)) * 100
mape_ar  <- mean(abs((actual - trade.ar.forc[1:4]) / actual)) * 100

mape_gls
#[1] 7.342898

mape_ar
#[1] 6.467586

#27 Create a variable called tradepreL, that represents the first lagged value
#   of the "tradepre" variable. For example tradepreL_t=tradepre_(t-1).
#   Moreover, transform "tradepreL" variable into a time series object by using ts().
#   It should cover the same time period as "tradepre".

tradepreL <- stats::lag(tradepre, -1)
tradepreLts <- ts(tradepreL, start=start(tradepre), frequency=12)


#28 Use lm() function to estimate linear regression model by regressing 
#   "tradepreL", "Time" and "Seas" on "tradepre". 
#   Set the value of the intercept to 0, in order to interpret the 
#   coefficients of the seasonal dummy variables as seasonal intercepts.
#   Save this regression model as "trade.ar.lmts".

trade.ar.lmts <- lm(tradepre ~ 0 + tradepreL + Time + Seas)
summary(trade.ar.lmts)


#29  By using new.Time variable, and the following forecasting equation 
#    x_(t+1)<-0+alpha1*x_t+alpha2*Time_(t+1)+beta 
#    create the following new variables:
#   "alpha1" - assumes value of the tradepreL coefficient from the trade.ar.lmts model
#   "alpha2" - assumes value of the Time coefficient from the trade.ar.lmts model
#   "beta1" - takes on values of the first seasonal coefficient from the trade.ar.lmts.
#   "beta2" - takes on values of the second seasonal coefficient from the trade.ar.lmts.
#   "beta3" - takes on values of the third seasonal coefficient from the trade.ar.lmts.
#   "beta4" - takes on values of the fourth seasonal coefficient from the trade.ar.lmts.
#   "forc20191" - takes on the forecasted value of the trade ratio for January 2019
#   "forc20192" - takes on the forecasted value of the trade ratio for February 2019
#   "forc20193" - takes on the forecasted value of the trade ratio for March 2019
#   "forc20194" - takes on the forecasted value of the trade ratio for April 2019
#   "trade.ar.lmts.forc" a vector of four predicted trade ratios.

alpha1 <- coef(trade.ar.lmts)["tradepreL"]
alpha2 <- coef(trade.ar.lmts)["Time"]
beta1 <- coef(trade.ar.lmts)["Seas1"]
beta2 <- coef(trade.ar.lmts)["Seas2"]
beta3 <- coef(trade.ar.lmts)["Seas3"]
beta4 <- coef(trade.ar.lmts)["Seas4"]

forc20191 <- alpha1*tradepre[length(tradepre)] + alpha2*new.Time[1] + beta1
forc20192 <- alpha1*forc20191 + alpha2*new.Time[2] + beta2
forc20193 <- alpha1*forc20192 + alpha2*new.Time[3] + beta3
forc20194 <- alpha1*forc20193 + alpha2*new.Time[4] + beta4

trade.ar.lmts.forc <- c(forc20191, forc20192, forc20193, forc20194)

#30 Please calculate forecast mean absolute percentage error 
#   for the trade.ar.lmts.forc forecasting model.
#   Which of the following models would you chose to based on this criteria?
##I would choose the model with the lowest MAPE,which is trade.ar.lmts.forc

#   Models: trade.ar.forc, trade.gls.forc, and trade.ar.lmts.forc)
#mape_gls
#[1] 7.342898
# mape_ar
#[1] 6.467586
# mape_ar_lmts
#[1] 5.741217

mape_ar_lmts <- mean(abs((actual - trade.ar.lmts.forc)/actual))*100

mape_ar_lmts
##[1] 5.741217
c(MAPE_AR=mape_ar, MAPE_GLS=mape_gls, MAPE_AR_LMTS=mape_ar_lmts)
