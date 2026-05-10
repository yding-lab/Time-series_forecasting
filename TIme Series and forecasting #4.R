#Time Series and Forecasting #4


#1  Check your working directory
getwd()

#2  Set your working directory to "ANLY 565/RScript". 
wd.path <- "~/Desktop/ANLY 565/RScript"


#3  Download "ffrategdp.xls" data file and set the "observation_date" 
#   variable to the date format and the "FEDFUNDS" and "GDPC1" variables to the numeric format.
#   The "FEDFUNDS" variable represents the effective federal funds rate,
#   which indicates the interest rate at which depository institutions trade federal funds 
#   (balances held at Federal Reserve Banks) with each other overnight. 
#   The "GDPC1" variable represents real gross domestic product.
data <- readxl::read_excel("~/Downloads/ffrategdp.xls")

data$observation_date <- as.Date(data$observation_date)
data$FEDFUNDS <- as.numeric(data$FEDFUNDS)
data$GDPC1 <- as.numeric(data$GDPC1)


#4  By using ts() function create a time series object that contains two variables: "FEDFUNDS" and "GDPC1".
#   Label it as "ffrategdpts".

start_ts <- c(as.numeric(format(min(data$observation_date), "%Y")), 
              as.numeric(format(min(data$observation_date), "%m")) %/% 4 + 1)
ffrategdpts <- ts(cbind(FEDFUNDS = data$FEDFUNDS, GDPC1 = data$GDPC1), start = start_ts, frequency = 4)


#5  Create two stand alone variables "fedrate" and "gdp" that take on values of the "FEDFUNDS" and "GDPC1"
#   variables from the "ffrategdpts" data set

fedrate <- ffrategdpts[ , "FEDFUNDS"]
gdp <- ffrategdpts[ , "GDPC1"]


#6 When federal funds rate goes down, the commercial loan interest rates go down too.
#  This means that people can borrow cheaply and invest in their businesses, 
#  which will result in higher gross domestic output. 
#  Therefore, you suspect that the federal funds rate has a negative correlation with GDP.
#  To test this hypothesis you decide to use lm() function to estimate the coefficients of 
#  a linear regression model in which "gdp" is a dependent variable and "fedrate" 
#  is as an independent variable.  
#  Save the estimated model as gdpfr.lm
#  Based on the results of this model can you make any conclusions about the nature of the 
#  relationship between the gdp and the federal funds rate?

##There is a negative relationship between the gdp and the federal funds rate
##with a negative coefficient of -538.30

gdpfr.lm <- lm(gdp ~ fedrate)
summary(gdpfr.lm)

##Min      1Q  Median      3Q     Max 
##-8680.0 -3843.8   526.3  3522.0  8398.7 

##Coefficients:
## Estimate Std. Error t value             Pr(>|t|)    
##(Intercept) 11915.26     467.51  25.487 < 0.0000000000000002 ***
##  fedrate      -538.30      78.18  -6.885      0.0000000000438 ***
---
  ## Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
  
  ##Residual standard error: 4504 on 258 degrees of freedom
  ##Multiple R-squared:  0.1552,	Adjusted R-squared:  0.1519 
  ##F-statistic:  47.4 on 1 and 258 DF,  p-value: 0.00000000004382
  
  #7 You have suspected that the "gdp" variable may contain a unit root.
  #  By using Augmented Dickey Fuller method test "gdp" variable for the 
  #  the presence of unit root. 
  #  Does "gdp" variable contain a unit root? Yes
  #  Is "gdp" variable stationary? No, it is not stationary.
  
  library(urca)
adf_test<-ur.df(data$GDPC1,type = "trend",lags=1)
summary(adf_test)

#8  By using Augmented Dickey Fuller method test "fedrate" variable for the 
#  the presence of unit root. 
#  Does "fedrate" variable contain a unit root? Yes
#  Is "fedrate" variable stationary? No,it is not stationary

library(tseries)
adf_test_fed<-adf.test(data$FEDFUNDS)
print(adf_test_fed)

#9  The Phillips-Ouliaris test shows whether there is evidence that the series are
#   cointegrated, which justifies the use of a regression model. 
#   Are "gdp" and "fedrate" variables cointegrated?

##Value of test-statistic is 0.1045,fails to reject Ho,so it is not cointegrated

#   Is "gdpfr.lm" a suitable model to explore the relationship between "gdp" and "fedrate"?

## No,it is not a suitable model.
po_test <- ca.po(cbind(gdp, fedrate), demean = "constant")
summary(po_test)


#10 Create the following 2 new variables:
#   "gdpgrowth" - that represents quarterly percentage change in GDP
#   "fedratediff" - that represents quarterly difference in the federal funds rate (simple difference)
#   To each of the variables add "NA" as the first observation .
#   This will ensure that the new variables are of the same length as the existing variables.

gdpgrowth <- c(NA, diff(gdp)/head(gdp,-1)*100)
fedratediff <- c(NA, diff(fedrate))


#11  By using ts() and cbind() functions add "gdpgrowth" and "fedratediff" variables 
#    to the "ffrategdpts" data set. 

ffrategdpts <- ts(cbind(ffrategdpts, gdpgrowth = gdpgrowth, fedratediff = fedratediff), start = start_ts, frequency = 4)


#12 Use na.omit() function to get rid of the missing values in the "ffrategdpts" data set. 
#   Save the new data set as "ffrategdptscc". 

ffrategdptscc <- na.omit(ffrategdpts)


#13  Create 2 new variables: 
#    "ggdp" - takes on values of the "gdpgrowth" from the "ffrategdptscc"
#    "dfrate" - takes on values of the "fedratediff" from the "ffrategdptscc"

ggdp <- ffrategdptscc[ , "gdpgrowth"]
dfrate <- ffrategdptscc[ , "fedratediff"]


#14 Use to Augmented Dickey-Fuller test to determine whether "ggdp" and "dfrate"
#   are stationary or not. 
#   Does "ggdp" contain a unit root? Is "ggdp" stationary? 

##p-value = 0.01<0.05,reject Ho,so "ggdp"does not contain a unit root and is stationary

#   Does "dfrate" contain a unit root? Is "dfrate" stationary?

##Does not contain a unit root and is stationary

adf.test(ggdp)
adf.test(dfrate)

#15 Use lm() function to estimate the coefficients of a linear regression model 
#   in which "ggdp" is a dependent variable and "dfrate" is as an independent variable.
#   Lable these estimates as "ggdp.dfrate.lm".
#   Based on the findings of the linear regression model what is the nature of the relationship 
#   between the growth rate of real gdp and difference in federal funds rate?

## There is a positive and statistically significant relationship 
## between the growth rate of real GDP and the difference in the federal funds rate

ggdp.dfrate.lm <- lm(ggdp ~ dfrate)
summary(ggdp.dfrate.lm)


#16 Create a variable called "ggdp.dfrate.lm.resid" that represents the residual series obtained 
#   from the "ggdp.dfrate.lm" regression

ggdp.dfrate.lm.resid <- resid(ggdp.dfrate.lm)


#17 Construct acf and pacf functions for "ggdp.dfrate.lm.resid".
#   What can you say about the goodness of the fit of the model?

## Not a good fit of the model as residuals still show autocorrelation.

ggdp.dfrate.lm.resid<-residuals(ggdp.dfrate.lm)
acf(ggdp.dfrate.lm.resid,main="ACF of Residuals")
pacf(ggdp.dfrate.lm.resid,main="PACF of Residuals")

#18 Maybe vector autoregression model would prove a better fit. 
#   Upload "vars" library that contains VAR() function

install.packages("vars")
library(vars)

#19 Estimate a VAR model for the "ggdp" and "dfrate" variables.
#   In this model include 3 lags of each variable. 
#   Save the estimates of the var model as "ggdp.dfrate.var"

var_data <- cbind(ggdp, dfrate)
ggdp.dfrate.var <- VAR(var_data, p = 3, type = "const")
summary(ggdp.dfrate.var)

#20 Use plot() and irf() functions to obtain and plot impulse response functions for each variable.
#   IRF illustrates the behavior of a variable in response to one standard deviation shock 
#   in its own value and in the value of the other variable.
#   Based on the these graph what conclusions can you draw about the nature of the relationship between 
#   the growth rate of gdp and the difference in federal funds rate? 

##An increase in the growth rate of gdp leads to an increase in federal funds rate

#Any potential explanations?

##The Fed usually raises interest rates in response to strong GPD growth to prevent inflation
##The Fed usually lowers interest rates when the economy needs stimulation

irf_obj <- irf(ggdp.dfrate.var, impulse = c("ggdp","dfrate"), response = c("ggdp","dfrate"), n.ahead = 8, boot = TRUE)
plot(irf_obj)


#21 Use resid() function to obtain the residuals from the ggdp equation of the "ggdp.dfrate.var" model.
#   Save this residual series as "var.ggdp.resid".

var.ggdp.resid <- residuals(ggdp.dfrate.var)[ , "ggdp"]


#22 Use resid() function to obtain the residuals from the dfrate equation of the "ggdp.dfrate.var" model.
#   Save this residual series as "var.dfrate.resid".

var.dfrate.resid <- residuals(ggdp.dfrate.var)[ , "dfrate"]


#24 Plot acf and pacf functions for the 'var.ggdp.resid". 
#   Does "ggdp.dfrate.var" model provide a good fit to explain growth rate of gdp?


##Yes,it provides a good fit to explain growth rate of gdp
##as the residuals show no strong autocorrelation

acf(var.ggdp.resid)
pacf(var.ggdp.resid)

#25 Plot acf and pacf functions for the 'var.dfrate.resid". 
#   Does "ggdp.dfrate.var" model provide a good fit to explain the difference in federal funds rate?

##No,as some residuals show strong autocorrelation

acf(var.dfrate.resid)
pacf(var.dfrate.resid)


#26 Use "ggdp.dfrate.var" model and predict() function to forecast growth rate of gdp and 
#   change in federal funds rate over the upcoming year. 
#   Save the predicted values as "VAR.pred"


VAR.pred <- predict(ggdp.dfrate.var, n.ahead = 4, ci = 0.95)

#27 Use ts() function and VAR.pred forecast to create a new variable "ggdp.pred".
#   It should contain the forcasted values of the growth rate of gdp over the next 4 quarters.

ggdp.pred <- ts(VAR.pred$fcst$ggdp[,"fcst"], start = end(ffrategdptscc)[1] + 0.25, frequency = 4)


#28 Use ts() function and VAR.pred forecast to create a new variable "dfrate.pred".
#   It should contain the prediction of the change in the federal funds rate over the next 4 quarters.

dfrate.pred <- ts(VAR.pred$fcst$dfrate[,"fcst"], start = end(ffrategdptscc)[1] + 0.25, frequency = 4)


#29 Plot the times series graph of the past growth rates of gdp alongside 
#   its future forecasted values. Do you expect the gdp to grow over the next 4 quarters?

##Yes, GDP is expected to grow moderately over the next 4 quarters.

ts.plot(ffrategdptscc[ , "gdpgrowth"], ggdp.pred, col = c("black", "blue"), lty = c(1,2))


#30 Plot the times series graph of the past changes of the federal funds rate alongside 
#   its future forecasted values. 
#   Do you expect the federal funds rate to increase over the next 4 quarters?

##Federal funds rate is not expected to increase over the next 4 quarters

#   Should one take out a loan now?

##someone should take out a loan now as the Fed is very unlikely to raise the interest rates

ts.plot(ffrategdptscc[ , "fedratediff"], dfrate.pred, col = c("black", "yellow"), lty = c(1,2))



