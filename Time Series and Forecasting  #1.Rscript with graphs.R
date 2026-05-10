Time Series and Forecasting #1 


#1  Check you working directory
getwd()
list.files()

#2  Set your working directory to "ANLY 565/RScript"
setwd("ANLY 565/RScript")

#3  Download goy data set posted on Canvas  and lable it 
#   goy. This dataset reperesnets daily prices of gold,
#   oil, and the price of 1 US dollar in terms of Japanese yen.
#   Set the first column in each data set to the date format 
#   and the remaining columns in numerical format.
library(readxl)

goy <- read_excel("/Users/sophiading/Downloads/goy.xls")
goy[[1]] <- as.Date(goy[[1]])
goy[, -1] <- lapply(goy[, -1], as.numeric)
str(goy)
head(goy)

#4  Create a new data set called "goycc" that contains all complete cases of goy data.
#   Utilize complete.cases function.
goycc <- goy[complete.cases(goy),]

#5 Create a stand alone variable "date" that takes on values of "observation_date"
# variable from the goycc data set. Set the mode of the variable to character
date <- goycc[[1]]
date <- as.character(date)
mode(date)
#6 Find the range of dates covered in goycc data set by applying range function
#  to "date" variable. 
range(as.Date(date))

#7 Create a time series object called "goyccts" by utilizing goycc dataset and 
#  ts() function. In this time series object please exclude the first column 
#  of the goycc dataset. 
goyccts <- ts(goycc[,-1], start = c(1946,1), end = c(2019,3), frequency = 12)
end(goyccts)

#8 Reassign the value of the yen variable from the goyccts data set
#  by converting the exchange rate of yen that represents 
#  the price of 1 US Dollar in terms of Japanese yen to represent 
#  the price of 1 Yen in terms of US Dollar. 
#  This way if the number increases it represents appreciation of Yen. 
#  Hint: Reassign the value of yen variable by taking a reciprocal 

goyccts[, "yen"] <- 1 / goyccts[, "yen"]
head(goyccts)

#9 Plot the time series plot of the three assets. Do you see any trend?
# Do you see any seasonal component? Not really,no obvious seasonal component.

plot(goyccts)
#10 Utilize the aggregate function to plot annual average prices of the three assets.
#   How does this graph differ from the monthly time series plot?
##The aggregate graph is more smooth and focus on long term trend compared to the time series plot.
plot(aggregate(goyccts))


#11 Find the average summer price of oil for the entire sample.
37.26092
summer_months <- cycle(goyccts) %in% 6:8
mean(goyccts[summer_months, "oil"], na.rm = TRUE)


#12 Find the average winter price of oil for the entire sample
34.74591
winter_months <- cycle(goyccts) %in% c(12, 1, 2)
mean(goyccts[winter_months, "oil"], na.rm = TRUE)

#13 Find how the summer price of oil compares to the winter price of oil.
#   Please provide your answer in percentages. 

summer_avg <- mean(goyccts[summer_months, "oil"], na.rm = TRUE)
winter_avg <- mean(goyccts[winter_months, "oil"], na.rm = TRUE)
((summer_avg - winter_avg) / winter_avg) * 100
##Answer: The summer price of oil is 7.24% higher comparing to the winter price of oil.

#14 Use window() function to create three stand alone variables 
#   "gold", "oil", and "yen" that take on values of the "gold", "oil", and "yen" 
#   variables from the goyccts dataset starting from January of 2005

gold <- window(goyccts[, "gold"], start = c(2005, 1))
oil  <- window(goyccts[, "oil"], start = c(2005, 1))
yen  <- window(goyccts[, "yen"], start = c(2005, 1))



#15 Use plot and decompose functions to generate three graphs that would depict
#   the observed values, trends, seasonal, and random components for "gold"
#   "oil" and "yen" variables. Would you choose multiplicative or 
#   additive decomposition model for each of the variables?

## For all 3 variables, I would choose additive decomposition model 
plot(decompose(gold))
plot(decompose(oil))
plot(decompose(yen))


#16 For each of the variables extract the random component and save 
#   them as "goldrand", "oilrand", and "yenrand". Moreover, use na.omit()
#   function to deal with the missing values.
goldrand <- na.omit(decompose(gold)$random)
oilrand  <- na.omit(decompose(oil)$random)
yenrand  <- na.omit(decompose(yen)$random)


#17 For the random component of each of the assets, please estimate 
#   autocorrelation function.Does any of the assets exhibit autocorrelation?
#   If yes, to what degree? 
#   Keep in mind there are missing values. 
## Yes,goldrand shows short-term positive autocorrelation at lag 1 and 2.
## oilrand shows short-term autocorrelation and  weaker autocorrelation later.
## yenrand shows strong autocorrelation.All assets exhibit autocorrelation initially.
acf(goldrand)
acf(oilrand)
acf(yenrand)



#18 For all possible pairs of assets please estimate cross-correlation function 
#   Do any of the variable lead or precede each other?
##Yes, these two variables in the graph precede each other 

#Could you use any of the varibales to predict values of other variables?
##Yes, because the correlations are statistically significant.

#   Make sure to use detrended and seasonally adjusted variables. 
#   ("goldrand", "oilrand", and "yenrand")


ccf(goldrand, oilrand)
ccf(goldrand, yenrand)
ccf(oilrand, yenrand)


#19 Based on the time series plot of gold, oil, and yen prices, 
#   there appears to be no systematic trends or seasonal effects. 
#   Therefore, it is reasonable to use exponential smoothing for these time series.
#   Estimate alpha, the smoothing parameter for gold, oil and yen. 
#   What does the value of alpha tell you tell you about the behavior of the mean?
##Answer:The higher the value of alpha, the mean value represents more data.With a close to 1, the mean value represents the most recent data.


#   What is the estimated value of the mean for each asset?

##Answer: Gold: 387.44, Oil: 19.04, Yen: 101.85

gold.hw <- HoltWinters(gold, beta = FALSE, gamma = FALSE)
oil.hw  <- HoltWinters(oil, beta = FALSE, gamma = FALSE)
yen.hw  <- HoltWinters(yen, beta = FALSE, gamma = FALSE)

gold.hw$alpha
oil.hw$alpha
yen.hw$alpha

gold.hw$fitted
oil.hw$fitted
yen.hw$fitted

#20 Use plot() function to generate three graphs that depict observed 
#   and HoltWinter fitted values for each asset.

plot(gold.hw)
plot(oil.hw)
plot(yen.hw)

#21 Use window() function to create 3 new variables called 
#   "goldpre", "oilpre", and "yenpre" that covers the period from January 2005, 
#   until August 2018. 
goldpre <- window(gold, end = c(2018, 8))
oilpre  <- window(oil, end = c(2018, 8))
yenpre  <- window(yen, end = c(2018, 8))

#22 Use window() function to create 3 new variables called 
#   goldpost, oilpost, and yenpost that cover the period from September 2018, 
#   until February 2019.

##  Time-Series [1:164] from 2005 to 2019: 413 410 384 374 330 ...
goldpost <- window(gold, start = c(2018, 9), end = c(2019, 2))
oilpost  <- window(oil, start = c(2018, 9), end = c(2019, 2))
yenpost  <- window(yen, start = c(2018, 9), end = c(2019, 2))
str(goldpre)


#23 Estimate HoltWinters filter model for each asset, while using only only pre data.
#   Save each of these estimates as "gold.hw", "oil.hw", and "yen.hw".

gold.hw <- HoltWinters(goldpre,seasonal = "additive")
oil.hw  <- HoltWinters(oilpre,seasonal = "additive")
yen.hw  <- HoltWinters(yenpre,seasonal = "additive")

plot(gold.hw)
plot(oil.hw)
plot(yen.hw)

#24 Use HoltWinters filter estimates generated in#23 and predict() function 
#   to create a 6 month ahead forecast of the gold, oil, and yen prices. 
#   Save these forcasted values as "goldforc", "oilforc", and "yenforc".
goldforc <- predict(gold.hw, n.ahead = 6)
oilforc  <- predict(oil.hw, n.ahead = 6)
yenforc  <- predict(yen.hw, n.ahead = 6)

#25 Use ts.plot() function to plot side-by-side post sample prices 
#   ("goldpost", "oilpost","yenpost") and their forecasted counterparts.
#   Please designate red color to represent the actual prices, 
#   and blue doted lines to represent forecasted values. 
ts.plot(goldpost, goldforc, col = c("red", "blue"), lty = c(1, 2))
ts.plot(oilpost, oilforc, col = c("red", "blue"), lty = c(1, 2))
ts.plot(yenpost, yenforc, col = c("red", "blue"), lty = c(1, 2))


#26 Please calculate forecast mean percentage error for each assets forecasting model. 
#   Which asset's forecasting model has the lowest mean percentage error?
## Oil has the lowest mean percentage error.

mpe <- function(actual, forecast) {
  mean((actual - forecast) / actual) * 100}
mpe(goldpost, goldforc[1:length(goldpost)])
4.394699
mpe(oilpost, oilforc[1:length(oilpost)])
1.640566
mpe(yenpost, yenforc[1:length(yenpost)])
11.11245

#27 Use gold, oil, and yen variables to estimate Holt-Winters model
#   for each asset. Save these estimates as "goldc.hw", "oilc.hw", and "yenc.hw".
goldc.hw <- HoltWinters(gold, beta = FALSE, gamma = FALSE)
oilc.hw  <- HoltWinters(oil, beta = FALSE, gamma = FALSE)
yenc.hw  <- HoltWinters(yen, beta = FALSE, gamma = FALSE)
#28 Use "goldc.hw", "oilc.hw", and "yenc.hw" models to create an out-of-sample
#   forecasts to predict the prices of each of the assets for the rest of the 2019.
#   Save these forecasts as "goldforcos", "oilforcos", "yenforcos".
#   What is the forecasted price of Gold for November 2019? 
398.6947
goldforcos <- predict(goldc.hw, n.ahead = 10)
oilforcos <- predict(oilc.hw, n.ahead = 10)
yenforcos <- predict(yenc.hw, n.ahead = 10)

goldforcos[9]
# 29 Create time series plots for each asset, that combines the actual price data
#    of each asset and their out-of-sample forecasted values.
#    Please designate red color to represent the actual prices, 
#    and blue doted lines to represent forecasted values.
#    What do you think will happen to the price of each asset by the end of the year?
##gold price will increase,oil price will go up and yen will continue to decline

ts.plot(gold, goldforcos, col = c("red", "blue"), lty = c(1, 3))
ts.plot(oil, oilforcos, col = c("red", "blue"), lty = c(1, 3))
ts.plot(yen, yenforcos, col = c("red", "blue"), lty = c(1, 3))

# 30 Please calculate percentage change between the price of each asset in 
#    February 2019 and their forecasted December 2019 prices. 
#    Which asset promises the highest rate of return? 

##yen promises the highest rate of return.

feb2019_gold <- window(gold, start = c(2019, 2), end = c(2019, 12))[1]
feb2019_oil  <- window(oil, start = c(2019, 2), end = c(2019, 12))[1]
feb2019_yen  <- window(yen, start = c(2019, 2), end = c(2019, 12))[1]

((goldforcos[length(goldforcos)] - feb2019_gold) / feb2019_gold) * 100
2.903626
((oilforcos[length(oilforcos)] - feb2019_oil) / feb2019_oil) * 100
-0.8402806
((yenforcos[length(yenforcos)] - feb2019_yen) / feb2019_yen) * 100
3.830801

plot_files <- c(
  "~/Downloads/goyccts_plot.png",
  "~/Downloads/goyccts_aggregate_plot.png",
  "~/Downloads/gold_decompose.png",
  "~/Downloads/oil_decompose.png",
  "~/Downloads/yen_decompose.png",
  "~/Downloads/gold_hw_plot.png",
  "~/Downloads/oil_hw_plot.png",
  "~/Downloads/yen_hw_plot.png",
  "~/Downloads/gold_forecast_plot.png",
  "~/Downloads/oil_forecast_plot.png",
  "~/Downloads/yen_forecast_plot.png",
  "~/Downloads/gold_out_sample_forecast.png",
  "~/Downloads/oil_out_sample_forecast.png",
  "~/Downloads/yen_out_sample_forecast.png"
)

plot_calls <- list(
  function() plot(goyccts),
  function() plot(aggregate(goyccts)),
  function() plot(decompose(gold)),
  function() plot(decompose(oil)),
  function() plot(decompose(yen)),
  function() plot(gold.hw),
  function() plot(oil.hw),
  function() plot(yen.hw),
  function() ts.plot(goldpost, goldforc, col = c("red", "blue"), lty = c(1, 2)),
  function() ts.plot(oilpost, oilforc, col = c("red", "blue"), lty = c(1, 2)),
  function() ts.plot(yenpost, yenforc, col = c("red", "blue"), lty = c(1, 2)),
  function() ts.plot(gold, goldforcos, col = c("red", "blue"), lty = c(1, 3)),
  function() ts.plot(oil, oilforcos, col = c("red", "blue"), lty = c(1, 3)),
  function() ts.plot(yen, yenforcos, col = c("red", "blue"), lty = c(1, 3)))

save_multiple_plots(plot_files, plot_calls)

