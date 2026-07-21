# ==============================================================================
# GitHub Portfolio Project
# Author: Y. Ding
# Language: R
#
# This script is a portfolio-ready adaptation of an academic time-series project.
# It removes course-administration text, avoids machine-specific working
# directories, and organizes the analysis into reproducible sections.
# ==============================================================================


# PROJECT OVERVIEW -------------------------------------------------------------
# Analyze monthly gold, oil, and Japanese yen data using decomposition,
# autocorrelation, exponential smoothing, and out-of-sample forecasting.
#
# Skills demonstrated:
# - Time-series construction and visualization
# - Trend and seasonal decomposition
# - ACF and cross-correlation analysis
# - Holt-Winters exponential smoothing
# - Forecast evaluation using MAPE and MPE

# PACKAGES ---------------------------------------------------------------------
required_packages <- c("readxl", "forecast")
missing_packages <- required_packages[
  !required_packages %in% rownames(installed.packages())
]
if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}
library(readxl)
library(forecast)

# DATA LOADING -----------------------------------------------------------------
# Place goy.xls in a data/ folder beside this script.
data_path <- file.path("data", "goy.xls")
if (!file.exists(data_path)) {
  stop("Missing data/goy.xls. Add the dataset before running this script.")
}

goy <- read_excel(data_path)
goy[[1]] <- as.Date(goy[[1]])
goy[, -1] <- lapply(goy[, -1], as.numeric)
goy <- goy[complete.cases(goy), ]

# TIME-SERIES PREPARATION -------------------------------------------------------
goy_ts <- ts(goy[, -1], start = c(1946, 1), frequency = 12)
goy_ts[, "yen"] <- 1 / goy_ts[, "yen"]

gold <- window(goy_ts[, "gold"], start = c(2005, 1))
oil  <- window(goy_ts[, "oil"],  start = c(2005, 1))
yen  <- window(goy_ts[, "yen"],  start = c(2005, 1))

# EXPLORATORY ANALYSIS ----------------------------------------------------------
plot(goy_ts, main = "Gold, Oil, and Yen Monthly Time Series")
plot(aggregate(goy_ts), main = "Annual Average Asset Prices")

summer_months <- cycle(goy_ts) %in% 6:8
winter_months <- cycle(goy_ts) %in% c(12, 1, 2)

summer_oil <- mean(goy_ts[summer_months, "oil"], na.rm = TRUE)
winter_oil <- mean(goy_ts[winter_months, "oil"], na.rm = TRUE)
seasonal_oil_difference <- (summer_oil - winter_oil) / winter_oil * 100
print(seasonal_oil_difference)

# DECOMPOSITION AND DEPENDENCE --------------------------------------------------
gold_decomp <- decompose(gold, type = "additive")
oil_decomp  <- decompose(oil,  type = "additive")
yen_decomp  <- decompose(yen,  type = "additive")

plot(gold_decomp)
plot(oil_decomp)
plot(yen_decomp)

gold_random <- na.omit(gold_decomp$random)
oil_random  <- na.omit(oil_decomp$random)
yen_random  <- na.omit(yen_decomp$random)

acf(gold_random, main = "ACF: Gold Random Component")
acf(oil_random,  main = "ACF: Oil Random Component")
acf(yen_random,  main = "ACF: Yen Random Component")

ccf(gold_random, oil_random, main = "CCF: Gold vs Oil")
ccf(gold_random, yen_random, main = "CCF: Gold vs Yen")
ccf(oil_random, yen_random, main = "CCF: Oil vs Yen")

# HOLT-WINTERS MODELS -----------------------------------------------------------
gold_model <- HoltWinters(gold, beta = FALSE, gamma = FALSE)
oil_model  <- HoltWinters(oil,  beta = FALSE, gamma = FALSE)
yen_model  <- HoltWinters(yen,  beta = FALSE, gamma = FALSE)

plot(gold_model)
plot(oil_model)
plot(yen_model)

# TRAIN-TEST EVALUATION ---------------------------------------------------------
gold_train <- window(gold, end = c(2018, 8))
oil_train  <- window(oil,  end = c(2018, 8))
yen_train  <- window(yen,  end = c(2018, 8))

gold_test <- window(gold, start = c(2018, 9), end = c(2019, 2))
oil_test  <- window(oil,  start = c(2018, 9), end = c(2019, 2))
yen_test  <- window(yen,  start = c(2018, 9), end = c(2019, 2))

gold_fit <- HoltWinters(gold_train, beta = FALSE, gamma = FALSE)
oil_fit  <- HoltWinters(oil_train,  beta = FALSE, gamma = FALSE)
yen_fit  <- HoltWinters(yen_train,  beta = FALSE, gamma = FALSE)

gold_forecast <- forecast(gold_fit, h = length(gold_test))
oil_forecast  <- forecast(oil_fit,  h = length(oil_test))
yen_forecast  <- forecast(yen_fit,  h = length(yen_test))

mape <- function(actual, predicted) {
  mean(abs((actual - predicted) / actual), na.rm = TRUE) * 100
}

evaluation <- data.frame(
  asset = c("Gold", "Oil", "Yen"),
  MAPE = c(
    mape(gold_test, gold_forecast$mean),
    mape(oil_test, oil_forecast$mean),
    mape(yen_test, yen_forecast$mean)
  )
)
print(evaluation)

# FINAL FORECASTS ---------------------------------------------------------------
gold_final <- forecast(gold_model, h = 10)
oil_final  <- forecast(oil_model,  h = 10)
yen_final  <- forecast(yen_model,  h = 10)

plot(gold_final, main = "Gold Price Forecast")
plot(oil_final,  main = "Oil Price Forecast")
plot(yen_final,  main = "Yen Exchange-Rate Forecast")

# KEY FINDINGS -----------------------------------------------------------------
# - The three assets exhibit different persistence and smoothing behavior.
# - Oil prices show a measurable summer-versus-winter difference.
# - Out-of-sample MAPE provides a comparable measure of forecast performance.
# - Holt-Winters offers an interpretable baseline for short-horizon forecasting.
