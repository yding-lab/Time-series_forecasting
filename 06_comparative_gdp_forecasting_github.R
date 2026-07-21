# ==============================================================================
# GitHub Portfolio Project
# Language: R
#
# This script presents a reproducible time-series analysis using relative paths,
# clear sectioning, model diagnostics, and portfolio-focused documentation.
# ==============================================================================


# PROJECT OVERVIEW -------------------------------------------------------------
# This project compares quarterly GDP growth in North America and Europe using:
# - quarterly growth-rate transformation,
# - STL decomposition and Holt-Winters smoothing,
# - ARIMA forecasting,
# - out-of-sample RMSE and MAPE,
# - residual diagnostics, and
# - GARCH volatility modeling.
#
# The script is aligned with the accompanying final report and preserves its
# analytical framework and reported model interpretation.

# REQUIRED PACKAGES ------------------------------------------------------------
required_packages <- c(
  "forecast",
  "tseries",
  "FinTS",
  "rugarch"
)

missing_packages <- required_packages[
  !required_packages %in% rownames(installed.packages())
]

if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

library(forecast)
library(tseries)
library(FinTS)
library(rugarch)

# DATA AVAILABILITY -------------------------------------------------------------
# Expected file: data/comparative_gdp_oecd.csv
#
# Required columns:
# date, united_states, canada, europe
#
# "europe" should contain the same European series used in the final report.
# The report notes that German data were used as a European proxy in the final
# implementation, despite the broader EU framing.

data_path <- file.path(
  "data",
  "comparative_gdp_oecd.csv"
)

if (!file.exists(data_path)) {
  stop(
    paste(
      "Missing data/comparative_gdp_oecd.csv.",
      "Required columns: date, united_states, canada, europe."
    )
  )
}

# DATA LOADING AND VALIDATION ---------------------------------------------------
gdp_data <- read.csv(
  data_path,
  stringsAsFactors = FALSE
)

required_columns <- c(
  "date",
  "united_states",
  "canada",
  "europe"
)

missing_columns <- setdiff(
  required_columns,
  names(gdp_data)
)

if (length(missing_columns) > 0) {
  stop(
    paste(
      "Missing columns:",
      paste(missing_columns, collapse = ", ")
    )
  )
}

gdp_data$united_states <- as.numeric(
  gdp_data$united_states
)

gdp_data$canada <- as.numeric(
  gdp_data$canada
)

gdp_data$europe <- as.numeric(
  gdp_data$europe
)

# GDP GROWTH TRANSFORMATION -----------------------------------------------------
# Quarter-over-quarter logarithmic growth:
# 100 * [log(GDP_t) - log(GDP_t-1)]

us_growth <- diff(log(gdp_data$united_states)) * 100
canada_growth <- diff(log(gdp_data$canada)) * 100
europe_growth <- diff(log(gdp_data$europe)) * 100

north_america_growth <- rowMeans(
  cbind(
    us_growth,
    canada_growth
  ),
  na.rm = TRUE
)

north_america_ts <- ts(
  north_america_growth,
  start = c(1995, 2),
  frequency = 4
)

europe_ts <- ts(
  europe_growth,
  start = c(1995, 2),
  frequency = 4
)

# EXPLORATORY ANALYSIS ----------------------------------------------------------
ts.plot(
  north_america_ts,
  europe_ts,
  lty = c(1, 2),
  main = "Quarterly GDP Growth: North America vs Europe",
  ylab = "Quarter-over-Quarter Growth (%)"
)

legend(
  "topleft",
  legend = c("North America", "Europe"),
  lty = c(1, 2),
  bty = "n"
)

summary_statistics <- data.frame(
  region = c("North America", "Europe"),
  mean = c(
    mean(north_america_ts, na.rm = TRUE),
    mean(europe_ts, na.rm = TRUE)
  ),
  standard_deviation = c(
    sd(north_america_ts, na.rm = TRUE),
    sd(europe_ts, na.rm = TRUE)
  ),
  minimum = c(
    min(north_america_ts, na.rm = TRUE),
    min(europe_ts, na.rm = TRUE)
  ),
  maximum = c(
    max(north_america_ts, na.rm = TRUE),
    max(europe_ts, na.rm = TRUE)
  )
)

print(summary_statistics)

# Reported descriptive results:
# North America mean: approximately 4.48
# North America standard deviation: approximately 0.67
# Europe mean: approximately 0.76
# Europe standard deviation: approximately 0.47

# TREND AND SEASONALITY ---------------------------------------------------------
north_america_stl <- stl(
  north_america_ts,
  s.window = "periodic"
)

europe_stl <- stl(
  europe_ts,
  s.window = "periodic"
)

plot(north_america_stl)
plot(europe_stl)

north_america_hw <- HoltWinters(
  north_america_ts
)

europe_hw <- HoltWinters(
  europe_ts
)

plot(north_america_hw)
plot(europe_hw)

# STATIONARITY -----------------------------------------------------------------
print(adf.test(north_america_ts))
print(adf.test(europe_ts))

# TRAIN-TEST SPLIT --------------------------------------------------------------
forecast_horizon <- 8

north_america_train <- head(
  north_america_ts,
  length(north_america_ts) - forecast_horizon
)

north_america_test <- tail(
  north_america_ts,
  forecast_horizon
)

europe_train <- head(
  europe_ts,
  length(europe_ts) - forecast_horizon
)

europe_test <- tail(
  europe_ts,
  forecast_horizon
)

# ARIMA MODELING ---------------------------------------------------------------
north_america_arima <- auto.arima(
  north_america_train,
  seasonal = TRUE
)

europe_arima <- auto.arima(
  europe_train,
  seasonal = TRUE
)

print(summary(north_america_arima))
print(summary(europe_arima))

checkresiduals(north_america_arima)
checkresiduals(europe_arima)

north_america_forecast <- forecast(
  north_america_arima,
  h = forecast_horizon
)

europe_forecast <- forecast(
  europe_arima,
  h = forecast_horizon
)

plot(
  north_america_forecast,
  main = "North America GDP Growth Forecast"
)

plot(
  europe_forecast,
  main = "Europe GDP Growth Forecast"
)

# FORECAST ACCURACY -------------------------------------------------------------
north_america_accuracy <- accuracy(
  north_america_forecast,
  north_america_test
)

europe_accuracy <- accuracy(
  europe_forecast,
  europe_test
)

accuracy_comparison <- rbind(
  North_America = north_america_accuracy[
    2,
    c("RMSE", "MAPE")
  ],
  Europe = europe_accuracy[
    2,
    c("RMSE", "MAPE")
  ]
)

print(accuracy_comparison)

# RESIDUAL AND ARCH DIAGNOSTICS -------------------------------------------------
north_america_residuals <- residuals(
  north_america_arima
)

europe_residuals <- residuals(
  europe_arima
)

print(
  Box.test(
    north_america_residuals,
    lag = 8,
    type = "Ljung-Box"
  )
)

print(
  Box.test(
    europe_residuals,
    lag = 8,
    type = "Ljung-Box"
  )
)

print(
  ArchTest(
    north_america_residuals,
    lags = 8
  )
)

print(
  ArchTest(
    europe_residuals,
    lags = 8
  )
)

# Reported ARCH-LM p-values:
# North America: approximately 0.892
# Europe: approximately 0.135

# GARCH VOLATILITY MODELS -------------------------------------------------------
garch_specification <- ugarchspec(
  variance.model = list(
    model = "sGARCH",
    garchOrder = c(1, 1)
  ),
  mean.model = list(
    armaOrder = c(1, 1),
    include.mean = TRUE
  ),
  distribution.model = "norm"
)

north_america_garch <- ugarchfit(
  spec = garch_specification,
  data = north_america_ts,
  solver = "hybrid"
)

europe_garch <- ugarchfit(
  spec = garch_specification,
  data = europe_ts,
  solver = "hybrid"
)

print(north_america_garch)
print(europe_garch)

plot(
  sigma(north_america_garch),
  type = "l",
  main = "North America Conditional Volatility",
  ylab = "Conditional Standard Deviation"
)

plot(
  sigma(europe_garch),
  type = "l",
  main = "Europe Conditional Volatility",
  ylab = "Conditional Standard Deviation"
)

# FINAL TWO-YEAR FORECAST -------------------------------------------------------
north_america_final_model <- auto.arima(
  north_america_ts,
  seasonal = TRUE
)

europe_final_model <- auto.arima(
  europe_ts,
  seasonal = TRUE
)

north_america_final_forecast <- forecast(
  north_america_final_model,
  h = 8
)

europe_final_forecast <- forecast(
  europe_final_model,
  h = 8
)

print(north_america_final_forecast)
print(europe_final_forecast)

# Reported end-of-horizon projections:
# North America: approximately 4.63%
# Europe: approximately 0.73%

# STATISTICAL COMPARISON --------------------------------------------------------
mean_difference_test <- t.test(
  as.numeric(north_america_ts),
  as.numeric(europe_ts)
)

variance_comparison <- var.test(
  as.numeric(north_america_ts),
  as.numeric(europe_ts)
)

print(mean_difference_test)
print(variance_comparison)

# KEY FINDINGS -----------------------------------------------------------------
# - North America demonstrated higher average growth and greater volatility.
# - Seasonal effects were limited in both regional series.
# - ARIMA forecasts preserved a persistent regional growth differential.
# - Residual tests suggested limited ARCH evidence, but GARCH was retained to
#   provide a complete volatility comparison.
# - The final report identified a persistent growth gap of about 3.9 percentage
#   points between the two regional series.

# LIMITATIONS ------------------------------------------------------------------
# - The European implementation used German data as a regional proxy.
# - Structural breaks may weaken stationarity assumptions.
# - Limited ARCH evidence may reduce the practical value of GARCH estimates.
# - The sample may not fully represent long-run economic cycles.

# FUTURE IMPROVEMENTS -----------------------------------------------------------
# - Replace the European proxy with an aggregate EU series.
# - Add structural-break tests and crisis-period indicators.
# - Compare frequentist ARIMA models with Bayesian forecasting methods.
# - Extend the analysis to multivariate cross-regional spillover models.
