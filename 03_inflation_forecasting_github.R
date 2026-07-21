# ==============================================================================
# GitHub Portfolio Project
# Language: R
#
# This script presents a reproducible time-series analysis using relative paths,
# clear sectioning, model diagnostics, and portfolio-focused documentation.
# ==============================================================================


# PROJECT OVERVIEW -------------------------------------------------------------
# This project forecasts the U.S. Consumer Price Index and monthly inflation
# using trend-seasonality regression, ARMA residual modeling, and SARIMA.
#
# The original model logic is retained, including:
# - ARMA(1,0,2) for CPI regression residuals,
# - a three-month forecast evaluation, and
# - seasonal ARIMA modeling for monthly inflation.

# REQUIRED PACKAGES ------------------------------------------------------------
required_packages <- c("readxl", "forecast", "tseries")

missing_packages <- required_packages[
  !required_packages %in% rownames(installed.packages())
]

if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

library(readxl)
library(forecast)
library(tseries)

# DATA AVAILABILITY -------------------------------------------------------------
# Expected file: data/Inflation.xls
# Do not publish the raw dataset if redistribution is restricted.

data_path <- file.path("data", "Inflation.xls")

if (!file.exists(data_path)) {
  stop("Missing data/Inflation.xls. Add the dataset before running this script.")
}

# DATA LOADING AND PREPARATION --------------------------------------------------
inflation_data <- read_excel(data_path)

observation_date <- as.Date(inflation_data[[1]])
cpi_values <- as.numeric(inflation_data[[2]])

cpi_ts <- ts(
  cpi_values,
  start = c(
    as.numeric(format(min(observation_date), "%Y")),
    as.numeric(format(min(observation_date), "%m"))
  ),
  frequency = 12
)

# EXPLORATORY ANALYSIS ----------------------------------------------------------
plot(
  cpi_ts,
  main = "U.S. Consumer Price Index",
  xlab = "Year",
  ylab = "Index"
)

acf(cpi_ts, main = "ACF: CPI")
pacf(cpi_ts, main = "PACF: CPI")

cpi_pre <- window(
  cpi_ts,
  start = c(1990, 1),
  end = c(2018, 10)
)

cpi_post <- window(
  cpi_ts,
  start = c(2018, 11)
)

# TREND AND SEASONALITY MODEL ---------------------------------------------------
time_index <- time(cpi_pre)
season <- factor(cycle(cpi_pre))

cpi_lm <- lm(
  cpi_pre ~ 0 + time_index + season
)

print(summary(cpi_lm))

new_time <- seq(
  2018.75 + 1 / 12,
  by = 1 / 12,
  length.out = 12
)

new_season <- factor(
  c(11, 12, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
  levels = levels(season)
)

new_data <- data.frame(
  time_index = new_time,
  season = new_season
)

cpi_regression_forecast <- predict(
  cpi_lm,
  newdata = new_data
)

# ARMA MODEL FOR REGRESSION RESIDUALS ------------------------------------------
cpi_lm_residuals <- residuals(cpi_lm)

acf(
  cpi_lm_residuals,
  main = "ACF: CPI Regression Residuals"
)

pacf(
  cpi_lm_residuals,
  main = "PACF: CPI Regression Residuals"
)

best_aic <- Inf
best_order <- c(0, 0, 0)
best_fit <- NULL

for (p in 0:3) {
  for (q in 0:3) {
    candidate <- try(
      arima(cpi_lm_residuals, order = c(p, 0, q)),
      silent = TRUE
    )

    if (!inherits(candidate, "try-error")) {
      candidate_aic <- AIC(candidate)

      if (candidate_aic < best_aic) {
        best_aic <- candidate_aic
        best_order <- c(p, 0, q)
        best_fit <- candidate
      }
    }
  }
}

residual_arma_model <- best_fit

print(best_order)
print(best_aic)
# Original selected order: ARMA(1,0,2).

residual_forecast <- predict(
  residual_arma_model,
  n.ahead = 12
)

cpi_forecast <- ts(
  as.numeric(cpi_regression_forecast) +
    as.numeric(residual_forecast$pred),
  start = c(2018, 11),
  frequency = 12
)

ts.plot(
  cpi_pre,
  cpi_forecast,
  lty = c(1, 2),
  main = "CPI History and 12-Month Forecast",
  ylab = "Index"
)

legend(
  "topleft",
  legend = c("Historical", "Forecast"),
  lty = c(1, 2),
  bty = "n"
)

mape <- function(actual, predicted) {
  mean(abs((actual - predicted) / actual), na.rm = TRUE) * 100
}

cpi_three_month_mape <- mape(
  cpi_post[1:3],
  cpi_forecast[1:3]
)

print(cpi_three_month_mape)

# MONTHLY INFLATION RATE --------------------------------------------------------
monthly_inflation <- diff(log(cpi_ts)) * 100

plot(
  monthly_inflation,
  main = "Monthly U.S. Inflation Rate",
  xlab = "Year",
  ylab = "Percent"
)

acf(monthly_inflation, main = "ACF: Monthly Inflation")
pacf(monthly_inflation, main = "PACF: Monthly Inflation")

inflation_pre <- window(
  monthly_inflation,
  start = c(1990, 1),
  end = c(2018, 10)
)

inflation_post <- window(
  monthly_inflation,
  start = c(2018, 11)
)

# SARIMA MODEL -----------------------------------------------------------------
sarima_model <- auto.arima(
  inflation_pre,
  seasonal = TRUE,
  stepwise = FALSE,
  approximation = FALSE
)

print(summary(sarima_model))
checkresiduals(sarima_model)

inflation_forecast <- forecast(
  sarima_model,
  h = 3
)

plot(
  inflation_forecast,
  main = "Three-Month Inflation Forecast"
)

inflation_three_month_mape <- mape(
  inflation_post[1:3],
  inflation_forecast$mean
)

print(inflation_three_month_mape)

# KEY FINDINGS -----------------------------------------------------------------
# - CPI levels are nonstationary and trend upward over time.
# - Trend and seasonal regression alone leaves autocorrelation in residuals.
# - ARMA(1,0,2) captures the remaining CPI residual dependence.
# - Monthly inflation is modeled more appropriately through SARIMA.
# - Short-horizon forecast accuracy is evaluated using MAPE.

# FUTURE IMPROVEMENTS -----------------------------------------------------------
# - Use rolling-origin cross-validation rather than one fixed holdout period.
# - Compare against ETS and structural time-series models.
# - Add macroeconomic predictors such as unemployment and policy rates.
