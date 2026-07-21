# ==============================================================================
# GitHub Portfolio Project
# Language: R
#
# This script presents a reproducible time-series analysis using relative paths,
# clear sectioning, model diagnostics, and portfolio-focused documentation.
# ==============================================================================


# PROJECT OVERVIEW -------------------------------------------------------------
# This project forecasts China's monthly export-to-import ratio using:
# - autoregressive modeling,
# - trend and seasonal regression,
# - generalized least squares with autocorrelated errors, and
# - out-of-sample forecast evaluation.
#
# The original model specifications and reported conclusions are preserved.

# REQUIRED PACKAGES ------------------------------------------------------------
required_packages <- c("readxl", "nlme")

missing_packages <- required_packages[
  !required_packages %in% rownames(installed.packages())
]

if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

library(readxl)
library(nlme)

# DATA AVAILABILITY -------------------------------------------------------------
# Expected file: data/trade.xls
#
# The dataset was used in an academic analysis. If redistribution is restricted,
# do not upload the raw file. Instead, document the source in the repository
# README and place a local copy inside data/ before running this script.

data_path <- file.path("data", "trade.xls")

if (!file.exists(data_path)) {
  stop("Missing data/trade.xls. Add the dataset before running this script.")
}

# DATA LOADING AND PREPARATION --------------------------------------------------
trade_data <- read_excel(data_path)

date_values <- as.Date(trade_data[[1]])
trade_values <- as.numeric(trade_data[[2]])

start_year <- as.numeric(format(min(date_values), "%Y"))
start_month <- as.numeric(format(min(date_values), "%m"))

trade_ts <- ts(
  trade_values,
  start = c(start_year, start_month),
  frequency = 12
)

# EXPLORATORY ANALYSIS ----------------------------------------------------------
plot(
  trade_ts,
  main = "China Export-to-Import Ratio",
  xlab = "Year",
  ylab = "Trade Ratio (%)"
)

trade_pre <- window(trade_ts, end = c(2018, 12))
trade_post <- window(trade_ts, start = c(2019, 1))

acf(trade_pre, main = "ACF: China Trade Ratio")
pacf(trade_pre, main = "PACF: China Trade Ratio")

# AUTOREGRESSIVE MODEL ----------------------------------------------------------
# The original analysis estimated an AR model using maximum likelihood.
trade_ar <- ar(
  trade_pre,
  aic = FALSE,
  method = "mle"
)

trade_ar_aic <- ar(
  trade_pre,
  aic = TRUE,
  method = "mle"
)

print(trade_ar)
print(trade_ar_aic$order)

# Approximate 95% confidence intervals for AR coefficients.
ar_standard_errors <- sqrt(diag(trade_ar$asy.var.coef))
ar_coefficients <- trade_ar$ar

ar_confidence_intervals <- cbind(
  coefficient = ar_coefficients,
  lower_95 = ar_coefficients - 2 * ar_standard_errors,
  upper_95 = ar_coefficients + 2 * ar_standard_errors
)

print(ar_confidence_intervals)

# AR MODEL DIAGNOSTICS ----------------------------------------------------------
trade_ar_residuals <- na.omit(trade_ar$resid)
acf(trade_ar_residuals, main = "ACF: AR Model Residuals")

# FOUR-MONTH AR FORECAST --------------------------------------------------------
trade_ar_forecast <- predict(trade_ar, n.ahead = 4)$pred

ts.plot(
  trade_post[1:4],
  trade_ar_forecast,
  lty = c(1, 2),
  main = "Actual vs AR Forecast",
  ylab = "Trade Ratio (%)"
)

legend(
  "topleft",
  legend = c("Actual", "Forecast"),
  lty = c(1, 2),
  bty = "n"
)

mape <- function(actual, predicted) {
  mean(abs((actual - predicted) / actual), na.rm = TRUE) * 100
}

ar_mape <- mape(trade_post[1:4], trade_ar_forecast)
print(ar_mape)
# Original result: approximately 6.47%.

# TREND REGRESSION --------------------------------------------------------------
time_index <- time(trade_pre)

trend_model <- lm(trade_pre ~ time_index)
print(summary(trend_model))
print(confint(trend_model))

# TREND AND SEASONAL REGRESSION -------------------------------------------------
season <- factor(cycle(trade_pre))

trend_season_model <- lm(
  trade_pre ~ 0 + time_index + season
)

print(summary(trend_season_model))
print(confint(trend_season_model))

trend_season_residuals <- residuals(trend_season_model)
acf(
  trend_season_residuals,
  main = "ACF: Trend-and-Seasonality Residuals"
)

# GENERALIZED LEAST SQUARES -----------------------------------------------------
gls_model <- gls(
  as.numeric(trade_pre) ~ 0 + time_index + season,
  correlation = corAR1(form = ~ time_index)
)

print(summary(gls_model))

# Preserve the original coefficient-based four-period forecast logic.
gls_coefficients <- coef(gls_model)
alpha <- gls_coefficients["time_index"]
seasonal_coefficients <- gls_coefficients[grep("^season", names(gls_coefficients))]

new_time <- seq(
  tail(time_index, 1) + 1 / 12,
  by = 1 / 12,
  length.out = 4
)

trade_gls_forecast <- as.numeric(
  alpha * new_time + seasonal_coefficients[1:4]
)

gls_mape <- mape(trade_post[1:4], trade_gls_forecast)

model_comparison <- data.frame(
  model = c("Autoregressive", "GLS trend and seasonality"),
  MAPE = c(ar_mape, gls_mape)
)

print(model_comparison)
# Original conclusion: the AR forecast performed better.
# Original reported GLS MAPE: approximately 7.34%.

# KEY FINDINGS -----------------------------------------------------------------
# - The trade ratio exhibits short-run positive autocorrelation.
# - A positive time trend is statistically significant.
# - Seasonal effects are present after controlling for trend.
# - Forecast accuracy declines as the horizon increases.
# - The AR model produced lower out-of-sample MAPE than the GLS alternative.

# FUTURE IMPROVEMENTS -----------------------------------------------------------
# - Compare ARIMA, ETS, and dynamic regression models using rolling validation.
# - Add prediction intervals to all forecast comparisons.
# - Investigate structural breaks in trade behavior.
