# ==============================================================================
# GitHub Portfolio Project
# Language: R
#
# This script presents a reproducible time-series analysis using relative paths,
# clear sectioning, model diagnostics, and portfolio-focused documentation.
# ==============================================================================


# PROJECT OVERVIEW -------------------------------------------------------------
# This project examines the dynamic relationship between U.S. real GDP growth
# and changes in the federal funds rate using regression and vector autoregression.
#
# The original analysis logic and four-quarter forecasting horizon are retained.

# REQUIRED PACKAGES ------------------------------------------------------------
required_packages <- c("readxl", "vars", "tseries")

missing_packages <- required_packages[
  !required_packages %in% rownames(installed.packages())
]

if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

library(readxl)
library(vars)
library(tseries)

# DATA AVAILABILITY -------------------------------------------------------------
# Expected file: data/ffrategdp.xls

data_path <- file.path("data", "ffrategdp.xls")

if (!file.exists(data_path)) {
  stop("Missing data/ffrategdp.xls. Add the dataset before running this script.")
}

# DATA LOADING AND PREPARATION --------------------------------------------------
economic_data <- read_excel(data_path)

observation_date <- as.Date(economic_data[[1]])
federal_funds_rate <- as.numeric(economic_data[[2]])
real_gdp <- as.numeric(economic_data[[3]])

start_year <- as.numeric(format(min(observation_date), "%Y"))
start_month <- as.numeric(format(min(observation_date), "%m"))
start_quarter <- ((start_month - 1) %/% 3) + 1

economic_ts <- ts(
  cbind(
    federal_funds_rate = federal_funds_rate,
    real_gdp = real_gdp
  ),
  start = c(start_year, start_quarter),
  frequency = 4
)

economic_ts <- na.omit(economic_ts)

# EXPLORATORY ANALYSIS ----------------------------------------------------------
plot(
  economic_ts,
  main = "Federal Funds Rate and Real GDP"
)

fed_rate <- economic_ts[, "federal_funds_rate"]
gdp <- economic_ts[, "real_gdp"]

# LEVEL REGRESSION --------------------------------------------------------------
level_model <- lm(gdp ~ fed_rate)
print(summary(level_model))

# STATIONARITY CHECKS -----------------------------------------------------------
print(adf.test(gdp))
print(adf.test(fed_rate))

# STATIONARY TRANSFORMATIONS ----------------------------------------------------
gdp_growth <- diff(log(gdp)) * 100
fed_rate_difference <- diff(fed_rate)

var_data <- na.omit(
  cbind(
    gdpgrowth = gdp_growth,
    dfrate = fed_rate_difference
  )
)

print(adf.test(var_data[, "gdpgrowth"]))
print(adf.test(var_data[, "dfrate"]))

# REGRESSION USING TRANSFORMED SERIES ------------------------------------------
transformed_model <- lm(
  var_data[, "gdpgrowth"] ~ var_data[, "dfrate"]
)

print(summary(transformed_model))

# VAR LAG SELECTION -------------------------------------------------------------
lag_selection <- VARselect(
  var_data,
  lag.max = 8,
  type = "const"
)

print(lag_selection$selection)

selected_lag <- lag_selection$selection[["AIC(n)"]]

# VECTOR AUTOREGRESSION ---------------------------------------------------------
gdp_rate_var <- VAR(
  var_data,
  p = selected_lag,
  type = "const"
)

print(summary(gdp_rate_var))

# RESIDUAL DIAGNOSTICS ----------------------------------------------------------
gdp_var_residuals <- residuals(gdp_rate_var)[, "gdpgrowth"]
rate_var_residuals <- residuals(gdp_rate_var)[, "dfrate"]

acf(
  gdp_var_residuals,
  main = "ACF: GDP Growth VAR Residuals"
)

pacf(
  gdp_var_residuals,
  main = "PACF: GDP Growth VAR Residuals"
)

acf(
  rate_var_residuals,
  main = "ACF: Rate-Change VAR Residuals"
)

pacf(
  rate_var_residuals,
  main = "PACF: Rate-Change VAR Residuals"
)

print(
  serial.test(
    gdp_rate_var,
    lags.pt = 16,
    type = "PT.asymptotic"
  )
)

# IMPULSE-RESPONSE ANALYSIS -----------------------------------------------------
rate_shock_response <- irf(
  gdp_rate_var,
  impulse = "dfrate",
  response = "gdpgrowth",
  n.ahead = 12,
  boot = TRUE
)

plot(rate_shock_response)

# FOUR-QUARTER FORECAST ---------------------------------------------------------
var_prediction <- predict(
  gdp_rate_var,
  n.ahead = 4,
  ci = 0.95
)

print(var_prediction)

forecast_start <- tsp(var_data)[2] + 1 / 4

gdp_growth_forecast <- ts(
  var_prediction$fcst$gdpgrowth[, "fcst"],
  start = forecast_start,
  frequency = 4
)

rate_change_forecast <- ts(
  var_prediction$fcst$dfrate[, "fcst"],
  start = forecast_start,
  frequency = 4
)

ts.plot(
  var_data[, "gdpgrowth"],
  gdp_growth_forecast,
  lty = c(1, 2),
  main = "GDP Growth and Four-Quarter Forecast",
  ylab = "Growth Rate (%)"
)

legend(
  "topleft",
  legend = c("Historical", "Forecast"),
  lty = c(1, 2),
  bty = "n"
)

ts.plot(
  var_data[, "dfrate"],
  rate_change_forecast,
  lty = c(1, 2),
  main = "Federal Funds Rate Changes and Forecast",
  ylab = "Rate Change"
)

legend(
  "topleft",
  legend = c("Historical", "Forecast"),
  lty = c(1, 2),
  bty = "n"
)

# KEY FINDINGS -----------------------------------------------------------------
# - Modeling GDP and the federal funds rate in levels can create misleading results.
# - GDP growth and first differences of the policy rate are more suitable for VAR.
# - The GDP-growth equation showed a stronger residual fit than the rate-change equation.
# - The original forecast indicated moderate GDP growth over the next four quarters.
# - The original rate forecast did not indicate an increase over the same horizon.

# FUTURE IMPROVEMENTS -----------------------------------------------------------
# - Add inflation, unemployment, and yield-curve variables.
# - Test alternative lag criteria and rolling forecast windows.
# - Evaluate structural breaks around major monetary-policy regimes.
