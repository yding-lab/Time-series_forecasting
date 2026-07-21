# ==============================================================================
# GitHub Portfolio Project
# Language: R
#
# This script presents a reproducible time-series analysis using relative paths,
# clear sectioning, model diagnostics, and portfolio-focused documentation.
# ==============================================================================


# PROJECT OVERVIEW -------------------------------------------------------------
# This project extends the inflation analysis by modeling residual volatility.
# A SARIMA model captures the conditional mean, while a GARCH(1,1) model
# estimates time-varying conditional variance.
#
# The original inflation transformation, SARIMA workflow, ARCH diagnostics,
# and two-period volatility forecast are retained.

# REQUIRED PACKAGES ------------------------------------------------------------
required_packages <- c(
  "readxl",
  "forecast",
  "tseries",
  "FinTS",
  "fGarch"
)

missing_packages <- required_packages[
  !required_packages %in% rownames(installed.packages())
]

if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

library(readxl)
library(forecast)
library(tseries)
library(FinTS)
library(fGarch)

# DATA AVAILABILITY -------------------------------------------------------------
# Expected file: data/Inflation.xls

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

monthly_inflation <- diff(log(cpi_ts)) * 100

inflation_pre <- window(
  monthly_inflation,
  start = c(1990, 1),
  end = c(2018, 10)
)

# EXPLORATORY ANALYSIS ----------------------------------------------------------
plot(
  monthly_inflation,
  main = "Monthly U.S. Inflation Rate",
  xlab = "Year",
  ylab = "Percent"
)

acf(monthly_inflation, main = "ACF: Monthly Inflation")
pacf(monthly_inflation, main = "PACF: Monthly Inflation")

# SARIMA CONDITIONAL-MEAN MODEL -------------------------------------------------
sarima_model <- auto.arima(
  inflation_pre,
  seasonal = TRUE,
  stepwise = FALSE,
  approximation = FALSE
)

print(summary(sarima_model))
checkresiduals(sarima_model)

sarima_residuals <- na.omit(
  residuals(sarima_model)
)

# VOLATILITY DIAGNOSTICS --------------------------------------------------------
acf(
  sarima_residuals,
  main = "ACF: SARIMA Residuals"
)

acf(
  sarima_residuals^2,
  main = "ACF: Squared SARIMA Residuals"
)

arch_lm_test <- ArchTest(
  sarima_residuals,
  lags = 12
)

print(arch_lm_test)

# GARCH(1,1) MODEL --------------------------------------------------------------
garch_model <- garchFit(
  formula = ~ garch(1, 1),
  data = sarima_residuals,
  trace = FALSE
)

print(summary(garch_model))

# CONDITIONAL VOLATILITY --------------------------------------------------------
conditional_volatility <- volatility(garch_model)

plot(
  conditional_volatility,
  type = "l",
  main = "Conditional Inflation Volatility",
  xlab = "Observation",
  ylab = "Conditional Standard Deviation"
)

# TWO-PERIOD VOLATILITY FORECAST ------------------------------------------------
garch_prediction <- predict(
  garch_model,
  n.ahead = 2
)

volatility_forecast <- data.frame(
  horizon = 1:2,
  standard_deviation = garch_prediction$standardDeviation,
  variance = garch_prediction$standardDeviation^2
)

print(volatility_forecast)

# Original reported residual-volatility forecasts were approximately:
# - Period 1: 0.1298
# - Period 2: 0.1358
#
# Exact values depend on package version, optimizer behavior, and input data.

# KEY FINDINGS -----------------------------------------------------------------
# - The SARIMA model addresses serial dependence in the inflation mean process.
# - Squared residuals and the ARCH-LM test assess remaining volatility clustering.
# - GARCH(1,1) captures persistent conditional variance.
# - The model produces short-horizon forecasts of inflation uncertainty.

# FUTURE IMPROVEMENTS -----------------------------------------------------------
# - Compare Gaussian, Student-t, and skewed error distributions.
# - Evaluate EGARCH and GJR-GARCH for asymmetric volatility.
# - Use rolling volatility forecasts and out-of-sample loss functions.
