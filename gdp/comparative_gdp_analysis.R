# ==============================================================================
# Comparative GDP growth forecasting: North America vs Europe
# Language: R
#
#   Rscript gdp/fetch_oecd_gdp.R              # once: download the OECD input data
#   Rscript gdp/comparative_gdp_analysis.R    # this analysis -> results/
#   Rscript gdp/rolling_origin_evaluation.R   # chronological cross-validation -> results/
#
# Run from the repository root.
# ==============================================================================


# PROJECT OVERVIEW -------------------------------------------------------------
# This project compares quarterly GDP growth in North America and Europe using:
# - quarterly growth-rate transformation,
# - STL decomposition and Holt-Winters smoothing,
# - ARIMA forecasting,
# - out-of-sample RMSE and MAPE, against naive baselines,
# - residual diagnostics, and
# - GARCH volatility modeling.
#
# FORECAST TARGET AND UNITS
# - Target: quarter-over-quarter growth of seasonally adjusted real GDP,
#   computed as 100 * [log(GDP_t) - log(GDP_t-1)], in PERCENT PER QUARTER
#   (not annualized).
# - North America = simple average of the US and Canada growth rates.
# - Europe = Germany, the European proxy used in the final report.
#
# The script is aligned with the accompanying final report and preserves its
# analytical framework and model interpretation. Numbers quoted from the report
# are marked "Reported in the original report"; see the README for how they
# compare with a re-run on freshly downloaded data.

# REQUIRED PACKAGES ------------------------------------------------------------
source(file.path("R", "packages.R"))
require_packages(c("forecast", "tseries", "FinTS", "rugarch"))

set.seed(42) # GARCH estimation uses a randomized solver fallback

results_dir <- "results"
figures_dir <- file.path(results_dir, "figures")
dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)

save_plot <- function(name, expr, width = 900, height = 600) {
  png(file.path(figures_dir, paste0(name, ".png")), width = width, height = height)
  on.exit(dev.off())
  expr
}

report <- character()
add <- function(...) report <<- c(report, paste0(...))
md_table <- function(df, digits = 3) {
  df <- as.data.frame(df)
  num <- vapply(df, is.numeric, logical(1))
  df[num] <- lapply(df[num], function(x) formatC(x, format = "f", digits = digits))
  c(paste0("| ", paste(names(df), collapse = " | "), " |"),
    paste0("|", paste(rep("---", ncol(df)), collapse = "|"), "|"),
    apply(df, 1, function(r) paste0("| ", paste(r, collapse = " | "), " |")))
}

# DATA AVAILABILITY -------------------------------------------------------------
# Expected file: data/comparative_gdp_oecd.csv (created by gdp/fetch_oecd_gdp.R)
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
      "Run `Rscript gdp/fetch_oecd_gdp.R` first",
      "(required columns: date, united_states, canada, europe)."
    ),
    call. = FALSE
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

gdp_data$united_states <- as.numeric(gdp_data$united_states)
gdp_data$canada <- as.numeric(gdp_data$canada)
gdp_data$europe <- as.numeric(gdp_data$europe)

if (anyNA(gdp_data[required_columns[-1]]) || any(gdp_data[required_columns[-1]] <= 0)) {
  stop("GDP levels must be positive and complete.", call. = FALSE)
}

# Quarter of the first observation, from a date like "1995-Q1".
first_year <- as.integer(substr(gdp_data$date[1], 1, 4))
first_quarter <- as.integer(sub(".*Q", "", gdp_data$date[1]))

# GDP GROWTH TRANSFORMATION -----------------------------------------------------
# Quarter-over-quarter logarithmic growth, percent per quarter:
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

growth_start <- c(first_year + (first_quarter == 4), first_quarter %% 4 + 1)

north_america_ts <- ts(
  north_america_growth,
  start = growth_start,
  frequency = 4
)

europe_ts <- ts(
  europe_growth,
  start = growth_start,
  frequency = 4
)

add("# Comparative GDP results")
add("")
add(sprintf("Data: `%s`, %d quarters (%s to %s), %d growth observations per region. ",
            data_path, nrow(gdp_data), gdp_data$date[1], gdp_data$date[nrow(gdp_data)],
            length(north_america_ts)),
    "Target: quarter-over-quarter log growth of seasonally adjusted real GDP, % per quarter.")
add("")

# EXPLORATORY ANALYSIS ----------------------------------------------------------
save_plot("growth_series", {
  ts.plot(
    north_america_ts,
    europe_ts,
    lty = c(1, 2),
    main = "Quarterly GDP Growth: North America vs Europe",
    ylab = "Quarter-over-Quarter Growth (%)"
  )
  legend("topleft", legend = c("North America", "Europe"), lty = c(1, 2), bty = "n")
})

summary_statistics <- data.frame(
  region = c("North America", "Europe"),
  mean = c(mean(north_america_ts), mean(europe_ts)),
  standard_deviation = c(sd(north_america_ts), sd(europe_ts)),
  minimum = c(min(north_america_ts), min(europe_ts)),
  maximum = c(max(north_america_ts), max(europe_ts))
)

print(summary_statistics)
write.csv(summary_statistics, file.path(results_dir, "summary_statistics.csv"), row.names = FALSE)
add("## Descriptive statistics (% per quarter)")
add("")
add(md_table(summary_statistics))
add("")

# Reported in the original report (see README: not consistent with QoQ growth of
# real GDP, which averages well under 1% per quarter for the US and Canada):
# North America mean: approximately 4.48
# North America standard deviation: approximately 0.67
# Europe mean: approximately 0.76
# Europe standard deviation: approximately 0.47

# TREND AND SEASONALITY ---------------------------------------------------------
north_america_stl <- stl(north_america_ts, s.window = "periodic")
europe_stl <- stl(europe_ts, s.window = "periodic")
save_plot("stl_north_america", plot(north_america_stl))
save_plot("stl_europe", plot(europe_stl))

north_america_hw <- HoltWinters(north_america_ts)
europe_hw <- HoltWinters(europe_ts)
save_plot("holt_winters_north_america", plot(north_america_hw))
save_plot("holt_winters_europe", plot(europe_hw))

# STATIONARITY -----------------------------------------------------------------
adf_na <- adf.test(north_america_ts)
adf_eu <- adf.test(europe_ts)
print(adf_na)
print(adf_eu)

# TRAIN-TEST SPLIT (chronological) -----------------------------------------------
# The last 8 quarters are held out; models never see them during fitting.
forecast_horizon <- 8

north_america_train <- head(north_america_ts, length(north_america_ts) - forecast_horizon)
north_america_test <- tail(north_america_ts, forecast_horizon)
europe_train <- head(europe_ts, length(europe_ts) - forecast_horizon)
europe_test <- tail(europe_ts, forecast_horizon)

# ARIMA MODELING ---------------------------------------------------------------
north_america_arima <- auto.arima(north_america_train, seasonal = TRUE)
europe_arima <- auto.arima(europe_train, seasonal = TRUE)

print(summary(north_america_arima))
print(summary(europe_arima))

save_plot("residuals_north_america", checkresiduals(north_america_arima, plot = TRUE))
save_plot("residuals_europe", checkresiduals(europe_arima, plot = TRUE))

north_america_forecast <- forecast(north_america_arima, h = forecast_horizon, level = c(80, 95))
europe_forecast <- forecast(europe_arima, h = forecast_horizon, level = c(80, 95))

save_plot("holdout_forecast_north_america", {
  plot(north_america_forecast, main = "North America GDP Growth Forecast (holdout)")
  lines(north_america_test, lty = 2)
})
save_plot("holdout_forecast_europe", {
  plot(europe_forecast, main = "Europe GDP Growth Forecast (holdout)")
  lines(europe_test, lty = 2)
})

# FORECAST ACCURACY: IN-SAMPLE FIT VS OUT-OF-SAMPLE FORECASTS -----------------------
# accuracy() returns a "Training set" row (one-step in-sample fit) and a "Test set"
# row (genuine forecasts of the held-out quarters). Only the latter is forecast skill.
# Naive (last value) and mean baselines are scored on the same test quarters.

evaluate_holdout <- function(region, train, test, arima_fc) {
  h <- length(test)
  models <- list(
    ARIMA = arima_fc,
    Naive = naive(train, h = h, level = c(80, 95)),
    `Seasonal naive` = snaive(train, h = h, level = c(80, 95)),
    Mean = meanf(train, h = h, level = c(80, 95))
  )
  rows <- lapply(names(models), function(m) {
    acc <- accuracy(models[[m]], test)
    fc <- models[[m]]
    data.frame(
      region = region, model = m,
      in_sample_RMSE = acc["Training set", "RMSE"],
      test_RMSE = acc["Test set", "RMSE"],
      test_MAE = acc["Test set", "MAE"],
      test_MAPE = acc["Test set", "MAPE"],
      PI80_coverage = mean(test >= fc$lower[, "80%"] & test <= fc$upper[, "80%"]),
      PI95_coverage = mean(test >= fc$lower[, "95%"] & test <= fc$upper[, "95%"]),
      check.names = FALSE
    )
  })
  do.call(rbind, rows)
}

accuracy_comparison <- rbind(
  evaluate_holdout("North America", north_america_train, north_america_test, north_america_forecast),
  evaluate_holdout("Europe", europe_train, europe_test, europe_forecast)
)

print(accuracy_comparison)
write.csv(accuracy_comparison, file.path(results_dir, "holdout_accuracy.csv"), row.names = FALSE)

add(sprintf("## Single holdout: last %d quarters (%s to %s)", forecast_horizon,
            paste(start(north_america_test), collapse = "-Q"), paste(end(north_america_test), collapse = "-Q")))
add("")
add(sprintf("ARIMA orders chosen on the training data: North America %s, Europe %s.",
            paste(arimaorder(north_america_arima), collapse = ","), paste(arimaorder(europe_arima), collapse = ",")))
add("`in_sample_RMSE` is fit on the training data; every `test_` column and the interval coverage are out-of-sample. ",
    "MAPE is unstable when growth is near zero. With 8 test quarters, coverage moves in steps of 12.5%.")
add("")
add(md_table(accuracy_comparison))
add("")

# Reported in the original report: RMSE and MAPE were computed on this 8-quarter holdout
# without baselines.

# RESIDUAL AND ARCH DIAGNOSTICS -------------------------------------------------
north_america_residuals <- residuals(north_america_arima)
europe_residuals <- residuals(europe_arima)

diagnostics <- data.frame(
  region = c("North America", "Europe"),
  ADF_p = c(adf_na$p.value, adf_eu$p.value),
  LjungBox_lag8_p = c(Box.test(north_america_residuals, lag = 8, type = "Ljung-Box")$p.value,
                      Box.test(europe_residuals, lag = 8, type = "Ljung-Box")$p.value),
  ARCH_LM_lag8_p = c(ArchTest(north_america_residuals, lags = 8)$p.value,
                     ArchTest(europe_residuals, lags = 8)$p.value)
)
print(diagnostics)
write.csv(diagnostics, file.path(results_dir, "diagnostics.csv"), row.names = FALSE)
add("## Diagnostics (p-values)")
add("")
add(md_table(diagnostics))
add("")

# Reported in the original report:
# Ljung-Box p-values: North America 0.264, Europe 0.441
# ARCH-LM p-values: North America approximately 0.892, Europe approximately 0.135

# GARCH VOLATILITY MODELS -------------------------------------------------------
garch_specification <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(1, 1), include.mean = TRUE),
  distribution.model = "norm"
)

north_america_garch <- ugarchfit(spec = garch_specification, data = north_america_ts, solver = "hybrid")
europe_garch <- ugarchfit(spec = garch_specification, data = europe_ts, solver = "hybrid")

print(north_america_garch)
print(europe_garch)

garch_table <- data.frame(
  region = c("North America", "Europe"),
  alpha1 = c(coef(north_america_garch)[["alpha1"]], coef(europe_garch)[["alpha1"]]),
  beta1 = c(coef(north_america_garch)[["beta1"]], coef(europe_garch)[["beta1"]]),
  mean_conditional_sd = c(mean(sigma(north_america_garch)), mean(sigma(europe_garch)))
)
add("## GARCH(1,1) with ARMA(1,1) mean (full sample)")
add("")
add(md_table(garch_table))
add("")

save_plot("garch_volatility_north_america",
          plot(as.numeric(sigma(north_america_garch)), type = "l",
               main = "North America Conditional Volatility", ylab = "Conditional Standard Deviation"))
save_plot("garch_volatility_europe",
          plot(as.numeric(sigma(europe_garch)), type = "l",
               main = "Europe Conditional Volatility", ylab = "Conditional Standard Deviation"))

# FINAL TWO-YEAR FORECAST (with prediction intervals) ------------------------------
# Refit on all data and forecast 8 quarters beyond the sample. There is no
# actual data for these quarters in the analysis window, so they can't be scored.
north_america_final_model <- auto.arima(north_america_ts, seasonal = TRUE)
europe_final_model <- auto.arima(europe_ts, seasonal = TRUE)

north_america_final_forecast <- forecast(north_america_final_model, h = 8, level = c(80, 95))
europe_final_forecast <- forecast(europe_final_model, h = 8, level = c(80, 95))

print(north_america_final_forecast)
print(europe_final_forecast)

forecast_table <- function(region, fc) {
  t <- time(fc$mean)
  data.frame(
    region = region,
    quarter = sprintf("%d-Q%d", floor(t + 1e-6), round((t - floor(t + 1e-6)) * 4) + 1),
    point = as.numeric(fc$mean),
    lower_80 = as.numeric(fc$lower[, "80%"]), upper_80 = as.numeric(fc$upper[, "80%"]),
    lower_95 = as.numeric(fc$lower[, "95%"]), upper_95 = as.numeric(fc$upper[, "95%"])
  )
}
final_forecasts <- rbind(forecast_table("North America", north_america_final_forecast),
                         forecast_table("Europe", europe_final_forecast))
write.csv(final_forecasts, file.path(results_dir, "final_forecast.csv"), row.names = FALSE)
add(sprintf("## Forecast beyond the sample, with 80%% and 95%% prediction intervals (models: North America %s, Europe %s)",
            paste(arimaorder(north_america_final_model), collapse = ","),
            paste(arimaorder(europe_final_model), collapse = ",")))
add("")
add(md_table(final_forecasts))
add("")
save_plot("final_forecast_north_america", plot(north_america_final_forecast, main = "North America: 8-quarter forecast"))
save_plot("final_forecast_europe", plot(europe_final_forecast, main = "Europe: 8-quarter forecast"))

# Reported in the original report:
# North America: approximately 4.63%
# Europe: approximately 0.73%

# STATISTICAL COMPARISON --------------------------------------------------------
mean_difference_test <- t.test(as.numeric(north_america_ts), as.numeric(europe_ts))
variance_comparison <- var.test(as.numeric(north_america_ts), as.numeric(europe_ts))

print(mean_difference_test)
print(variance_comparison)
add("## Regional comparison")
add("")
add(sprintf("Welch t-test, difference in mean growth: %.3f pp (95%% CI %.3f to %.3f), p = %.3g. ",
            unname(diff(rev(mean_difference_test$estimate))), mean_difference_test$conf.int[1],
            mean_difference_test$conf.int[2], mean_difference_test$p.value),
    sprintf("F-test, variance ratio: %.2f, p = %.3g. ", unname(variance_comparison$estimate), variance_comparison$p.value),
    "Both tests assume independent observations, which serially correlated growth rates are not, so the p-values are optimistic.")
add("")

writeLines(report, file.path(results_dir, "gdp_results.md"))
writeLines(capture.output(sessionInfo()), file.path(results_dir, "sessionInfo.txt"))
cat("\nWrote results/gdp_results.md, CSV tables and figures/.\n")

# KEY FINDINGS (from the original report) ----------------------------------------
# - North America demonstrated higher average growth and greater volatility.
# - Seasonal effects were limited in both regional series.
# - ARIMA forecasts preserved a persistent regional growth differential.
# - Residual tests suggested limited ARCH evidence, but GARCH was retained to
#   provide a complete volatility comparison.
# - The final report identified a persistent growth gap of about 3.9 percentage
#   points between the two regional series.

# LIMITATIONS ------------------------------------------------------------------
# - The European implementation used German data as a regional proxy.
# - Structural breaks (2008-09, 2020) may weaken stationarity assumptions.
# - Limited ARCH evidence may reduce the practical value of GARCH estimates.
# - The sample may not fully represent long-run economic cycles.
# - A single 8-quarter holdout is a small test; see rolling_origin_evaluation.R.

# FUTURE IMPROVEMENTS -----------------------------------------------------------
# - Replace the European proxy with an aggregate EU series.
# - Add structural-break tests and crisis-period indicators.
# - Compare frequentist ARIMA models with Bayesian forecasting methods.
# - Extend the analysis to multivariate cross-regional spillover models.
