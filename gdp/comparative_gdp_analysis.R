# ==============================================================================
# Comparative GDP growth forecasting: US/Canada average vs Germany
# Language: R
#
#   Rscript gdp/fetch_oecd_gdp.R              # once: download the OECD input data
#   Rscript gdp/comparative_gdp_analysis.R    # this analysis -> results/
#   Rscript gdp/rolling_origin_evaluation.R   # chronological cross-validation -> results/
#
# Run from the repository root.
# ==============================================================================


# PROJECT OVERVIEW -------------------------------------------------------------
# This project compares quarterly GDP growth of the United States and Canada (averaged)
# with Germany, the original report's stand-in for Europe, using:
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
# - US/Canada average = unweighted mean of the US and Canadian growth rates, quarter by
#   quarter. It is NOT an official North American aggregate: the US economy is roughly ten
#   times Canada's, so a GDP-weighted aggregate would track the US much more closely.
# - Germany = Germany alone. The original report used it as a proxy for Europe; it is
#   labelled as Germany here because it is not a European aggregate.
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
# date, united_states, canada, germany
#
# "germany" should contain the same European series used in the final report.
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
      "(required columns: date, united_states, canada, germany)."
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
  "germany"
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
gdp_data$germany <- as.numeric(gdp_data$germany)

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
germany_growth <- diff(log(gdp_data$germany)) * 100

us_canada_growth <- rowMeans(
  cbind(
    us_growth,
    canada_growth
  ),
  na.rm = TRUE
)

growth_start <- c(first_year + (first_quarter == 4), first_quarter %% 4 + 1)

us_canada_ts <- ts(
  us_canada_growth,
  start = growth_start,
  frequency = 4
)

germany_ts <- ts(
  germany_growth,
  start = growth_start,
  frequency = 4
)

add("# Comparative GDP results")
add("")
add(sprintf("Data: `%s`, %d quarters (%s to %s), %d growth observations per region. ",
            data_path, nrow(gdp_data), gdp_data$date[1], gdp_data$date[nrow(gdp_data)],
            length(us_canada_ts)),
    "Target: quarter-over-quarter log growth of seasonally adjusted real GDP, % per quarter.")
add("")

# EXPLORATORY ANALYSIS ----------------------------------------------------------
save_plot("growth_series", {
  ts.plot(
    us_canada_ts,
    germany_ts,
    lty = c(1, 2),
    main = "Quarterly GDP Growth: US/Canada average vs Germany",
    ylab = "Quarter-over-Quarter Growth (%)"
  )
  legend("topleft", legend = c("US/Canada average", "Germany"), lty = c(1, 2), bty = "n")
})

summary_statistics <- data.frame(
  region = c("US/Canada average", "Germany"),
  mean = c(mean(us_canada_ts), mean(germany_ts)),
  standard_deviation = c(sd(us_canada_ts), sd(germany_ts)),
  minimum = c(min(us_canada_ts), min(germany_ts)),
  maximum = c(max(us_canada_ts), max(germany_ts))
)

print(summary_statistics)
write.csv(summary_statistics, file.path(results_dir, "summary_statistics.csv"), row.names = FALSE)
add("## Descriptive statistics (% per quarter)")
add("")
add(md_table(summary_statistics))
add("")

# Reported in the original report, which labelled the series "North America" and
# "Europe" (see README: not consistent with QoQ growth of
# real GDP, which averages well under 1% per quarter for the US and Canada):
# "North America" mean: approximately 4.48
# "North America" standard deviation: approximately 0.67
# Germany mean: approximately 0.76
# Germany standard deviation: approximately 0.47

# TREND AND SEASONALITY ---------------------------------------------------------
us_canada_stl <- stl(us_canada_ts, s.window = "periodic")
germany_stl <- stl(germany_ts, s.window = "periodic")
save_plot("stl_us_canada", plot(us_canada_stl))
save_plot("stl_germany", plot(germany_stl))

us_canada_hw <- HoltWinters(us_canada_ts)
germany_hw <- HoltWinters(germany_ts)
save_plot("holt_winters_us_canada", plot(us_canada_hw))
save_plot("holt_winters_germany", plot(germany_hw))

# STATIONARITY -----------------------------------------------------------------
adf_na <- adf.test(us_canada_ts)
adf_eu <- adf.test(germany_ts)
print(adf_na)
print(adf_eu)

# TRAIN-TEST SPLIT (chronological) -----------------------------------------------
# The last 8 quarters are held out; models never see them during fitting.
forecast_horizon <- 8

us_canada_train <- head(us_canada_ts, length(us_canada_ts) - forecast_horizon)
us_canada_test <- tail(us_canada_ts, forecast_horizon)
germany_train <- head(germany_ts, length(germany_ts) - forecast_horizon)
germany_test <- tail(germany_ts, forecast_horizon)

# ARIMA MODELING ---------------------------------------------------------------
us_canada_arima <- auto.arima(us_canada_train, seasonal = TRUE)
germany_arima <- auto.arima(germany_train, seasonal = TRUE)

print(summary(us_canada_arima))
print(summary(germany_arima))

save_plot("residuals_us_canada", checkresiduals(us_canada_arima, plot = TRUE))
save_plot("residuals_germany", checkresiduals(germany_arima, plot = TRUE))

us_canada_forecast <- forecast(us_canada_arima, h = forecast_horizon, level = c(80, 95))
germany_forecast <- forecast(germany_arima, h = forecast_horizon, level = c(80, 95))

save_plot("holdout_forecast_us_canada", {
  plot(us_canada_forecast, main = "US/Canada average GDP Growth Forecast (holdout)")
  lines(us_canada_test, lty = 2)
})
save_plot("holdout_forecast_germany", {
  plot(germany_forecast, main = "Germany GDP Growth Forecast (holdout)")
  lines(germany_test, lty = 2)
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
  evaluate_holdout("US/Canada average", us_canada_train, us_canada_test, us_canada_forecast),
  evaluate_holdout("Germany", germany_train, germany_test, germany_forecast)
)

print(accuracy_comparison)
write.csv(accuracy_comparison, file.path(results_dir, "holdout_accuracy.csv"), row.names = FALSE)

# Holdout forecasts with intervals and the actual values, for the report figures.
holdout_frame <- function(region, fc, test) {
  t <- time(test)
  data.frame(region = region,
             quarter = sprintf("%d-Q%d", floor(t + 1e-6), round((t - floor(t + 1e-6)) * 4) + 1),
             actual = as.numeric(test), point = as.numeric(fc$mean),
             lower_80 = as.numeric(fc$lower[, "80%"]), upper_80 = as.numeric(fc$upper[, "80%"]),
             lower_95 = as.numeric(fc$lower[, "95%"]), upper_95 = as.numeric(fc$upper[, "95%"]))
}
write.csv(rbind(holdout_frame("US/Canada average", us_canada_forecast, us_canada_test),
                holdout_frame("Germany", germany_forecast, germany_test)),
          file.path(results_dir, "holdout_forecasts.csv"), row.names = FALSE)
growth_frame <- data.frame(quarter = sprintf("%d-Q%d", floor(time(us_canada_ts) + 1e-6),
                                             cycle(us_canada_ts)),
                           us = us_growth, canada = canada_growth,
                           us_canada_average = as.numeric(us_canada_ts), germany = as.numeric(germany_ts))
write.csv(growth_frame, file.path(results_dir, "growth_series.csv"), row.names = FALSE)

add(sprintf("## Single holdout: last %d quarters (%s to %s)", forecast_horizon,
            paste(start(us_canada_test), collapse = "-Q"), paste(end(us_canada_test), collapse = "-Q")))
add("")
add(sprintf("ARIMA orders chosen on the training data: US/Canada average %s, Germany %s.",
            paste(arimaorder(us_canada_arima), collapse = ","), paste(arimaorder(germany_arima), collapse = ",")))
add("`in_sample_RMSE` is fit on the training data; every `test_` column and the interval coverage are out-of-sample. ",
    "MAPE is unstable when growth is near zero. With 8 test quarters, coverage moves in steps of 12.5%.")
add("")
add(md_table(accuracy_comparison))
add("")

# Reported in the original report: RMSE and MAPE were computed on this 8-quarter holdout
# without baselines.

# RESIDUAL AND ARCH DIAGNOSTICS -------------------------------------------------
us_canada_residuals <- residuals(us_canada_arima)
germany_residuals <- residuals(germany_arima)

diagnostics <- data.frame(
  region = c("US/Canada average", "Germany"),
  ADF_p = c(adf_na$p.value, adf_eu$p.value),
  LjungBox_lag8_p = c(Box.test(us_canada_residuals, lag = 8, type = "Ljung-Box")$p.value,
                      Box.test(germany_residuals, lag = 8, type = "Ljung-Box")$p.value),
  ARCH_LM_lag8_p = c(ArchTest(us_canada_residuals, lags = 8)$p.value,
                     ArchTest(germany_residuals, lags = 8)$p.value)
)
print(diagnostics)
write.csv(diagnostics, file.path(results_dir, "diagnostics.csv"), row.names = FALSE)
add("## Diagnostics (p-values)")
add("")
add(md_table(diagnostics))
add("")

# Reported in the original report:
# Ljung-Box p-values: "North America" 0.264, Germany 0.441
# ARCH-LM p-values: "North America" approximately 0.892, Germany approximately 0.135

# GARCH VOLATILITY MODELS -------------------------------------------------------
garch_specification <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(1, 1), include.mean = TRUE),
  distribution.model = "norm"
)

us_canada_garch <- ugarchfit(spec = garch_specification, data = us_canada_ts, solver = "hybrid")
germany_garch <- ugarchfit(spec = garch_specification, data = germany_ts, solver = "hybrid")

print(us_canada_garch)
print(germany_garch)

garch_table <- data.frame(
  region = c("US/Canada average", "Germany"),
  alpha1 = c(coef(us_canada_garch)[["alpha1"]], coef(germany_garch)[["alpha1"]]),
  beta1 = c(coef(us_canada_garch)[["beta1"]], coef(germany_garch)[["beta1"]]),
  mean_conditional_sd = c(mean(sigma(us_canada_garch)), mean(sigma(germany_garch)))
)
write.csv(garch_table, file.path(results_dir, "garch.csv"), row.names = FALSE)
add("## GARCH(1,1) with ARMA(1,1) mean (full sample)")
add("")
add(md_table(garch_table))
add("")

save_plot("garch_volatility_us_canada",
          plot(as.numeric(sigma(us_canada_garch)), type = "l",
               main = "US/Canada average Conditional Volatility", ylab = "Conditional Standard Deviation"))
save_plot("garch_volatility_germany",
          plot(as.numeric(sigma(germany_garch)), type = "l",
               main = "Germany Conditional Volatility", ylab = "Conditional Standard Deviation"))

# FINAL TWO-YEAR FORECAST (with prediction intervals) ------------------------------
# Refit on all data and forecast 8 quarters beyond the sample. There is no
# actual data for these quarters in the analysis window, so they can't be scored.
us_canada_final_model <- auto.arima(us_canada_ts, seasonal = TRUE)
germany_final_model <- auto.arima(germany_ts, seasonal = TRUE)

us_canada_final_forecast <- forecast(us_canada_final_model, h = 8, level = c(80, 95))
germany_final_forecast <- forecast(germany_final_model, h = 8, level = c(80, 95))

print(us_canada_final_forecast)
print(germany_final_forecast)

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
final_forecasts <- rbind(forecast_table("US/Canada average", us_canada_final_forecast),
                         forecast_table("Germany", germany_final_forecast))
write.csv(final_forecasts, file.path(results_dir, "final_forecast.csv"), row.names = FALSE)
add(sprintf("## Forecast beyond the sample, with 80%% and 95%% prediction intervals (models: US/Canada average %s, Germany %s)",
            paste(arimaorder(us_canada_final_model), collapse = ","),
            paste(arimaorder(germany_final_model), collapse = ",")))
add("")
add(md_table(final_forecasts))
add("")
save_plot("final_forecast_us_canada", plot(us_canada_final_forecast, main = "US/Canada average: 8-quarter forecast"))
save_plot("final_forecast_germany", plot(germany_final_forecast, main = "Germany: 8-quarter forecast"))

# Reported in the original report:
# "North America": approximately 4.63%
# Germany: approximately 0.73%

# STATISTICAL COMPARISON --------------------------------------------------------
mean_difference_test <- t.test(as.numeric(us_canada_ts), as.numeric(germany_ts))
variance_comparison <- var.test(as.numeric(us_canada_ts), as.numeric(germany_ts))

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

write.csv(data.frame(
  mean_difference_pp = unname(diff(rev(mean_difference_test$estimate))),
  ci95_low = mean_difference_test$conf.int[1], ci95_high = mean_difference_test$conf.int[2],
  welch_p = mean_difference_test$p.value,
  variance_ratio = unname(variance_comparison$estimate), f_test_p = variance_comparison$p.value
), file.path(results_dir, "regional_comparison.csv"), row.names = FALSE)
writeLines(c(sprintf("arima_holdout_us_canada,%s", paste(arimaorder(us_canada_arima), collapse = "-")),
             sprintf("arima_holdout_germany,%s", paste(arimaorder(germany_arima), collapse = "-")),
             sprintf("arima_final_us_canada,%s", paste(arimaorder(us_canada_final_model), collapse = "-")),
             sprintf("arima_final_germany,%s", paste(arimaorder(germany_final_model), collapse = "-"))),
           file.path(results_dir, "arima_orders.csv"))
writeLines(report, file.path(results_dir, "gdp_results.md"))
writeLines(capture.output(sessionInfo()), file.path(results_dir, "sessionInfo.txt"))
cat("\nWrote results/gdp_results.md, CSV tables and figures/.\n")

# KEY FINDINGS (from the original report; not reproduced, see README) ---------------
# - "North America" demonstrated higher average growth and greater volatility.
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
