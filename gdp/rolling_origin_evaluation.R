# ==============================================================================
# Rolling-origin (time-series cross-validation) evaluation of the GDP forecasts.
#
#   Rscript gdp/rolling_origin_evaluation.R   # after gdp/fetch_oecd_gdp.R; run from repo root
#
# A single 8-quarter holdout scores one stretch of history. Here every quarter from
# MIN_TRAIN onward serves as a forecast origin: the model is refitted on data up to
# that origin only (expanding window, strictly chronological), forecasts 1 to 8
# quarters ahead, and is scored against what actually happened. ARIMA (auto.arima,
# re-selected at every origin, as in the main analysis) is compared with:
#   - naive:          next value = last observed value
#   - seasonal naive: next value = value four quarters earlier
#   - mean:           next value = mean of all data so far
# Target and units: quarter-over-quarter log growth of real GDP, % per quarter.
#
# Writes results/rolling_origin_metrics.csv and results/rolling_origin.md.
# ==============================================================================

source(file.path("R", "packages.R"))
require_packages(c("forecast"))

MIN_TRAIN <- 60  # quarters in the first training window (15 years)
H <- 8           # forecast horizons 1..8

gdp <- read.csv(file.path("data", "comparative_gdp_oecd.csv"), stringsAsFactors = FALSE)
first_year <- as.integer(substr(gdp$date[1], 1, 4))
first_quarter <- as.integer(sub(".*Q", "", gdp$date[1]))
growth <- function(x) diff(log(as.numeric(x))) * 100
start <- c(first_year + (first_quarter == 4), first_quarter %% 4 + 1)
series <- list(
  `North America` = ts(rowMeans(cbind(growth(gdp$united_states), growth(gdp$canada))), start = start, frequency = 4),
  Europe = ts(growth(gdp$europe), start = start, frequency = 4)
)

forecasters <- list(
  ARIMA = function(y) forecast(auto.arima(y, seasonal = TRUE), h = H, level = c(80, 95)),
  Naive = function(y) naive(y, h = H, level = c(80, 95)),
  `Seasonal naive` = function(y) snaive(y, h = H, level = c(80, 95)),
  Mean = function(y) meanf(y, h = H, level = c(80, 95))
)

records <- list()
for (region in names(series)) {
  y <- series[[region]]
  n <- length(y)
  for (origin in MIN_TRAIN:(n - 1)) {
    train <- window(y, end = time(y)[origin])
    steps <- seq_len(min(H, n - origin))
    actual <- as.numeric(y[origin + steps])
    for (model in names(forecasters)) {
      fc <- forecasters[[model]](train)
      records[[length(records) + 1]] <- data.frame(
        region = region, model = model, origin = origin, h = steps,
        error = actual - as.numeric(fc$mean)[steps],
        in80 = actual >= fc$lower[steps, "80%"] & actual <= fc$upper[steps, "80%"],
        in95 = actual >= fc$lower[steps, "95%"] & actual <= fc$upper[steps, "95%"]
      )
    }
  }
}
errors <- do.call(rbind, records)

summarise <- function(d) {
  data.frame(
    n_forecasts = nrow(d),
    RMSE = sqrt(mean(d$error^2)),
    MAE = mean(abs(d$error)),
    PI80_coverage = mean(d$in80),
    PI95_coverage = mean(d$in95)
  )
}
keys <- unique(errors[, c("region", "model", "h")])
metrics <- do.call(rbind, lapply(seq_len(nrow(keys)), function(i) {
  k <- keys[i, ]
  cbind(k, summarise(errors[errors$region == k$region & errors$model == k$model & errors$h == k$h, ]))
}))
# Relative RMSE: below 1 means the model beats the naive forecast at that horizon.
naive_rmse <- metrics[metrics$model == "Naive", c("region", "h", "RMSE")]
names(naive_rmse)[3] <- "naive_RMSE"
metrics <- merge(metrics, naive_rmse, by = c("region", "h"))
metrics$RMSE_vs_naive <- metrics$RMSE / metrics$naive_RMSE
metrics$naive_RMSE <- NULL
metrics <- metrics[order(metrics$region, metrics$model, metrics$h), ]

dir.create("results", showWarnings = FALSE)
write.csv(metrics, file.path("results", "rolling_origin_metrics.csv"), row.names = FALSE)

overall <- do.call(rbind, lapply(split(errors, list(errors$region, errors$model), drop = TRUE), function(d) {
  cbind(region = d$region[1], model = d$model[1], summarise(d))
}))
overall <- overall[order(overall$region, overall$RMSE), ]
fmt <- function(df) {
  num <- vapply(df, is.numeric, logical(1))
  df[num] <- lapply(df[num], function(x) ifelse(abs(x - round(x)) < 1e-9 & abs(x) >= 1,
                                                 format(round(x)), formatC(x, format = "f", digits = 3)))
  c(paste0("| ", paste(names(df), collapse = " | "), " |"),
    paste0("|", paste(rep("---", ncol(df)), collapse = "|"), "|"),
    apply(df, 1, function(r) paste0("| ", paste(r, collapse = " | "), " |")))
}
h_table <- function(hs) metrics[metrics$h %in% hs, c("region", "model", "h", "RMSE", "RMSE_vs_naive", "PI80_coverage", "PI95_coverage")]

origins <- MIN_TRAIN:(length(series[[1]]) - 1)
qlabel <- function(t) sprintf("%d-Q%d", as.integer(floor(t + 1e-6)), as.integer(round((t - floor(t + 1e-6)) * 4)) + 1L)
tt <- time(series[[1]])
writeLines(c(
  "# Rolling-origin evaluation",
  "",
  sprintf("Expanding window, %d forecast origins per region. The first training window is %d quarters (%s to %s); the last origin is %s. ",
          length(origins), MIN_TRAIN, qlabel(tt[1]), qlabel(tt[MIN_TRAIN]), qlabel(tt[length(tt) - 1])),
  "Errors are actual minus forecast, in percentage points of quarterly growth. All numbers are out-of-sample.",
  "",
  "## All horizons pooled",
  "",
  fmt(overall),
  "",
  "## By horizon (h = 1, 4, 8 quarters ahead)",
  "",
  "`RMSE_vs_naive` below 1 means better than the naive forecast at that horizon. Nominal coverage is 0.80 and 0.95.",
  "",
  fmt(h_table(c(1, 4, 8))),
  "",
  sprintf("Full table (h = 1 to %d): results/rolling_origin_metrics.csv", H)
), file.path("results", "rolling_origin.md"))

print(overall)
cat("\nWrote results/rolling_origin.md and results/rolling_origin_metrics.csv\n")
