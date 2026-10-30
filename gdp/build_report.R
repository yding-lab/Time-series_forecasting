# ==============================================================================
# Build the current report and its key figures from the analysis outputs.
#
#   Rscript gdp/build_report.R    # after comparative_gdp_analysis.R and rolling_origin_evaluation.R
#
# Reads only files in results/ and data/*.SOURCE.txt, and writes:
#   report/comparative_gdp_report.md    the report (supersedes the original PDF)
#   report/figures/growth_series.png    quarterly growth, both series
#   report/figures/holdout_forecast.png actual vs ARIMA forecast with 80%/95% intervals
#   report/figures/error_by_horizon.png rolling-origin RMSE by horizon, every model
#
# Every number in the report is computed from those files. The only numbers typed in
# by hand are the original PDF's, quoted for comparison and marked as such.
# ==============================================================================

res <- function(f) read.csv(file.path("results", f), stringsAsFactors = FALSE, check.names = FALSE)
needed <- c("growth_series.csv", "summary_statistics.csv", "holdout_accuracy.csv", "holdout_forecasts.csv",
            "diagnostics.csv", "garch.csv", "final_forecast.csv", "regional_comparison.csv",
            "rolling_origin_metrics.csv")
missing <- needed[!file.exists(file.path("results", needed))]
if (length(missing)) {
  stop("Missing results/", paste(missing, collapse = ", results/"),
       ". Run gdp/comparative_gdp_analysis.R and gdp/rolling_origin_evaluation.R first.", call. = FALSE)
}

growth <- res("growth_series.csv")
stats_tab <- res("summary_statistics.csv")
holdout <- res("holdout_accuracy.csv")
hfc <- res("holdout_forecasts.csv")
diag <- res("diagnostics.csv")
garch <- res("garch.csv")
final_fc <- res("final_forecast.csv")
cmp <- res("regional_comparison.csv")
ro <- res("rolling_origin_metrics.csv")
orders <- setNames(sub(".*,", "", readLines(file.path("results", "arima_orders.csv"))),
                   sub(",.*", "", readLines(file.path("results", "arima_orders.csv"))))
source_txt <- readLines(file.path("data", "comparative_gdp_oecd.SOURCE.txt"))
retrieved <- sub("^Retrieved \\(UTC\\): ", "", grep("^Retrieved", source_txt, value = TRUE))
retrieved_date <- substr(retrieved, 1, 10)

REGIONS <- c("US/Canada average", "Germany")
MODELS <- c("ARIMA", "Mean", "Naive", "Seasonal naive")
COL <- c(`US/Canada average` = "#2a78d6", Germany = "#eb6834")  # validated categorical slots 1-2
MODEL_COL <- c(ARIMA = "#2a78d6", Mean = "#eb6834", Naive = "#1baf7a", `Seasonal naive` = "#eda100")
MODEL_PCH <- c(ARIMA = 16, Mean = 15, Naive = 17, `Seasonal naive` = 18)  # shape too, never colour alone
MODEL_LTY <- c(ARIMA = 1, Mean = 1, Naive = 2, `Seasonal naive` = 3)
INK2 <- "#52514e"; GRID <- "#e4e3df"
first_q <- growth$quarter[1]; last_q <- growth$quarter[nrow(growth)]
SOURCE <- sprintf("Source: OECD Quarterly National Accounts (real GDP, chain-linked, seasonally adjusted), retrieved %s. Growth = 100 x change in log GDP, %% per quarter, not annualized.",
                  retrieved_date)

fmt_order <- function(key) {  # arimaorder(): p, d, q[, P, D, Q, period]
  v <- as.integer(strsplit(orders[[key]], "-")[[1]])
  if (length(v) == 3 || all(v[4:6] == 0)) sprintf("(%d,%d,%d)", v[1], v[2], v[3])
  else sprintf("(%d,%d,%d)(%d,%d,%d)[%d]", v[1], v[2], v[3], v[4], v[5], v[6], v[7])
}
f3 <- function(x) formatC(x, format = "f", digits = 3)
f2 <- function(x) formatC(x, format = "f", digits = 2)
pct <- function(x) sprintf("%.0f%%", 100 * x)
qtime <- function(q) as.integer(substr(q, 1, 4)) + (as.integer(sub(".*Q", "", q)) - 1) / 4
md_table <- function(df) {
  c(paste0("| ", paste(names(df), collapse = " | "), " |"),
    paste0("|", paste(rep("---", ncol(df)), collapse = "|"), "|"),
    apply(df, 1, function(r) paste0("| ", paste(r, collapse = " | "), " |")))
}

dir.create(file.path("report", "figures"), recursive = TRUE, showWarnings = FALSE)
png_open <- function(name, w = 1400, h = 700) {
  png(file.path("report", "figures", name), width = w, height = h, res = 150, bg = "#fcfcfb")
  par(family = "sans", col.axis = INK2, col.lab = INK2, fg = INK2, las = 1, mgp = c(2.4, 0.6, 0), tcl = -0.3)
}
caption <- function() mtext(SOURCE, side = 1, line = -1, outer = TRUE, adj = 0.01, cex = 0.55, col = INK2)

# ---- figure 1: growth series ------------------------------------------------------
png_open("growth_series.png", h = 650)
par(mar = c(4, 4.2, 3, 1), oma = c(1.2, 0, 0, 0))
tt <- qtime(growth$quarter)
plot(tt, growth$us_canada_average, type = "n", ylim = range(growth[, c("us_canada_average", "germany")]),
     xlab = "", ylab = "Real GDP growth, % per quarter", bty = "n")
abline(h = pretty(par("usr")[3:4]), col = GRID, lwd = 0.8)
abline(h = 0, col = INK2, lwd = 0.8)
lines(tt, growth$us_canada_average, col = COL[[1]], lwd = 2)
lines(tt, growth$germany, col = COL[[2]], lwd = 2)
legend("bottomleft", legend = c("US/Canada average (unweighted mean of US and Canada)", "Germany"),
       col = COL, lwd = 2, bty = "n", cex = 0.8, text.col = "#0b0b0b")
title(sprintf("Quarterly real GDP growth, %s to %s", first_q, last_q), adj = 0, font.main = 1, cex.main = 1.05)
caption()
dev.off()

# ---- figure 2: holdout, actual vs forecast with intervals ---------------------------
png_open("holdout_forecast.png", w = 1500, h = 700)
par(mfrow = c(1, 2), mar = c(3, 4.2, 4.2, 1), oma = c(1.2, 0, 0, 0))
first_test <- qtime(hfc$quarter[1])
for (r in REGIONS) {
  h <- hfc[hfc$region == r, ]
  col <- if (r == "Germany") "germany" else "us_canada_average"
  hist_rows <- tt >= first_test - 4 & tt < first_test
  ht <- qtime(h$quarter)
  ylim <- range(c(h$lower_95, h$upper_95, h$actual, growth[[col]][hist_rows]))
  plot(NA, xlim = c(first_test - 4, max(ht)), ylim = ylim, xlab = "", ylab = "% per quarter", bty = "n")
  abline(h = pretty(ylim), col = GRID, lwd = 0.8)
  polygon(c(ht, rev(ht)), c(h$lower_95, rev(h$upper_95)), col = adjustcolor(COL[[r]], 0.15), border = NA)
  polygon(c(ht, rev(ht)), c(h$lower_80, rev(h$upper_80)), col = adjustcolor(COL[[r]], 0.30), border = NA)
  lines(tt[hist_rows | tt >= first_test], growth[[col]][hist_rows | tt >= first_test], col = "#0b0b0b", lwd = 1.5)
  points(ht, h$actual, pch = 16, cex = 0.8, col = "#0b0b0b")
  lines(ht, h$point, col = COL[[r]], lwd = 2.5)
  abline(v = first_test - 0.125, lty = 3, col = INK2)
  title(sprintf("%s: ARIMA%s, 8-quarter holdout", r,
                fmt_order(if (r == "Germany") "arima_holdout_germany" else "arima_holdout_us_canada")),
        adj = 0, font.main = 1, cex.main = 0.95, line = 2)
  legend("top", inset = c(0, -0.1), xpd = TRUE, horiz = TRUE, bty = "n", cex = 0.7,
         legend = c("actual", "forecast", "80% interval", "95% interval"),
         col = c("#0b0b0b", COL[[r]], adjustcolor(COL[[r]], 0.45), adjustcolor(COL[[r]], 0.2)),
         lwd = c(1.5, 2.5, 8, 8), pch = c(16, NA, NA, NA))
}
caption()
dev.off()

# ---- figure 3: rolling-origin RMSE by horizon -----------------------------------------
png_open("error_by_horizon.png", w = 1500, h = 700)
par(mfrow = c(1, 2), mar = c(4.2, 4.2, 3, 1), oma = c(2.6, 0, 0, 0))
ylim <- c(0, max(ro$RMSE) * 1.05)
for (r in REGIONS) {
  plot(NA, xlim = c(1, max(ro$h)), ylim = ylim, xlab = "Forecast horizon (quarters ahead)",
       ylab = "RMSE, percentage points", bty = "n", xaxt = "n")
  axis(1, at = seq_len(max(ro$h)))
  abline(h = pretty(ylim), col = GRID, lwd = 0.8)
  for (m in MODELS) {
    d <- ro[ro$region == r & ro$model == m, ]
    lines(d$h, d$RMSE, col = MODEL_COL[[m]], lwd = 2, lty = MODEL_LTY[[m]])
    points(d$h, d$RMSE, col = MODEL_COL[[m]], pch = MODEL_PCH[[m]], cex = 1.1)
  }
  title(sprintf("%s: rolling-origin RMSE (%d origins)", r, max(ro$n_forecasts)), adj = 0, font.main = 1, cex.main = 0.95)
  legend("bottomright", legend = MODELS, col = MODEL_COL, lty = MODEL_LTY, pch = MODEL_PCH, lwd = 2, bty = "n", cex = 0.75)
}
caption()
dev.off()

# ---- numbers for the text ---------------------------------------------------------------
pooled <- do.call(rbind, lapply(split(ro, list(ro$region, ro$model), drop = TRUE), function(d) {
  w <- d$n_forecasts
  data.frame(region = d$region[1], model = d$model[1], n = sum(w),
             RMSE = sqrt(sum(w * d$RMSE^2) / sum(w)), MAE = sum(w * d$MAE) / sum(w),
             PI80 = sum(w * d$PI80_coverage) / sum(w), PI95 = sum(w * d$PI95_coverage) / sum(w))
}))
pooled <- pooled[order(match(pooled$region, REGIONS), pooled$RMSE), ]
best <- sapply(REGIONS, function(r) pooled$model[pooled$region == r][1])
rel <- function(r, m, h) ro$RMSE_vs_naive[ro$region == r & ro$model == m & ro$h == h]
st <- function(r, k) stats_tab[[k]][stats_tab$region == r]
fin <- function(r) final_fc[final_fc$region == r, ]
n_origins <- max(ro$n_forecasts)
arima_vs_mean <- sapply(REGIONS, function(r) {
  pooled$RMSE[pooled$region == r & pooled$model == "ARIMA"] / pooled$RMSE[pooled$region == r & pooled$model == "Mean"]
})

pooled_md <- data.frame(Series = pooled$region, Model = pooled$model, Forecasts = pooled$n,
                        RMSE = f3(pooled$RMSE), MAE = f3(pooled$MAE),
                        `80% PI coverage` = f2(pooled$PI80), `95% PI coverage` = f2(pooled$PI95), check.names = FALSE)
hz <- ro[ro$h %in% c(1, 4, 8) & ro$model %in% c("ARIMA", "Mean"), ]
hz_md <- do.call(rbind, lapply(REGIONS, function(r) do.call(rbind, lapply(c("ARIMA", "Mean"), function(m) {
  data.frame(Series = r, Model = m, `h = 1` = f3(rel(r, m, 1)), `h = 4` = f3(rel(r, m, 4)),
             `h = 8` = f3(rel(r, m, 8)), check.names = FALSE)
}))))
hold_md <- data.frame(Series = holdout$region, Model = holdout$model, `In-sample RMSE` = f3(holdout$in_sample_RMSE),
                      `Test RMSE` = f3(holdout$test_RMSE), `Test MAE` = f3(holdout$test_MAE), check.names = FALSE)
desc_md <- data.frame(Series = stats_tab$region, Mean = f3(stats_tab$mean), SD = f3(stats_tab$standard_deviation),
                      Min = f3(stats_tab$minimum), Max = f3(stats_tab$maximum))
fc_md <- do.call(rbind, lapply(REGIONS, function(r) {
  d <- fin(r)
  data.frame(Series = r, `First quarter` = d$quarter[1], Point = f3(d$point[1]),
             `80% PI` = sprintf("%s to %s", f3(d$lower_80[1]), f3(d$upper_80[1])),
             `95% PI` = sprintf("%s to %s", f3(d$lower_95[1]), f3(d$upper_95[1])),
             `Point at h = 8` = f3(d$point[nrow(d)]), check.names = FALSE)
}))
diag_md <- data.frame(Series = diag$region, `ADF p` = f3(diag$ADF_p), `Ljung-Box p (lag 8)` = f3(diag$LjungBox_lag8_p),
                      `ARCH-LM p (lag 8)` = f3(diag$ARCH_LM_lag8_p), check.names = FALSE)
garch_md <- data.frame(Series = garch$region, alpha1 = f3(garch$alpha1), beta1 = f3(garch$beta1))
na <- REGIONS[1]; de <- REGIONS[2]
orig_md <- data.frame(
  Quantity = c("Mean growth, % per quarter (US/Canada avg / Germany)", "Standard deviation (US/Canada avg / Germany)",
               "Difference in mean growth", "ARIMA order selected (full sample)", "Ljung-Box p (US/Canada avg / Germany)",
               "ARCH-LM p (US/Canada avg / Germany)", "Long-run forecast (US/Canada avg / Germany)"),
  `Original report (PDF)` = c("4.48 / 0.76", "0.67 / 0.47", "about 3.9 pp, \"persistent\"", "(1,0,1) for both",
                              "0.264 / 0.441", "0.892 / 0.135", "about 4.63 / 0.73"),
  `This analysis` = c(sprintf("%s / %s", f3(st(na, "mean")), f3(st(de, "mean"))),
                      sprintf("%s / %s", f3(st(na, "standard_deviation")), f3(st(de, "standard_deviation"))),
                      sprintf("%s pp, 95%% CI %s to %s, p = %s", f3(cmp$mean_difference_pp), f3(cmp$ci95_low),
                              f3(cmp$ci95_high), f3(cmp$welch_p)),
                      sprintf("%s / %s", fmt_order("arima_final_us_canada"),
                              fmt_order("arima_final_germany")),
                      sprintf("%s / %s", f3(diag$LjungBox_lag8_p[1]), f3(diag$LjungBox_lag8_p[2])),
                      sprintf("%s / %s", f3(diag$ARCH_LM_lag8_p[1]), f3(diag$ARCH_LM_lag8_p[2])),
                      sprintf("%s / %s", f3(fin(na)$point[8]), f3(fin(de)$point[8]))),
  check.names = FALSE)

session <- if (file.exists(file.path("results", "sessionInfo.txt"))) readLines(file.path("results", "sessionInfo.txt")) else character()
pkg_line <- function(p) { v <- tryCatch(as.character(packageVersion(p)), error = function(e) NA); if (is.na(v)) NULL else sprintf("%s %s", p, v) }
versions <- paste(c(R.version.string, unlist(lapply(c("forecast", "tseries", "FinTS", "rugarch"), pkg_line))), collapse = ", ")

lines <- c(
  "# Quarterly GDP growth: US/Canada average vs Germany",
  "",
  sprintf("*Generated by `gdp/build_report.R` on %s (UTC) from the outputs of `gdp/comparative_gdp_analysis.R` and `gdp/rolling_origin_evaluation.R`. OECD data retrieved %s. %s.*",
          format(Sys.time(), "%Y-%m-%d", tz = "UTC"), retrieved, versions),
  "",
  "> This report supersedes the original course report ([archive/](../archive/README.md)). Several of the original's headline numbers can't be reproduced from the data source it describes; see *Relation to the original report* below.",
  "",
  "## Summary",
  "",
  sprintf("- From %s to %s, quarterly real GDP growth averaged **%s%%** for the US/Canada average and **%s%%** for Germany. The %s pp difference %s (Welch 95%% CI %s to %s, p = %s).",
          first_q, last_q, f3(st(na, "mean")), f3(st(de, "mean")), f3(cmp$mean_difference_pp),
          if (cmp$welch_p < 0.05) "is statistically significant at 5%, with the caveat on serial correlation below" else "isn't statistically significant",
          f3(cmp$ci95_low), f3(cmp$ci95_high), f2(cmp$welch_p)),
  sprintf("- In chronological rolling-origin evaluation (%d forecast origins, horizons 1 to 8), the best model by RMSE is **%s** for the US/Canada average and **%s** for Germany. ARIMA's pooled RMSE is %.2f times the historical mean's for the US/Canada average, and %.2f times for Germany.",
          n_origins, best[[na]], best[[de]], arima_vs_mean[[na]], arima_vs_mean[[de]]),
  sprintf("- Prediction intervals: pooled over horizons, nominal 80%% intervals covered %s–%s of outcomes and nominal 95%% intervals %s–%s.%s",
          pct(min(pooled$PI80)), pct(max(pooled$PI80)), pct(min(pooled$PI95)), pct(max(pooled$PI95)),
          if (all(pooled$PI80 > 0.8) && all(pooled$PI95 < 0.95)) " They're too wide in the middle and too narrow in the tails, the pattern expected when normal intervals meet fat-tailed errors (2008–09, 2020)." else ""),
  "",
  "## Data and definitions",
  "",
  md_table(data.frame(Item = c("Source", "Countries", "Window", "Target", "US/Canada average", "Germany", "Revisions"),
    Definition = c(
      "OECD Quarterly National Accounts, dataflow `DSD_NAMAIN1@DF_QNA` v1.1: real GDP (B1GQ), chain-linked volume, national currency, calendar and seasonally adjusted. Query in `data/comparative_gdp_oecd.SOURCE.txt`",
      "United States (USA), Canada (CAN), Germany (DEU)",
      sprintf("%s to %s levels, giving %d quarterly growth observations (%s to %s)", "1995-Q1", "2023-Q4", nrow(growth), first_q, last_q),
      "100 × [log(GDP_t) − log(GDP_t−1)], **percent per quarter, not annualized**. Forecast errors are in percentage points of quarterly growth",
      "The **unweighted mean of the US and Canadian growth rates**, quarter by quarter. It is *not* an official North American aggregate: US GDP is more than ten times Canada's, so a GDP-weighted aggregate would track the US much more closely. It's kept because it's the construction the original analysis used",
      "Germany alone. The original report used it as a proxy for Europe, and this OECD dataflow has no euro-area or EU aggregate for the series definition used here. Results describe Germany, not Europe",
      sprintf("National accounts are revised. These are the values as published on %s", retrieved_date)
    ))),
  "",
  "![Quarterly real GDP growth, US/Canada average and Germany](figures/growth_series.png)",
  "",
  "### Descriptive statistics, % per quarter",
  "",
  md_table(desc_md),
  "",
  "The 2008–09 recession and the 2020 pandemic quarters dominate the extremes and much of the variance.",
  "",
  "## Forecast evaluation",
  "",
  "### Rolling origin (the main result)",
  "",
  sprintf("Every quarter from the 60th observation on is a forecast origin. Each model is refitted on the data up to that origin only (expanding window), forecasts 1–8 quarters ahead, and is scored against what happened. That gives %d origins and %d forecasts per model and series. `auto.arima` re-selects its order at every origin. The baselines are naive (last value), seasonal naive (value four quarters earlier) and mean (average of all data so far).",
          n_origins, pooled$n[1]),
  "",
  md_table(pooled_md),
  "",
  "![Rolling-origin RMSE by forecast horizon for each model](figures/error_by_horizon.png)",
  "",
  "RMSE relative to the naive forecast at selected horizons (below 1 = better than naive):",
  "",
  md_table(hz_md),
  "",
  sprintf("**Why the mean beats ARIMA.** Quarter-over-quarter growth has little predictable short-run dynamics beyond its average. ARIMA adds estimated short-run dynamics that don't carry over out of sample, and after 2020 every training window contains the pandemic outliers, which distort the fitted dynamics. For the US/Canada average one quarter ahead, ARIMA's RMSE is %s times the naive forecast's, against %s for the mean. This is a real result, not a failure of the evaluation: a forecaster that can't beat the historical mean here shouldn't be preferred over it.",
          f3(rel(na, "ARIMA", 1)), f3(rel(na, "Mean", 1))),
  "",
  "**Revised data vs real-time forecasting.** This backtest uses today's revised GDP series at every origin. A forecaster at that origin would have seen an earlier vintage, often with materially different recent quarters, because first releases are revised for years. So these errors are *pseudo-out-of-sample*: they measure how well each model extrapolates the final data, and they likely understate real-time errors, especially at short horizons. A real-time evaluation would rebuild each origin from that date's data vintage, for example from the OECD's archive of original releases and revisions, or ALFRED for US data. That isn't done here.",
  "",
  "### Single 8-quarter holdout",
  "",
  sprintf("The last 8 quarters (%s to %s) are held out. ARIMA orders chosen on the training data: US/Canada average ARIMA%s, Germany ARIMA%s.",
          hfc$quarter[1], hfc$quarter[nrow(hfc) / 2], fmt_order("arima_holdout_us_canada"), fmt_order("arima_holdout_germany")),
  "",
  md_table(hold_md),
  "",
  "![Actual growth and ARIMA holdout forecasts with 80% and 95% prediction intervals](figures/holdout_forecast.png)",
  "",
  paste0(if (all(holdout$test_RMSE[holdout$model == "ARIMA"] < holdout$in_sample_RMSE[holdout$model == "ARIMA"]))
    "ARIMA's test errors are *lower* than its in-sample fit only because the holdout window was calm compared with a training period that includes 2008–09 and 2020. " else "",
    "One 8-quarter window is a small sample; the rolling-origin results above are the more reliable measure."),
  "",
  "### Forecast beyond the sample",
  "",
  sprintf("Final models refitted on all data: US/Canada average ARIMA%s, Germany ARIMA%s.",
          fmt_order("arima_final_us_canada"), fmt_order("arima_final_germany")),
  "",
  md_table(fc_md),
  "",
  "These forecasts can now be checked against published 2024–25 data by extending `END` in `gdp/fetch_oecd_gdp.R`.",
  "",
  "## Diagnostics",
  "",
  md_table(diag_md),
  "",
  "GARCH(1,1) with an ARMA(1,1) mean, full sample:",
  "",
  md_table(garch_md),
  "",
  if (any(c(garch$alpha1, garch$beta1) < 0.02 | c(garch$alpha1, garch$beta1) > 0.98))
    "Some estimates sit at the edge of the parameter space (alpha or beta near 0 or 1). That suggests a few extreme quarters, chiefly 2020, drive the fit, and the volatility dynamics aren't well identified from this sample." else
    "Interpret with care: a few extreme quarters (2008–09, 2020) carry much of the information about volatility.",
  "",
  "## Regional comparison",
  "",
  sprintf("Difference in mean growth (US/Canada average − Germany): %s pp, Welch 95%% CI %s to %s, p = %s. Variance ratio: %s (F-test p = %s). Both tests assume independent observations; growth rates are serially correlated, so these p-values are optimistic.",
          f3(cmp$mean_difference_pp), f3(cmp$ci95_low), f3(cmp$ci95_high), f3(cmp$welch_p), f2(cmp$variance_ratio), f3(cmp$f_test_p)),
  "",
  "## Relation to the original report",
  "",
  "The original report (`archive/comparative_gdp_final_report.pdf`, kept unchanged) describes the same transformation and data source. Applying them gives different headline numbers:",
  "",
  md_table(orig_md),
  "",
  "**What can and can't be established.** Average quarter-over-quarter growth of US and Canadian real GDP is well under 1% per quarter in any published vintage, so a mean of 4.48% per quarter (about 19% a year) can't come from the transformation the report describes. The most likely explanations are a differently defined input, such as already-transformed or annualized growth rates, nominal values, or a series on a different basis, or a processing error. The original input file wasn't kept, so the cause **can't be confirmed**. Germany's original mean (0.76% per quarter, about 3% a year) is also well above what Germany's real GDP grew over this window, which points to the same kind of input problem. Data revisions shift long-run averages by far less than these gaps.",
  "",
  "The report's methods and its qualitative framing carry over. Its quantitative conclusions do not: there's no evidence here of a large or persistent growth gap, and the selected models are simpler.",
  "",
  "## Limitations",
  "",
  "- The US/Canada average is unweighted, and Germany isn't Europe. A GDP-weighted North American series and a euro-area aggregate (from Eurostat) would answer the original regional question more directly.",
  "- 2008–09 and 2020 are structural breaks. A run that excludes or models the pandemic quarters would likely change the GARCH and interval results most.",
  "- Normal prediction intervals don't fit the fat-tailed errors. Bootstrap or Student-t intervals are worth comparing.",
  "- The evaluation uses revised data, not real-time vintages (see above).",
  ""
)
writeLines(lines, file.path("report", "comparative_gdp_report.md"))
cat("Wrote report/comparative_gdp_report.md and report/figures/\n")
