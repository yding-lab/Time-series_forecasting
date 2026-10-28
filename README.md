# Time Series Forecasting Portfolio

[![gdp-analysis](https://github.com/yding-lab/Time-series_forecasting/actions/workflows/gdp.yml/badge.svg)](https://github.com/yding-lab/Time-series_forecasting/actions/workflows/gdp.yml)

Statistical modeling and forecasting in R. The lead project is a reproducible comparison of GDP growth in North America and Europe: the data is downloaded from the OECD, models are evaluated chronologically against naive baselines, and forecasts come with prediction intervals. Five earlier analyses (markets, trade, inflation, VAR, GARCH) are kept as supporting work.

## Lead project: comparative GDP growth forecasting, North America vs Europe

**Question.** How do quarterly growth dynamics differ between North America (the average of the US and Canada) and Europe (Germany, the proxy used in the original report)? And how well can each be forecast?

| | |
|---|---|
| **Target** | Quarter-over-quarter growth of seasonally adjusted real GDP, `100 × [log(GDP_t) − log(GDP_{t−1})]` |
| **Units** | Percent per quarter (not annualized). Forecast errors are in percentage points of quarterly growth |
| **Data** | OECD Quarterly National Accounts: real GDP, chain-linked volume, national currency, calendar and seasonally adjusted. USA, CAN, DEU; 1995-Q1 to 2023-Q4 (116 quarters, 115 growth observations) |
| **Methods** | STL decomposition, Holt-Winters, ADF, `auto.arima` (Box-Jenkins), Ljung-Box, ARCH-LM, GARCH(1,1), Welch t-test / F-test |
| **Validation** | (1) a chronological 8-quarter holdout, and (2) **rolling-origin evaluation**: 55 expanding-window origins, horizons 1–8, against naive, seasonal-naive and mean baselines, with prediction-interval coverage |

### Reproduce from a fresh clone

Requires R 4.5 (tested on 4.5.3) and internet access to `sdmx.oecd.org`.

```bash
Rscript install_packages.R               # pinned: CRAN snapshot 2026-10-01 (forecast 9.0.2, rugarch 1.5-6, tseries 0.10-63)
Rscript gdp/fetch_oecd_gdp.R             # -> data/comparative_gdp_oecd.csv + .SOURCE.txt (query, retrieval time)
Rscript gdp/comparative_gdp_analysis.R   # -> results/gdp_results.md, CSV tables, results/figures/
Rscript gdp/rolling_origin_evaluation.R  # -> results/rolling_origin.md, results/rolling_origin_metrics.csv
```

Run from the repository root. The scripts never install packages themselves: a missing package stops the run with the install command. CI (`.github/workflows/gdp.yml`) runs exactly these four commands on every push and uploads `results/` as an artifact.

**Data and license.** The data isn't committed. `fetch_oecd_gdp.R` downloads it from the OECD SDMX API (`DSD_NAMAIN1@DF_QNA` v1.1, key `Q.Y.USA+CAN+DEU.S1.S1.B1GQ._Z._Z._Z.XDC.L.N.T0102`) and records the query and retrieval time next to it. Reuse is governed by the [OECD terms and conditions](https://www.oecd.org/en/about/terms-conditions.html). The OECD revises national accounts, so a later download can change the numbers below slightly.

### Results (re-run in CI on 2026-10-28, OECD data retrieved the same day)

**Descriptive statistics**, % per quarter:

| | Mean | SD | Min | Max |
|---|---|---|---|---|
| North America | 0.591 | 1.333 | −9.929 | 8.085 |
| Europe (Germany) | 0.312 | 1.453 | −9.366 | 8.424 |

The 2020 pandemic quarters (about −10% and +8%) dominate the extremes and much of the variance.

**Out-of-sample forecast accuracy: rolling origin.** 412 forecasts per model and region, all horizons pooled:

| Region | Model | RMSE | MAE | 80% PI coverage | 95% PI coverage |
|---|---|---|---|---|---|
| North America | Mean | **1.872** | **0.696** | 0.903 | 0.939 |
| North America | Seasonal naive | 2.656 | 1.239 | 0.864 | 0.891 |
| North America | Naive | 2.723 | 1.175 | 0.876 | 0.910 |
| North America | ARIMA | 2.807 | 0.995 | 0.867 | 0.908 |
| Europe | Mean | **1.879** | **0.812** | 0.917 | 0.932 |
| Europe | ARIMA | 1.956 | 0.842 | 0.910 | 0.927 |
| Europe | Naive | 2.733 | 1.356 | 0.910 | 0.939 |
| Europe | Seasonal naive | 2.758 | 1.427 | 0.879 | 0.913 |

RMSE relative to the naive forecast (below 1 = better than naive), by horizon:

| | h = 1 | h = 4 | h = 8 |
|---|---|---|---|
| North America, ARIMA | 1.129 | 1.015 | 0.950 |
| North America, Mean | 0.634 | 0.706 | 0.703 |
| Europe, ARIMA | 0.786 | 0.684 | 0.685 |
| Europe, Mean | 0.630 | 0.676 | 0.685 |

What this shows:
- **ARIMA doesn't beat the historical mean.** For North America, one quarter ahead, it's worse than the naive forecast. Quarterly growth has little predictable dynamics beyond its average, and refitting `auto.arima` after the 2020 shock produces unstable models.
- **Prediction intervals have the wrong shape.** The 80% intervals cover about 87–92% of outcomes (too wide), while the 95% intervals cover about 89–94% (too narrow). Normal intervals fitted to data with pandemic outliers are too wide in calm periods and too narrow in the tails.

**Single 8-quarter holdout (2022-Q1 to 2023-Q4).** Training-set (in-sample) RMSE is 1.36 for North America and 1.47 for Europe. Test (out-of-sample) RMSE:

| | ARIMA | Mean | Naive | Seasonal naive |
|---|---|---|---|---|
| North America | 0.183 | 0.197 | 1.171 | 0.905 |
| Europe | 0.545 | 0.542 | 0.635 | 1.265 |

Test errors are *lower* than the in-sample fit here only because 2022–23 was calm compared with the training period, which includes 2008–09 and 2020. One 8-quarter window is a small sample; the rolling-origin results above are the more reliable measure. Both selected models are MA(1) (`0,0,1`).

**Forecast beyond the sample (2024-Q1 to 2025-Q4)**, final MA(1) models refitted on all data:

| | 2024-Q1 point | 80% PI | 95% PI | Long-run point |
|---|---|---|---|---|
| North America | 0.615 | −1.078 to 2.307 | −1.974 to 3.203 | 0.592 |
| Europe | 0.426 | −1.418 to 2.270 | −2.394 to 3.246 | 0.312 |

**Regional comparison.**
- The difference in mean growth is 0.279 pp (Welch 95% CI −0.083 to 0.641, p = 0.13). The variance ratio is 0.84 (p = 0.36).
- Both tests assume independent observations; serial correlation makes these p-values optimistic.

**Diagnostics.** ADF p ≤ 0.01 for both series. Ljung-Box (lag 8) on ARIMA residuals: p = 0.998 (North America) and 0.843 (Europe). ARCH-LM: p = 0.427 and 0.097. The GARCH(1,1) estimates sit at the boundary of the parameter space (North America α ≈ 1, β ≈ 0; Europe α ≈ 0, β ≈ 1). This is a sign that a few pandemic quarters drive the fit, not that the volatility dynamics are well identified.

### Original report vs this re-run

The original analysis (`gdp/comparative_gdp_final_report.pdf`) is kept unchanged. Several of its headline numbers aren't reproduced when the documented method is applied to the documented data source:

| | Original report | Re-run (OECD, 2026-10-28) |
|---|---|---|
| Mean growth, North America / Europe | 4.48% / 0.76% per quarter | 0.591% / 0.312% |
| SD, North America / Europe | 0.67 / 0.47 | 1.333 / 1.453 |
| Growth differential | ≈ 3.9 pp, "persistent" | 0.28 pp, not significant |
| Selected ARIMA | (1,0,1) for both | (0,0,1) for both |
| Ljung-Box p (NA / EU) | 0.264 / 0.441 | 0.998 / 0.843 |
| ARCH-LM p (NA / EU) | 0.892 / 0.135 | 0.427 / 0.097 |
| End-of-horizon forecast (NA / EU) | ≈ 4.63% / 0.73% | 0.592% / 0.312% |

Average quarter-over-quarter growth of real GDP for the US and Canada is well under 1% per quarter, so a 4.48% mean (about 19% a year) can't come from the transformation the report describes. The original North American input was most likely a differently defined series, such as already-transformed growth rates or values on a different basis. The original data file isn't available, so this can't be confirmed. The report's methods and qualitative framing stand; its quantitative conclusions about the size of the regional gap shouldn't be cited without this caveat.

### Limitations

- Germany stands in for Europe. The euro-area or EU aggregate isn't in this OECD dataflow.
- 2008–09 and 2020 are structural breaks. A robustness run excluding or modeling the pandemic quarters is the obvious next step and would likely change the GARCH and interval results most.
- Normal prediction intervals don't fit the fat-tailed errors well. Bootstrap or Student-t intervals are worth comparing.
- The forecast beyond the sample can now be checked against 2024–25 actuals by extending `END` in the fetch script.

## Supporting analyses

These are earlier coursework analyses. Their data came from course materials, isn't included, and isn't redistributable here, so **these scripts haven't been re-run, and their findings are as originally reported.** Each script stops with a clear message if its file is missing (checked in CI).

| Script | Topic | Methods | Required file |
|---|---|---|---|
| `supporting/01_financial_market_forecasting.R` | Gold, oil and yen prices (monthly, 2005–) | Decomposition, ACF/CCF, Holt-Winters, MAPE on a 6-month holdout | `data/goy.xls` |
| `supporting/02_ratio_forecasting.R` | China export/import ratio | AR, trend + seasonal regression, GLS with AR errors. As reported: AR beat GLS out of sample, MAPE ~6.47% vs ~7.34% | `data/trade.xls` |
| `supporting/03_inflation_forecasting.R` | U.S. CPI and monthly inflation | Trend + seasonal regression, ARMA residuals, SARIMA | `data/Inflation.xls` |
| `supporting/04_interest_rate_gdp_var_analysis.R` | U.S. GDP growth and Fed funds rate | ADF, VAR, lag selection, impulse responses | `data/ffrategdp.xls` |
| `supporting/05_garch_volatility_modeling.R` | U.S. inflation volatility | SARIMA mean, ARCH-LM, GARCH(1,1) | `data/Inflation.xls` |

Run any of them with `Rscript supporting/<script>.R` from the repository root, after placing the file in `data/`.

## Layout

```
gdp/
  fetch_oecd_gdp.R                 download + validate the OECD input, record provenance
  comparative_gdp_analysis.R       the lead analysis (holdout, baselines, intervals, diagnostics, GARCH)
  rolling_origin_evaluation.R      expanding-window cross-validation vs baselines, horizons 1-8
  comparative_gdp_final_report.pdf the original report
supporting/                        01-05 earlier analyses (data not included)
R/packages.R                       package check shared by all scripts (no silent installs)
install_packages.R                 pinned package installation
.github/workflows/gdp.yml          fresh-clone reproduction in CI
```
