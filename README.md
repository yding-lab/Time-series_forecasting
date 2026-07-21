# Time Series Forecasting Portfolio

A collection of R projects demonstrating classical and modern time-series forecasting techniques applied to financial markets, trade data, inflation, monetary policy, and GDP growth.

---

## Projects

### 01 — Financial Market Forecasting
**File:** `01_financial_market_forecasting_github.R`  
**Data:** `data/goy.xls` (monthly gold, oil, and Japanese yen prices)

Analyzes monthly price series for gold, oil, and the Japanese yen from 2005 onward. Applies additive decomposition, ACF and cross-correlation analysis, and simple Holt-Winters exponential smoothing. Evaluates out-of-sample forecast accuracy using MAPE over a 6-month holdout period.

**Methods:** Time-series decomposition, ACF/CCF, Holt-Winters, MAPE evaluation, train-test split  
**Packages:** `readxl`, `forecast`

---

### 02 — Trade Ratio Forecasting
**File:** `02_ratio_forecasting_github.R`  
**Data:** `data/trade.xls`

Forecasts China's monthly export-to-import ratio using autoregressive modeling, trend and seasonal regression, and generalized least squares with autocorrelated errors. Compares AR and GLS approaches on out-of-sample accuracy.

**Key findings:**
- The trade ratio exhibits short-run positive autocorrelation
- A positive time trend is statistically significant
- Seasonal effects are present after controlling for trend
- The AR model outperformed GLS on out-of-sample MAPE (~6.47% vs ~7.34%)

**Methods:** AR modeling, trend-seasonality regression, GLS, MAPE comparison  
**Packages:** `readxl`, `nlme`, `forecast`

---

### 03 — U.S. Inflation and CPI Forecasting
**File:** `03_inflation_forecasting_github.R`  
**Data:** `data/Inflation.xls` (U.S. CPI, monthly)

Forecasts the U.S. Consumer Price Index and monthly inflation rate using trend-seasonality regression, ARMA residual modeling, and SARIMA. Evaluates short-horizon forecast accuracy using MAPE.

**Key findings:**
- CPI levels are nonstationary and trend upward over time
- Trend and seasonal regression alone leaves autocorrelation in residuals
- ARMA(1,0,2) captures remaining CPI residual dependence
- Monthly inflation is modeled more appropriately through SARIMA

**Methods:** Trend-seasonality regression, ARMA residual modeling, SARIMA, MAPE  
**Packages:** `readxl`, `forecast`

---

### 04 — Interest Rate and GDP: VAR Analysis
**File:** `04_interest_rate_gdp_var_analysis_github.R`  
**Data:** `data/ffrategdp.xls` (U.S. GDP and Federal Funds Rate, quarterly)

Models the dynamic relationship between U.S. real GDP growth and changes in the Federal Funds Rate using regression and Vector Autoregression (VAR). Includes stationarity checks, lag selection, impulse-response analysis, and a four-quarter forecast.

**Key findings:**
- Modeling GDP and the federal funds rate in levels produces misleading results
- First-differenced policy rate and GDP growth are appropriate for VAR
- The GDP-growth equation showed stronger residual fit than the rate-change equation
- The original forecast indicated moderate GDP growth with no rate increase over the horizon

**Methods:** ADF stationarity testing, VAR, impulse-response functions, lag selection (AIC/BIC)  
**Packages:** `readxl`, `vars`, `tseries`

---

### 05 — Inflation Volatility: SARIMA + GARCH
**File:** `05_garch_volatility_modeling_github.R`  
**Data:** `data/Inflation.xls` (U.S. CPI, monthly)

Extends the inflation analysis by modeling residual volatility. A SARIMA model captures the conditional mean of monthly inflation; a GARCH(1,1) model estimates time-varying conditional variance and produces a two-period volatility forecast.

**Key findings:**
- SARIMA addresses serial dependence in the inflation mean process
- ARCH-LM test assesses remaining volatility clustering in squared residuals
- GARCH(1,1) captures persistent conditional variance
- Reported two-period volatility forecasts: Period 1 ≈ 0.1298, Period 2 ≈ 0.1358

**Methods:** SARIMA, ARCH-LM test, GARCH(1,1), conditional volatility forecasting  
**Packages:** `readxl`, `forecast`, `fGarch`, `FinTS`

---

### 06 — Comparative GDP Forecasting: North America vs Europe
**File:** `06_comparative_gdp_forecasting_github.R`  
**Supplementary report:** `07_comparative_gdp_final_report_github.pdf`  
**Data:** `data/comparative_gdp_oecd.csv` (quarterly GDP: USA, Canada, Europe; OECD source)

Compares quarterly GDP growth dynamics between North America (US + Canada average) and Europe (Q1 1995 – Q4 2023, 115 observations). Applies STL decomposition, Holt-Winters smoothing, ARIMA forecasting via Box-Jenkins methodology, and GARCH volatility modeling. Evaluates forecast accuracy using RMSE and MAPE on an 8-quarter holdout.

**Key findings:**
- North America: mean growth 4.48%, std dev 0.67%; Europe: mean growth 0.76%, std dev 0.47%
- Both series selected ARIMA(1,0,1) — similar temporal dependency structure despite different growth levels
- Ljung-Box p-values: North America 0.264, Europe 0.441 — residuals consistent with white noise
- North America stabilizes at ~4.63% by mid-2027; Europe stabilizes at ~0.73%
- Persistent growth differential of ~3.9 percentage points maintained throughout forecast horizon
- GARCH volatility: North America conditional std dev peaks ~0.6–0.7; Europe ~0.4–0.5

**Methods:** STL decomposition, Holt-Winters, auto.arima (Box-Jenkins), ADF stationarity test, ARCH-LM, GARCH, RMSE/MAPE  
**Packages:** `readxl`, `forecast`, `rugarch`, `tseries`

---

## Techniques Demonstrated

| Category | Methods |
|----------|---------|
| Decomposition | Additive decomposition, STL, Holt-Winters |
| ARIMA family | AR, ARMA, ARIMA, SARIMA, auto.arima |
| Multivariate | Vector Autoregression (VAR), impulse-response analysis |
| Volatility | ARCH-LM test, GARCH(1,1) |
| Stationarity | Augmented Dickey-Fuller (ADF), first differencing |
| Regression | Trend-seasonality OLS, GLS with autocorrelated errors |
| Diagnostics | ACF/PACF, CCF, Ljung-Box test, residual analysis |
| Evaluation | MAPE, RMSE, train-test split, out-of-sample forecasting |

---

## Tech Stack

**Language:** R

**Packages:** `forecast`, `vars`, `rugarch`, `fGarch`, `FinTS`, `tseries`, `nlme`, `readxl`

---

## Repository Structure

```
Time-series_forecasting/
│
├── README.md
├── 01_financial_market_forecasting_github.R
├── 02_ratio_forecasting_github.R
├── 03_inflation_forecasting_github.R
├── 04_interest_rate_gdp_var_analysis_github.R
├── 05_garch_volatility_modeling_github.R
├── 06_comparative_gdp_forecasting_github.R
├── 07_comparative_gdp_final_report_github.pdf
└── data/
    ├── goy.xls
    ├── trade.xls
    ├── Inflation.xls
    ├── ffrategdp.xls
    └── comparative_gdp_oecd.csv
```

---

## Data

The datasets used in these projects were originally provided for academic analysis and are not included in this repository.

Each script uses a relative `data/` path and will produce a clear error message if the required file is missing. To run a script, place the corresponding dataset in the `data/` folder beside the script.

| Script | Required file | Source |
|--------|--------------|--------|
| 01 | `data/goy.xls` | Monthly gold, oil, yen prices |
| 02 | `data/trade.xls` | China monthly trade data |
| 03, 05 | `data/Inflation.xls` | U.S. CPI (monthly) |
| 04 | `data/ffrategdp.xls` | U.S. GDP and Federal Funds Rate |
| 06 | `data/comparative_gdp_oecd.csv` | OECD quarterly GDP: USA, Canada, Europe |

---

## How to Run

1. Clone or download this repository
2. Place the required dataset(s) in the `data/` folder
3. Open the script in RStudio or run via `Rscript`
4. Required packages are automatically installed if missing




