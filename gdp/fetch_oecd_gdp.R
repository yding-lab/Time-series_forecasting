# ==============================================================================
# Download the input data for the comparative GDP analysis from the OECD.
#
#   Rscript gdp/fetch_oecd_gdp.R            # run from the repository root
#
# Writes:
#   data/comparative_gdp_oecd.csv           date, united_states, canada, europe
#   data/comparative_gdp_oecd.SOURCE.txt    query URL, series definition, retrieval time
#
# Series (OECD Quarterly National Accounts, dataflow DSD_NAMAIN1@DF_QNA v1.1):
#   real GDP (B1GQ, total economy), chain-linked volume (PRICE_BASE = L), national
#   currency (XDC), calendar and seasonally adjusted (ADJUSTMENT = Y), quarterly levels.
#   "europe" is Germany (DEU), the European proxy the original report used.
#
# The data is not committed to this repository. Reuse is governed by the OECD's terms:
# https://www.oecd.org/en/about/terms-conditions.html
# ==============================================================================

START <- "1995-Q1"
END <- "2023-Q4" # the window analysed in the report
COUNTRIES <- c(united_states = "USA", canada = "CAN", europe = "DEU")

key <- sprintf(
  "Q.Y.%s.S1.S1.B1GQ._Z._Z._Z.XDC.L.N.T0102",
  paste(COUNTRIES, collapse = "+")
)
url <- sprintf(
  "https://sdmx.oecd.org/public/rest/data/OECD.SDD.NAD,DSD_NAMAIN1@DF_QNA,1.1/%s?startPeriod=%s&endPeriod=%s&format=csvfile",
  key, START, END
)

dir.create("data", showWarnings = FALSE)
raw_file <- tempfile(fileext = ".csv")
status <- download.file(
  url, raw_file,
  method = "libcurl", quiet = TRUE,
  headers = c("User-Agent" = "time-series-portfolio (R)")
)
if (status != 0) stop("OECD download failed with status ", status, call. = FALSE)

raw <- read.csv(raw_file, stringsAsFactors = FALSE)
needed <- c("REF_AREA", "TIME_PERIOD", "OBS_VALUE", "ADJUSTMENT", "PRICE_BASE", "UNIT_MEASURE")
if (!all(needed %in% names(raw))) {
  stop("Unexpected OECD response; columns: ", paste(names(raw), collapse = ", "), call. = FALSE)
}
raw <- raw[raw$ADJUSTMENT == "Y" & raw$PRICE_BASE == "L" & raw$UNIT_MEASURE == "XDC", ]

wide <- reshape(
  raw[, c("REF_AREA", "TIME_PERIOD", "OBS_VALUE")],
  idvar = "TIME_PERIOD", timevar = "REF_AREA", direction = "wide"
)
names(wide) <- sub("^OBS_VALUE\\.", "", names(wide))
wide <- wide[order(wide$TIME_PERIOD), ]

out <- data.frame(date = wide$TIME_PERIOD, stringsAsFactors = FALSE)
for (column in names(COUNTRIES)) out[[column]] <- as.numeric(wide[[COUNTRIES[[column]]]])

expected_quarters <- (as.integer(substr(END, 1, 4)) - as.integer(substr(START, 1, 4)) + 1) * 4
if (nrow(out) != expected_quarters || anyNA(out)) {
  stop(sprintf("Expected %d complete quarters, got %d rows (%d missing values).",
               expected_quarters, nrow(out), sum(is.na(out))), call. = FALSE)
}

write.csv(out, file.path("data", "comparative_gdp_oecd.csv"), row.names = FALSE)
writeLines(c(
  "Source: OECD Quarterly National Accounts (DSD_NAMAIN1@DF_QNA, version 1.1)",
  paste("Query:", url),
  "Series: real GDP (B1GQ), chain-linked volume (L), national currency (XDC), calendar and seasonally adjusted (Y)",
  "Columns: united_states = USA, canada = CAN, europe = DEU (Germany, the report's European proxy)",
  paste("Retrieved (UTC):", format(Sys.time(), tz = "UTC", usetz = TRUE)),
  "Terms of use: https://www.oecd.org/en/about/terms-conditions.html",
  "Values are subject to revision by the OECD; re-downloading later may change them."
), file.path("data", "comparative_gdp_oecd.SOURCE.txt"))

cat(sprintf("Wrote data/comparative_gdp_oecd.csv: %d quarters, %s to %s\n",
            nrow(out), out$date[1], out$date[nrow(out)]))
