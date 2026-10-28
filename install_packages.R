# Install every package the analyses use, at versions frozen by a dated snapshot of CRAN
# (Posit Public Package Manager), so a fresh clone gets the same versions as the results
# in this repository.
#
#   Rscript install_packages.R
#
# Tested with R 4.5 on Ubuntu (CI). On Linux, pre-built binaries for the same snapshot are
# used when available, which avoids compiling packages and their system dependencies.
# To move to newer packages, change SNAPSHOT and re-run the analyses.

SNAPSHOT <- "2026-10-01"

repo <- sprintf("https://packagemanager.posit.co/cran/%s", SNAPSHOT)
if (Sys.info()[["sysname"]] == "Linux" && file.exists("/etc/os-release")) {
  os <- readLines("/etc/os-release")
  codename <- sub("^VERSION_CODENAME=", "", grep("^VERSION_CODENAME=", os, value = TRUE))
  if (length(codename) == 1 && nzchar(codename)) {
    repo <- sprintf("https://packagemanager.posit.co/cran/__linux__/%s/%s", codename, SNAPSHOT)
    # Package Manager only serves Linux binaries to clients that identify their R version.
    options(HTTPUserAgent = sprintf(
      "R/%s R (%s)", getRversion(),
      paste(getRversion(), R.version[["platform"]], R.version[["arch"]], R.version[["os"]])
    ))
  }
}
options(repos = c(CRAN = repo))
cat("Installing from", repo, "\n")

packages <- c(
  # lead project: comparative GDP forecasting
  "forecast", "tseries", "FinTS", "rugarch",
  # supporting analyses
  "readxl", "nlme", "vars", "fGarch"
)

missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing) > 0) {
  install.packages(missing, Ncpus = max(1L, parallel::detectCores() - 1L))
}

still_missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(still_missing) > 0) {
  stop("Failed to install: ", paste(still_missing, collapse = ", "), call. = FALSE)
}
cat("Installed versions:\n")
for (p in packages) cat(sprintf("  %-10s %s\n", p, as.character(packageVersion(p))))
