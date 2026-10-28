# Package check shared by every script.
#
# Scripts never install packages themselves: installing silently changes the environment
# and makes results depend on whatever versions CRAN serves that day. Install once, with
# pinned versions, via:  Rscript install_packages.R

require_packages <- function(packages) {
  missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) > 0) {
    stop(
      "Missing R packages: ", paste(missing, collapse = ", "), ".\n",
      "Install them with:  Rscript install_packages.R",
      call. = FALSE
    )
  }
  invisible(lapply(packages, library, character.only = TRUE))
}
