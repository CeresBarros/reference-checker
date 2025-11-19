## --------------------------------------------------------------------------------------------
## Functions and flow built with the help of MS365 Copilot, powered by ChatGPT 5.0
## --------------------------------------------------------------------------------------------

# Purpose: Accept full citation strings; resolve to DOI via Crossref; verify existence in Web of Science (Starter API).

suppressPackageStartupMessages({
  Require::Require(
    c("httr",
      "jsonlite",
      "readr",
      "stringr",
      "dplyr",
      "rcrossref",   # Crossref API client  [5](https://docs.ropensci.org/rcrossref/reference/cr_works.html",
      "stringdist")  # for fuzzy title matching
  )
})

# ---- Config ----
WOS_API_KEY <- Sys.getenv("WOS_API_KEY")
if (WOS_API_KEY == "") stop("WOS_API_KEY environment variable is not set.")

BASE_URL_WOS <- "https://api.clarivate.com/apis/wos-starter/v1/documents"  # Starter API /documents  [3](https://developer.clarivate.com/apis/wos-starter/swagger)

# Optional: identify your email to Crossref per etiquette (improves reliability)  [4](https://github.com/CrossRef/rest-api-doc)
CROSSREF_MAILTO <- Sys.getenv("CROSSREF_MAILTO", unset = NA_character_)
if (!is.na(CROSSREF_MAILTO) && CROSSREF_MAILTO != "") {
  # Set polite pool contact
  cr_ua(paste0("wos-existence-check/1.0 (mailto:", CROSSREF_MAILTO, ")"))
}

source("R/functions.R")

# ---- If you want to run from the command line ----
# Example:
#   Rscript verify_wos_existence.R refs.txt results.csv
if (isFALSE(interactive())) {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) >= 1) {
  in_file  <- args[[1]]
  out_file <- if (length(args) >= 2) args[[2]] else sub("\\.txt", "_results.csv", basename(in_file))
  message(sprintf("Reading: %s", in_file))
  out <- verify_wos_existence(in_file, out_file)
  message(sprintf("Wrote: %s  (%d rows)", out_file, nrow(out)))
  } else {
    stop("Provide input and (optional) output file names.\n",
         "E.g.: 'Rscript verify_wos_existence.R refs.txt' OR 'Rscript verify_wos_existence.R refs.txt results.csv'")
  }
} else {
  in_file  <- "AnaJAErefs.txt"
  out_file <- sub("\\.txt", "_results.csv", basename(in_file))
  message(sprintf("Reading: %s", in_file))
  out <- verify_wos_existence(in_file, out_file)
}
