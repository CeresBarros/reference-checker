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

# ---- Utilities ----
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
polite_pause <- function() Sys.sleep(0.25)

extract_doi <- function(x) {
  m <- str_match(x, "(10\\.\\d{4,9}/[-._;()/:a-zA-Z0-9]+)")[,2]
  ifelse(is.na(m), NA_character_, m)
}

# Try to guess a year (1800-2100) to aid TI+PY fallback
guess_year <- function(x) {
  m <- str_match(x, "(18|19|20|21)\\d{2}")[,1]
  ifelse(is.na(m), NA_integer_, as.integer(m))
}

guess_title <- function(x) {
  t1 <- str_match(x, "\"([^\"]+)\"")[,2]
  t2 <- str_match(x, "“([^”]+)”")[,2]
  title <- coalesce(t1, t2)

  if (is.na(title)) {
    # Remove everything up to and including the year (e.g., "(2021).")
    candidate <- str_replace(x, ".*\\(\\d{4}\\)\\.?\\s*", "")

    # Split remaining string by period or comma
    fragments <- str_split(candidate, "\\.\\s|,\\s")[[1]]

    # Pick the longest fragment (likely the article title)
    title <- fragments[which.max(nchar(fragments))] %||% candidate
  }
  title <- str_squish(title)
  return(title)
}

normalize_title <- function(x) {
  x |>
    tolower() |>
    str_replace_all("[^a-z0-9 ]", " ") |>
    str_squish()
}

# ---- Crossref resolution from a full citation string ----
# Uses Crossref 'query.bibliographic' to return likely works, then picks best by title similarity and year proximity.  [1](https://community.crossref.org/t/rest-api-works-query-bibliographic/3203)[5](https://docs.ropensci.org/rcrossref/reference/cr_works.html)
resolve_via_crossref <- function(citation, max_candidates = 5, title_sim_threshold = 0.80, year_tol = 2) {
  # Call Crossref: query.bibliographic
  # In rcrossref, pass field queries via 'flq' (fielded query).  [5](https://docs.ropensci.org/rcrossref/reference/cr_works.html)
  res <- tryCatch({
    cr_works(flq = c(query.bibliographic = citation), limit = max_candidates)
  }, error = function(e) NULL)

  if (is.null(res) || is.null(res$data) || nrow(res$data) == 0) {
    return(list(doi = NA_character_, title = NA_character_, year = NA_integer_, debug = "no_crossref_match"))
  }

  # Prepare input title proxy: try content between quotes, else longest capitalized segment, else normalized whole string.
  in_title <- guess_title(citation)
  in_year <- guess_year(citation)

  # Score candidates by normalized title similarity (Jaro-Winkler) and optional year proximity
  cand_df <- res$data %>%
    transmute(
      doi   = doi %||% NA_character_,
      title = if(length(title) > 0) title else NA_character_,
      year  = as.integer(format(as.Date(res$data$issued),"%Y"))
    ) %>%
    mutate(
      title_norm = normalize_title(title %||% ""),
      jw = 1 - stringdist(in_title, title_norm, method = "jw"),
      year_ok = ifelse(is.na(in_year) | is.na(year), TRUE, abs(in_year - year) <= year_tol),
      score = jw + ifelse(year_ok, 0.05, 0.00)  # small bonus if year roughly matches
    ) %>%
    arrange(desc(score))

  best <- cand_df[1, , drop = FALSE]
  if (nrow(best) == 0 || is.na(best$doi) || best$jw < title_sim_threshold) {
    return(list(doi = NA_character_, title = NA_character_, year = NA_integer_, debug = "low_confidence"))
  }

  list(
    doi = best$doi,
    title = best$title,
    year = best$year,
    debug = paste0("crossref_jw=", round(best$jw, 3))
  )
}

# ---- Web of Science query wrappers (Starter API) ----
# Documentation for Starter API parameters (db, q, limit, detail) and field tags (DO, TI, PY).  [3](https://developer.clarivate.com/apis/wos-starter/swagger)
query_wos_starter <- function(q, db = "WOS", limit = 1, detail = "short") {
  browser()
  resp <- GET(
    url   = BASE_URL_WOS,
    query = list(db = db, q = q, limit = limit, detail = detail),
    add_headers(`X-ApiKey` = WOS_API_KEY)
  )
  if (status_code(resp) == 401) stop("Unauthorized (401). Check WOS_API_KEY and plan.")
  if (status_code(resp) == 429) { warning("HTTP 429 Too Many Requests."); return(NULL) }
  if (status_code(resp) >= 500) { warning("Server error."); return(NULL) }
  if (status_code(resp) != 200) { warning(paste("HTTP", status_code(resp))); return(NULL) }

  j <- suppressWarnings(fromJSON(content(resp, as = "text", encoding = "UTF-8"), simplifyVector = TRUE))
  total <- tryCatch(j$metadata$total, error = function(e) 0)
  if (is.null(total) || is.na(total) || total < 1) return(list(total = 0, hit = NULL))


  hit <- if (!is.null(j$hits) && length(j$hits) >= 1 && is.list(j$hits[[1]])) {
    j$hits[[1]]
  } else {
    return(list(total = total, hit = NULL))
  }

  list(
    total = total,
    hit   = list(
      uid    = hit$uid %||% NA_character_,
      title  = hit$title %||% NA_character_,
      doi    = hit$identifiers$doi %||% NA_character_,
      year   = hit$source$publishYear %||% NA_integer_,
      source = hit$source$sourceTitle %||% NA_character_,
      times_cited = tryCatch(hit$citations[[1]]$count, error = function(e) NA_integer_),
      wos_url = hit$links$record %||% NA_character_
    )
  )
  polite_pause()
}

# Build the best-available Web of Science query from a full citation string.
build_best_wos_query <- function(full_citation) {

  doi <- extract_doi(full_citation)
  if (!is.na(doi)) {
    return(list(mode = "DOI", q = sprintf('DO="%s"', doi), key = doi, via = "inline"))
  }

  # Resolve via Crossref bibliographic search  [1](https://community.crossref.org/t/rest-api-works-query-bibliographic/3203)
  cx <- resolve_via_crossref(full_citation)
  if (!is.na(cx$doi)) {
    return(list(mode = "DOI", q = sprintf('DO="%s"', cx$doi), key = cx$doi, via = cx$debug))
  }

  # Fallback: title (+year) if we can approximate them; both TI and PY are supported in Starter API  [3](https://developer.clarivate.com/apis/wos-starter/swagger)
  # Extract quoted title if present, else use heuristic slice
  title <- guess_title(full_citation)

  year <- guess_year(full_citation)

  q <- sprintf('TI=("%s")', gsub('"', '\\"', title))
  if (!is.na(year)) q <- paste0(q, " AND PY=", year)

  list(mode = "TITLE", q = q, key = title, via = "fallback_TI_PY")
}

verify_wos_existence <- function(txt_file, out_csv = "wos_verification_results.csv") {
  refs <- read_lines(txt_file)
  refs <- refs[nchar(trimws(refs)) > 0]

  results <- lapply(refs, function(ref) {
    spec <- build_best_wos_query(ref)
    res  <- query_wos_starter(spec$q, db = "WOS", limit = 1, detail = "short")  # db=WOS = Core Collection  [3](https://developer.clarivate.com/apis/wos-starter/swagger)

    if (is.null(res)) {
      tibble(
        input_reference = ref,
        query_mode = spec$mode,
        query_via = spec$via,
        query_used = spec$q,
        exists_in_wos = NA,
        uid = NA, title = NA, matched_doi = NA, year = NA,
        source_title = NA, times_cited = NA, wos_record_url = NA
      )
    } else if (res$total < 1 || is.null(res$hit)) {
      tibble(
        input_reference = ref,
        query_mode = spec$mode,
        query_via = spec$via,
        query_used = spec$q,
        exists_in_wos = FALSE,
        uid = NA, title = NA, matched_doi = NA, year = NA,
        source_title = NA, times_cited = NA, wos_record_url = NA
      )
    } else {
      h <- res$hit
      tibble(
        input_reference = ref,
        query_mode = spec$mode,
        query_via = spec$via,
        query_used = spec$q,
        exists_in_wos = TRUE,
        uid = h$uid,
        title = h$title,
        matched_doi = h$doi,
        year = h$year,
        source_title = h$source,
        times_cited = h$times_cited,
        wos_record_url = h$wos_url
      )
    }
  })

  df <- bind_rows(results)
  write_csv(df, out_csv)
  df
}


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
