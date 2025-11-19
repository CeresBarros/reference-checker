
#' Null-coalescing operator
#'
#' Returns `a` unless it is `NULL` or has length zero, in which case returns `b`.
#'
#' @param a First object to test.
#' @param b Fallback value if `a` is `NULL` or empty.
#' @return `a` if non-null and non-empty, otherwise `b`.
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

#' Polite pause between API calls
#'
#' Sleeps for a short interval to respect API rate limits.
#'
#' @return Invisibly returns `NULL`.
polite_pause <- function() Sys.sleep(0.25)

#' Extract DOI from a citation string
#'
#' Searches for a DOI pattern in the input string, handling lowercase and URL prefixes.
#'
#' @param x Character string containing a citation.
#' @return A DOI string if found, otherwise `NA_character_`.
#' @importFrom stringr str_match str_replace
extract_doi <- function(x) {
  m <- str_match(x, "(10\\.\\d{4,9}/[-._;()/:a-zA-Z0-9]+)")[,2]
  ifelse(is.na(m), NA_character_, m)
}

#' Guess publication year from a citation string
#'
#' Extracts a four-digit year between 1800 and 2100.
#'
#' @param x Character string containing a citation.
#' @return Integer year if found, otherwise `NA_integer_`.
#' @importFrom stringr str_match
guess_year <- function(x) {
  m <- str_match(x, "(18|19|20|21)\\d{2}")[,1]
  ifelse(is.na(m), NA_integer_, as.integer(m))
}

#' Guess article title from a citation string
#'
#' Attempts to extract the title using quotes or heuristics:
#' removes authors/year, splits by punctuation, and selects the longest fragment.
#'
#' @param x Character string containing a citation.
#' @return A cleaned title string.
#' @importFrom stringr str_match str_replace str_split str_squish
#' @importFrom dplyr coalesce
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

#' Guess article title from a citation string
#'
#' Attempts to extract the title using quotes or heuristics:
#' removes authors/year, splits by punctuation, and selects the longest fragment.
#'
#' @param x Character string containing a citation.
#' @return A cleaned title string.
#' @importFrom stringr str_replace_all str_squish
normalize_title <- function(x) {
  x |>
    tolower() |>
    str_replace_all("[^a-z0-9 ]", " ") |>
    str_squish()
}

#' Resolve DOI via Crossref bibliographic query
#'
#' Uses Crossref REST API to find candidate works for a full citation string.
#' Scores candidates by Jaro-Winkler similarity and year proximity.
#'
#' @param citation Full citation string.
#' @param max_candidates Maximum number of candidates to retrieve.
#' @param title_sim_threshold Minimum Jaro-Winkler similarity to accept.
#' @param year_tol Year tolerance for matching.
#' @return List with `doi`, `title`, `year`, and `debug` info.
#' @importFrom rcrossref cr_works
#' @importFrom dplyr transmute mutate arrange
#' @importFrom stringdist stringdist
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

#' Query Web of Science Starter API
#'
#' Sends a GET request to the Starter API `/documents` endpoint and retrieves metadata for the first hit.
#'
#' @param q Query string using supported field tags (e.g., DO, TI, PY).
#' @param db Database code (default "WOS" for Core Collection).
#' @param limit Maximum number of hits to return.
#' @param detail Level of detail ("short" or "full").
#' @return List with `total` and `hit` (or `NULL` if no match).
#' @importFrom httr GET add_headers content status_code
#' @importFrom jsonlite fromJSON
query_wos_starter <- function(q, db = "WOS", limit = 1, detail = "short") {
  resp <- GET(
    url   = BASE_URL_WOS,
    query = list(db = db, q = q, limit = limit, detail = detail),
    add_headers(`X-ApiKey` = WOS_API_KEY)
  )
  polite_pause()
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
}

#' Build best Web of Science query from a citation
#'
#' Chooses query strategy: DOI if present, else Crossref DOI resolution, else title/year fallback.
#'
#' @param full_citation Full citation string.
#' @return List with `mode`, `q`, `key`, and `via` (strategy used).
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

#' Verify existence of references in Web of Science
#'
#' Reads a text file of citations, builds queries, and checks existence via Starter API.
#'
#' @param txt_file Path to text file with one citation per line.
#' @param out_csv Path to output CSV file.
#' @return Tibble with verification results.
#' @importFrom readr read_lines write_csv
#' @importFrom dplyr bind_rows
#' @importFrom tibble tibble
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
