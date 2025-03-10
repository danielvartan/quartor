bbt_scan_citation_keys <- function(
    dir = c("", "qmd", "tex"), #nolint
    pattern = "\\.qmd$|\\.tex$",
    ignore = NULL,
    locale = readr::default_locale(),
    wd = here::here()
  ) {
  checkmate::assert_string(wd)
  checkmate::assert_directory_exists(wd)
  checkmate::assert_character(dir)
  for (i in dir) checkmate::assert_directory_exists(file.path(wd, i))
  checkmate::assert_string(pattern)
  checkmate::assert_string(ignore, null.ok = TRUE)
  checkmate::assert_class(locale, "locale")

  bbt_types <- c(
    "article", "booklet", "conference", "inbook", "incollection",
    "inproceedings", "manual", "mastersthesis", "misc", "phdthesis",
    "proceedings", "techreport", "unpublished"
  )

  quarto_types <- c(
    "cnj-", "cor-", "def-", "eq-", "exm-", "exr-", "fig-", "lem-", "lst-",
    "prp-", "sec-", "tbl-", "thm-"
  )

  out <-
    dir |>
    lapply(function(x) {
      setdiff(
        list.files(file.path(wd, x), full.names = TRUE),
        list.dirs(file.path(wd, x), recursive = FALSE, full.names = TRUE)
      ) |>
        stringr::str_subset(pattern)
    }) |>
    unlist() |>
    bbt_detect_citations(locale = locale) |>
    sort()

  out <-
    out[!out %in% bbt_types] |>
    stringr::str_subset(
      paste0("^", quarto_types, collapse = "|"), negate = TRUE
    )

  if (!is.null(ignore)) {
    out |> stringr::str_subset(ignore, negate = TRUE)
  } else {
    out
  }
}
