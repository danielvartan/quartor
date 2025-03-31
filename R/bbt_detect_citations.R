# # Helper
#
# file <- tcltk::tk_choose.files()
#
# file |> (bbt_detect_citations)

# Adapted from:
# https://github.com/paleolimbot/rbbt/blob/master/R/detect-citations.R
bbt_detect_citations <- function(
  path = rbbt::bbt_guess_citation_context(),
  locale = readr::default_locale()
) {
  checkmate::assert_file_exists(path, "r")
  checkmate::assert_class(locale, "locale")

  path |>
    vapply(
      readr::read_file,
      locale = locale,
      FUN.VALUE = character(1)
    ) |>
    bbt_detect_citations_chr()
}

# Adapted from:
# https://github.com/paleolimbot/rbbt/blob/master/R/detect-citations.R
bbt_detect_citations_chr <- function(text) {
  checkmate::assert_character(text)

  # R CMD Check variable bindings fix (see <https://bit.ly/3z24hbU>)
  # nolint start: object_usage_linter.
  . <- NULL
  # nolint end

  pattern <-
    paste0(
      c(
        "(?<=@)[a-zA-Z0-9_\\.\\-:]+[a-zA-Z0-9]",
        "(?<=cite\\{)[a-zA-Z0-9_\\.\\-:]+[a-zA-Z0-9]"
        # "(?<=cite\\{).+(?=\\})".
      ),
      collapse = "|"
    )

  text |>
    paste0(collapse = "\n") |>
    # Don't include text in code chunks
    stringr::str_remove("\n```\\{.+?\\}.+?\r?\n```") |>
    # Don't include text in in-line R code
    stringr::str_remove("\\`r.+?\\`") |>
    # Don't include email addresses
    stringr::str_remove("([a-zA-Z0-9_.+-]+@[a-zA-Z0-9-]+\\.[a-zA-Z0-9-.]+)") |>
    # Don't include inline markdown URLs
    stringr::str_remove("\\(http.+?\\)|<http.+?>") |>
    stringr::str_match_all(
      stringr::regex(pattern, multiline = TRUE, dotall = TRUE)
    ) |>
    magrittr::extract2(1) |>
    magrittr::extract(, 1, drop = TRUE) |>
    unique()
}
