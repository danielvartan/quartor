spell_check_quarto <- function(
    dir = c("", "qmd", "tex"), #nolint
    pattern = c("\\.qmd$|\\.Rmd$|\\.tex$"),
    ignore = NULL,
    wordlist = "WORDLIST",
    wd = here::here()
  ) {
  checkmate::assert_string(wd)
  checkmate::assert_directory_exists(wd)
  checkmate::assert_character(dir)
  for (i in dir) checkmate::assert_directory_exists(file.path(wd, i))
  checkmate::assert_string(pattern)
  checkmate::assert_string(ignore, null.ok = TRUE)
  checkmate::assert_string(wordlist, null.ok = TRUE)

  # R CMD Check variable bindings fix (see <https://bit.ly/3z24hbU>)
  # nolint start: object_usage_linter.
  word <- NULL
  # nolint end

  out <- gather_words_from_spell_check(
    dir = dir,
    pattern = pattern,
    ignore = ignore,
    wd = wd
  )

  if (!is.null(wordlist)) {
    if (checkmate::test_file_exists(file.path(wd, wordlist), "r")) {
      wordlist_char <- readLines(wordlist)

      out <- out |> dplyr::filter(!word %in% wordlist_char)
    } else {
      cli::cli_alert_warning(
        paste0(
          "Wordlist file not found. ",
          "You can create one with ",
          "{.strong `set_quarto_speel_check()`}. ",
          "Use `wordlist = NULL` to suppress this message."
        )
      )
    }
  }

  if (length(out$word) == 0) {
    cli::cli_alert_info(
      paste0(
        "No spelling errors were found. Good job! \U0001F389"
      )
    )
  } else {
    out
  }
}
