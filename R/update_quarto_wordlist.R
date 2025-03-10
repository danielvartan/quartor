update_quarto_wordlist <- function(
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
  checkmate::assert_file_exists(file.path(wd, wordlist), "r")

  words <-
    gather_words_from_spell_check(
      dir = dir,
      pattern = pattern,
      ignore = ignore,
      wd = wd
    ) |>
    magrittr::extract2("word")

  wordlist_char <- readLines(wordlist)
  words_to_add <- words[is.na(match(words, wordlist_char))]
  words_to_remove <- wordlist_char[is.na(match(wordlist_char, words))]
  words_to_keep <- wordlist_char[!is.na(match(wordlist_char, words))] #nolint

  if (!length(words_to_add) == 0) {
    cli::cli_h1(
      paste0(
        "The following {length(words_to_add)} words will be ",
        "{.strong {cli::col_blue('added')}} to the ",
        "wordlist", "\n"
      )
    )
    cli::cli_ul(sort(words_to_add))

    ifelse(!length(words_to_remove) == 0, "", cli::cli_rule())
  }

  if (!length(words_to_remove) == 0) {
    cli::cli_h1(
      paste0(
        "The following {length(words_to_remove)} words will be ",
        "{.strong {cli::col_red('removed')}} from the ",
        "wordlist", "\n"
      )
    )
    cli::cli_ul(sort(words_to_remove))
    cli::cli_rule()
  }

  if (!length(words_to_add) == 0 || !length(words_to_remove) == 0) {
    decision <- readline("Do you want to proceed? [Y/n] ")

    while (!decision %in% c("Y", "n")) {
      decision <- readline("Do you want to proceed? [Y/n] ")
    }

    if (decision == "n") {
      return(invisible())
    } else {
      writeLines(sort(words), wordlist)
    }
  } else {
    cli::cli_alert_info("No spelling errors were found. Good job! \U0001F389")
  }

  invisible()
}
