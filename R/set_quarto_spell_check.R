set_quarto_spell_check <- function(wd = here::here()) {
  checkmate::assert_string(wd)
  checkmate::assert_directory_exists(wd)

  if (checkmate::test_file_exists("WORDLIST", "r")) {
    cli::cli_alert_info(
      paste0(
        "No need for setting. ",
        "You already have a {.strong {cli::col_blue('WORDLIST')}} ",
        "file on your project."
      )
    )
  } else {
    rutils:::create_file(file.path(wd, "WORDLIST"))

    cli::cli_alert_success(
      paste0(
        "All set! The wordlist file was created on ",
        "{.strong {cli::col_blue(file.path(wd, 'WORDLIST'))}}. ",
        "Use {.strong `spell_check_quarto()`} and ",
        "{.strong `update_quarto_wordlist()`} ",
        "to check the spelling of your Quarto documents."
      )
    )
  }

  invisible()
}
