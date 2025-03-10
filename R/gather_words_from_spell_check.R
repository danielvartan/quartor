gather_words_from_spell_check <- function(
    dir = c("", "qmd", "tex"), #nolint
    pattern = "\\.qmd$|\\.Rmd$|\\.tex$",
    ignore = NULL,
    wd = here::here()
  ) {
  checkmate::assert_string(wd)
  checkmate::assert_directory_exists(wd)
  checkmate::assert_character(dir)
  for (i in dir) checkmate::assert_directory_exists(file.path(wd, i))
  checkmate::assert_string(pattern)
  checkmate::assert_string(ignore, null.ok = TRUE)

  # R CMD Check variable bindings fix (see <https://bit.ly/3z24hbU>)
  # nolint start: object_usage_linter.
  word <- NULL
  # nolint end

  files <- list_quarto_files(
    dir = dir,
    pattern = pattern,
    ignore = ignore,
    wd = wd
  )

  bbt_citations <-
    bbt_scan_citation_keys(
      dir = dir,
      pattern = pattern,
      ignore = NULL,
      wd = wd
    )

  files |>
    spelling::spell_check_files() |>
    dplyr::filter(!word %in% bbt_citations, !word == "")
}
