apply_on_value_between_tags <- function(
    dir = c("", "qmd"), #nolint
    pattern = "\\.qmd$",
    ignore = "^_",
    begin_tag = "%:::% .common h1 begin %:::%",
    end_tag = "%:::% .common h1 end %:::%",
    fun = stringr::str_to_upper,
    wd = here::here()
  ) {
  checkmate::assert_string(wd)
  checkmate::assert_directory_exists(wd)
  checkmate::assert_character(dir)
  for (i in dir) checkmate::assert_directory_exists(file.path(wd, i))
  checkmate::assert_string(pattern)
  checkmate::assert_string(ignore, null.ok = TRUE)
  checkmate::assert_string(begin_tag)
  checkmate::assert_string(end_tag)
  checkmate::assert_function(fun)

  list_quarto_files(
    dir = dir,
    pattern = pattern,
    ignore = ignore,
    wd = wd
  ) |>
    lapply(function(x) {
      swap_value_between_tags(
        x = readLines(here::here(x)),
        value = fun,
        begin_tag = begin_tag,
        end_tag = end_tag
      ) |>
        writeLines(x)

      invisible()
    })

  invisible()
}
