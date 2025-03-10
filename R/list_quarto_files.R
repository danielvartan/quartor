list_quarto_files <- function(
    dir = c("", "qmd"), #nolint
    pattern = "\\.qmd$",
    ignore = NULL,
    wd = here::here()
  ) {
  checkmate::assert_string(wd)
  checkmate::assert_directory_exists(wd)
  checkmate::assert_character(dir)
  for (i in dir) checkmate::assert_directory_exists(file.path(wd, i))
  checkmate::assert_string(pattern)
  checkmate::assert_string(ignore, null.ok = TRUE)

  out <- dir |>
    lapply(function(x) {
      setdiff(
        list.files(file.path(wd, x), full.names = TRUE),
        list.dirs(file.path(wd, x), recursive = FALSE, full.names = TRUE)
      ) |>
        stringr::str_subset(pattern)
    }) |>
    unlist()

  if (!is.null(ignore)) {
    out |> stringr::str_subset(ignore, negate = TRUE)
  } else {
    out
  }
}
