check_missing_dollar_signs <- function(file) {
  checkmate::assert_file_exists(file, "r")

  data <- file |> readLines()
  out <- character()

  for (i in seq_along(data)) {
    signs <-
      data[i] |>
      stringr::str_extract_all("\\$") |>
      unlist()

    if (!length(signs) %% 2 == 0) {
      out <- out |> append(i)

      cli::cli_alert_warning(paste0(
        "A missing dollar sign was found on line ",
        "{.strong {cli::col_red(i)}}."
      ))
    }
  }

  out
}

check_missing_dollar_signs_in_dir <- function( #nolint
    dir = c("_extensions", "_extensions/tex"), #nolint
    pattern = "\\.tex$",
    ignore = NULL,
    wd = here::here()
  ) {
  checkmate::assert_string(wd)
  checkmate::assert_directory_exists(wd)
  checkmate::assert_character(dir)
  for (i in dir) checkmate::assert_directory_exists(file.path(wd, i))
  checkmate::assert_string(pattern)
  checkmate::assert_string(ignore, null.ok = TRUE)

  files <- dir |>
    lapply(function(x) {
      setdiff(
        list.files(file.path(wd, x), full.names = TRUE),
        list.dirs(file.path(wd, x), recursive = FALSE, full.names = TRUE)
      ) |>
        stringr::str_subset(pattern)
    }) |>
    unlist()

  if (!is.null(ignore)) {
    files <- files |> stringr::str_subset(ignore, negate = TRUE)
  } else {
    files
  }

  for (i in files) {
    test <- shush(check_missing_dollar_signs(i))

    if (!length(test) == 0) {
      cli::cli_h1(paste0("File ", basename(i)))
      check_missing_dollar_signs(i)
    }
  }

  invisible()
}
