clean_quarto_mess <- function(
    file = NULL, #nolint
    dir = c(""),
    ext = c("aux", "cls", "loa", "log"),
    ignore = NULL,
    wd = here::here()
  ) {
  checkmate::assert_string(wd)
  checkmate::assert_directory_exists(wd)
  checkmate::assert_character(file, null.ok = TRUE)
  checkmate::assert_character(dir)
  checkmate::assert_character(ext, null.ok = TRUE)
  checkmate::assert_character(ignore, null.ok = TRUE)

  ext_files <- list.files(
    wd,
    pattern = paste0("\\.", ext, "$", collapse = "|")
  )

  if (!length(ext_files) == 0) file <- file |> append(ext_files)

  if (!is.null(ignore)) {
    file <- file |> stringr::str_subset(ignore, negate = TRUE)
  }

  for (i in file) {
    if (checkmate::test_file_exists(file.path(wd, i))) {
      fs::file_delete(i)
    }
  }

  for (i in dir) {
    if (checkmate::test_directory_exists(file.path(wd, i))) {
      fs::file_delete(i)
    }
  }

  invisible()
}
