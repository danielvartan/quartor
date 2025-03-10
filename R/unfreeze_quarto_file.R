unfreeze_quarto_file <- function(
    file, #nolint
    unfreeze_tag = "<!-- %:::% unfreeze-tag %:::% -->"
  ) {
  checkmate::assert_file_exists(file, "rw")
  checkmate::assert_string(unfreeze_tag)

  data <- readLines(file)

  if (data[1] == unfreeze_tag) {
    data <- data[-1]
  } else {
    data <- append(data, unfreeze_tag, 0)
  }

  data |> writeLines(file)

  invisible()
}
