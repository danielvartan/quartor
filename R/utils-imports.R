# Borrowed from `rutils`: github.com/danielvartan/rutils
extract_fit_engine <- function(model) {
  checkmate::assert_multi_class(model, c("lm", "model_fit", "workflow"))

  if (checkmate::test_multi_class(model, c("model_fit", "workflow"))) {
    model |> parsnip::extract_fit_engine()
  } else {
    model
  }
}

# Borrowed from `rutils`: github.com/danielvartan/rutils
inbetween_integers <- function(x, y) {
  checkmate::assert_int(x)
  checkmate::assert_int(y)

  lower_int <- min(x, y) |> floor()
  upper_int <- max(x, y) |> ceiling()

  setdiff(lower_int:upper_int, sort(c(x, y)))
}

# Borrowed from `rutils`: github.com/danielvartan/rutils
get_file_ext <- function(file) {
  checkmate::assert_character(file)

  file |>
    basename() |>
    stringr::str_extract("\\.[[:alnum:]]+$")
}

# Borrowed from `rutils`: github.com/danielvartan/rutils
get_file_name_without_ext <- function(file) {
  checkmate::assert_character(file)

  ext <- get_file_ext(file)

  ifelse(
    is.na(ext),
    file |> basename(),
    file |>
      basename() |>
      stringr::str_replace(paste0("\\", ext, "$"), "")
  )
}

# Borrowed from `groomr`: github.com/danielvartan/groomr
make_machine_readable <- function(x) {
  checkmate::assert_character(x)

  x |>
    stringr::str_to_lower() |>
    stringr::str_squish() |>
    stringr::str_replace_all(" - |_| ", "-") |>
    iconv(to = "ASCII//TRANSLIT") |>
    stringr::str_replace_all("[^A-Za-z0-9-_.]", "")
}

# Borrowed from `rutils`: github.com/danielvartan/rutils
shush <- function(x, quiet = TRUE) {
  if (isTRUE(quiet)) {
    suppressMessages(suppressWarnings(x))
  } else {
    x
  }
}
