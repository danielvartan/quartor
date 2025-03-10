swap_value_between_tags <- function(
    x, #nolint
    value,
    begin_tag = "%:::% .common h1 begin %:::%",
    end_tag = "%:::% .common h1 end %:::%"
  ) {
  checkmate::assert_character(x)
  checkmate::assert_multi_class(value, c("character", "function"))
  checkmate::assert_string(begin_tag)
  checkmate::assert_string(end_tag)

  if (length(x) == 1 && checkmate::test_file_exists(x)) x <- readLines(x)

  begin_index <- grep(begin_tag, x = x)
  end_index <- grep(end_tag, x = x)

  if (length(begin_index) == 0 || length(end_index) == 0) {
    cli::cli_abort("One or both of the tags were not found.")
  }

  if (!length(begin_index) == 1 || !length(end_index) == 1) {
    cli::cli_abort(
      paste0(
        "More than one tag with the same value was found. ",
        "Tags must be unique."
      )
    )
  }

  if (inherits(value, "function")) {
    value <- x[rutils:::inbetween_integers(begin_index, end_index)] |> value()
  }

  x[seq(1, begin_index)] |>
    append(value) |>
    append(x[seq(end_index, length(x))])
}
