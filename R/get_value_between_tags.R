get_value_between_tags <- function(
    x,
    begin_tag = "%:::% .common h1 begin %:::%",
    end_tag = "%:::% .common h1 end %:::%") {
  checkmate::assert_character(x)
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

  x[rutils:::inbetween_integers(begin_index, end_index)]
}
