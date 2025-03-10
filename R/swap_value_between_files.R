swap_value_between_files <- function(
    from, #nolint
    to = from,
    begin_tag,
    end_tag,
    value,
    quarto_render = FALSE,
    cite_method = "biblatex"
  ) {
  checkmate::assert_string(from)
  checkmate::assert_file_exists(from, "r")
  checkmate::assert_string(to)
  checkmate::assert_file_exists(to, "rw")
  checkmate::assert_string(begin_tag)
  checkmate::assert_string(end_tag)
  checkmate::assert_multi_class(
    value, c("character", "function"), null.ok = TRUE
  )
  checkmate::assert_flag(quarto_render)
  checkmate::assert_choice(cite_method, c("citeproc", "biblatex", "natbib"))

  # nolint start: object_usage_linter.
  from_format <- to_format  <- NULL
  # nolint end

  if (!identical(from, to) && is.null(value)) {
    value <- get_value_between_tags(
      x = from,
      begin_tag = begin_tag,
      end_tag = end_tag
    )
  }

  if (isTRUE(quarto_render) &&
      stringr::str_detect(from, "\\.tex$", negate = TRUE) && #nolint
      stringr::str_detect(to, "\\.tex$")) {
    value <- object_quarto_render(
      x = value,
      output_format = "latex",
      cite_method = "biblatex"
    )
  }

  swap_value_between_tags(
    x = to,
    value = value,
    begin_tag = begin_tag,
    end_tag = end_tag
  ) |>
    writeLines(to)

  invisible()
}
