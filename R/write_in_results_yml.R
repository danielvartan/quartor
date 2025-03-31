write_in_results_yml <- function(
    x, #nolint
    path = here::here("_results.yml"),
    digits = 30
  ) {
  checkmate::assert_list(x)
  checkmate::assert_file_exists(path)

  out <- yaml::read_yaml(path)

  for (i in seq_along(x)) {
    if (is.numeric(x[[i]])) {
      x[[i]] <- x[[i]] |> signif(digits)
    }

    if (names(x)[i] %in% names(out)) {
      out[[names(x)[i]]] <- x[[i]]
    } else {
      out <- c(out, x[i])
    }
  }

  yaml::write_yaml(out, path)
}
