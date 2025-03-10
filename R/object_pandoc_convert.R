# object_pandoc_convert("Hello [World](https://www.teste.com.br/)")
object_pandoc_convert <- function(
    x, #nolint
    from = "markdown",
    to = "latex",
    citeproc = FALSE,
    options = NULL,
    verbose = FALSE
  ) {
  checkmate::assert_character(x)
  checkmate::assert_string(from, null.ok = TRUE)
  checkmate::assert_string(to, null.ok = TRUE)
  checkmate::assert_flag(citeproc)
  checkmate::assert_character(options, null.ok = TRUE)
  checkmate::assert_flag(verbose)

  in_file <- tempfile()
  out_file <- tempfile()
  writeLines(x, in_file)

  rmarkdown::pandoc_convert(
    input = in_file,
    to = to,
    from = from,
    output = out_file,
    citeproc = citeproc,
    options = options,
    verbose = verbose,
    wd = tempdir()
  ) |>
    rutils::shush()

  readLines(out_file)
}
