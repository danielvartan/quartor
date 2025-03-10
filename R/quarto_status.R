# Use '#| output: asis'
# Credits: <https://github.com/hadley/r4ds/blob/main/_common.R>.
quarto_status <- function(
    type, #nolint
    of_what = "of this book",
    latex_parskip = NULL
  ) {
  checkmate::assert_string(type)
  checkmate::assert_string(of_what)
  checkmate::assert_string(latex_parskip, null.ok = TRUE)

  status <- switch(
    type,
    polishing = paste0(
      "should be readable but is currently undergoing final polishing"
    ),
    restructuring = paste0(
      "is undergoing heavy restructuring and may be confusing or incomplete"
    ),
    drafting = paste0(
      "is currently a dumping ground for ideas, and I don't recommend", " ",
      "reading it"
    ),
    complete = "is largely complete and just needs final proof reading",
    cli::cli_abort("Invalid {.strong {cli::col_red('type')}}.")
  )

  class <- switch(
    type,
    polishing = "note",
    restructuring = "important",
    drafting = "important",
    complete = "note"
  )

  if (!is.null(latex_parskip)) {
    latex_parskip <- paste0("\n", latex_parskip, "\n\n")
  } else {
    latex_parskip <- ""
  }

  cat(paste0(
    "\n",
    "::: {.callout-", class, "}", "\n",
    "You are reading the work-in-progress ", of_what, ".\n",
    latex_parskip,
    "This chapter ", status, ".", "\n",
    ":::",
    "\n"
  ))
}
