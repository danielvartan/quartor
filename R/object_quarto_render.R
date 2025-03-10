# object_quarto_render("Hello [World](http://www.test.com). @test")
object_quarto_render <- function(
    x, #nolint
    output_format = "latex",
    cite_method = "biblatex"
  ) {
  checkmate::assert_character(x, min.len = 1)
  checkmate::assert_choice(output_format, c("html", "latex"))
  checkmate::assert_choice(cite_method, c("citeproc", "biblatex", "natbib"))

  ext <- switch(
    output_format,
    html = ".html",
    latex = ".tex"
  )

  in_file <- tempfile(fileext = ".qmd")

  out_file <- file.path(
    dirname(in_file),
    paste0(rutils:::get_file_name_without_ext(in_file), ext)
  )

  if (output_format == "latex") {
    begin_tag <- "%:::% clip start %:::%"
    end_tag <- "%:::% clip end %:::%"
  } else {
    begin_tag <- "<-- %:::% clip start %:::% -->"
    end_tag <- "<--%:::% clip end %:::% -->"
  }

  fake_content <- c(
    "---",
    "format:",
    paste0("  ", output_format, ":"),
    paste0("    cite-method: ", cite_method),
    "---",
    "",
    "# Placeholder",
    "",
    ifelse(output_format == "latex", "```{=latex}", ""),
    begin_tag,
    ifelse(output_format == "latex", "```", ""),
    "",
    x,
    "",
    ifelse(output_format == "latex", "```{=latex}", ""),
    end_tag,
    ifelse(output_format == "latex", "```", "")
  )

  writeLines(fake_content, in_file)
  quarto::quarto_render(input = in_file, quiet = TRUE)

  get_value_between_tags(
    x = readLines(out_file),
    begin_tag = begin_tag,
    end_tag = end_tag
  )
}
