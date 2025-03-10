convert_quarto_ref_to_latex <- function(x) {
  checkmate::assert_character(x, min.len = 1)

  stringr::str_replace_all(
    x,
    "@.+",
    ~ paste0("\\textcite{", substring(.x, 2), "}")
  )
}
