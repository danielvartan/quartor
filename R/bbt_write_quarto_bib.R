#' Write a bibliography file from Zotero citation keys for a whole R project
#'
#' @description
#'
#' `r lifecycle::badge("maturing")`
#'
#' `bbt_write_quarto_bib()` write a bibliography file scanning all citation
#' keys in a R project.
#'
#' Different from [`bbt_write_bib()`][rbbt::bbt_write_bib], this function
#' scans all files in a project directory and subdirectories for citation
#' keys, ensuring that all citations are included in the bibliography file.
#'
#' @details
#'
#' ## BetterBibTeX
#'
#' To use this function, you must have the BetterBibTeX
#' ([BBT](https://retorque.re/zotero-better-bibtex/)) extension installed in
#' your Zotero. Zotero must be open and running when you execute this function.
#'
#' ## Note
#'
#' (2025-03-10) Unfortunately, the author of the `rbbt` package has not yet
#' merged a
#' [crucial pull request](https://github.com/paleolimbot/rbbt/pull/48) into
#' the package repository. If you encounter problems with this function,
#' please try downloading and installing this `rbbt` fork:
#' <https://github.com/wmoldham/rbbt>. You can install it using the following
#' code:
#'
#' ```r
#' remotes::install_github("wmoldham/rbbt")
#' ```
#'
#' @param bib_file (Optional) A [`character`][base::character] string with the
#'   path to the bibliography file (Default: `here::here("references.json")`).
#' @param dir (Optional) A [`character`][base::character] vector indicating the
#'   directories to scan for citation keys (Default: `c("", "qmd", "tex")`).
#' @param pattern (Optional) A [`character`][base::character] string with a
#'   regular expression to match files to scan for citation keys
#'   (Default: `"\.qmd$|\.tex$"`).
#' @param ignore (Optional) A [`character`][base::character] string with a
#'   regular expression to match files to ignore when scanning for citation
#'   keys (Default: `NULL`).
#' @param translator (Optional) A [`character`][base::character] string with
#'   the name of the translator to use when writing the bibliography file.
#'   The available translators are: `json` (CSL-JSON), `biblatex`, `bibtex`
#'   (BibLaTeX), and `yaml` (CSL YAML). CSL-JSON is recommended if users are
#'   not specifically using a LaTeX citation processor. See
#' [`bbt_write_bib()`][rbbt::bbt_write_bib] for more details
#'   (Default: `rbbt::bbt_guess_translator(bib_file)`).
#' @param library_id (Optional) A [`numeric`][base::numeric] integer with the
#'   Zotero library ID to use when writing the bibliography file.
#'   See [`bbt_write_bib()`][rbbt::bbt_write_bib] for more details
#'   (Default: `getOption("rbbt.default.library_id", 1)`).
#' @param overwrite (Optional) A [`logical`][base::logical] flag indicating
#'   whether to overwrite the bibliography file if it already exists
#'   (Default: `TRUE`).
#' @param filter (Optional) A [`function`][base::function] to filter the
#'   bibliography entries before writing the bibliography file. See
#'   [`bbt_write_bib()`][rbbt::bbt_write_bib] for more details
#'   (Default: `identity`).
#' @param wd (Optional) A [`character`][base::character] string with the path
#'   to the working directory (Default: `here::here()`).
#'
#' @return An invisible `NULL`. This function is used for its side effect.
#'
#' @family Zotero functions
#' @export
#'
#' @examples
#' \dontrun{
#'   bbt_write_quarto_bib(
#'     bib_file = "references.bib",
#'     dir = c("", "qmd", "tex"),
#'     pattern = c("\\.qmd$|\\.tex$"),
#'     wd = here::here()
#'  )
#' }
bbt_write_quarto_bib <- function(
    bib_file = here::here("references.json"),
    dir = c("", "qmd", "tex"),
    pattern = "\\.qmd$|\\.tex$",
    ignore = NULL,
    translator = rbbt::bbt_guess_translator(bib_file),
    library_id = getOption("rbbt.default.library_id", 1),
    overwrite = TRUE,
    filter = identity,
    wd = here::here()) {
  translator_choices <- c("json", "biblatex", "bibtex", "yaml")

  checkmate::assert_string(wd)
  checkmate::assert_directory_exists(wd)
  checkmate::assert_string(bib_file)
  checkmate::assert_path_for_output(bib_file, overwrite = TRUE)
  checkmate::assert_character(dir)
  for (i in dir) checkmate::assert_directory_exists(file.path(wd, i))
  checkmate::assert_string(pattern)
  checkmate::assert_string(ignore, null.ok = TRUE)
  checkmate::assert_choice(translator, translator_choices)
  checkmate::assert_number(library_id)
  checkmate::assert_flag(overwrite)
  checkmate::assert_function(filter)

  keys <- bbt_scan_citation_keys(
    dir = dir,
    pattern = pattern,
    ignore = ignore,
    wd = wd
  )

  bbt_write_bib(
    path = bib_file,
    keys = keys,
    translator = translator,
    library_id = library_id,
    overwrite = overwrite,
    filter = filter
  )

  invisible()
}
