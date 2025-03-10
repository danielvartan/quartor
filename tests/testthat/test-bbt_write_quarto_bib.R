testthat::test_that("bbt_write_quarto_bib() | General test", {
  testthat::local_mocked_bindings(
    bbt_scan_citation_keys = function(...) TRUE,
    bbt_write_bib = function(...) TRUE
  )

  bbt_write_quarto_bib(
    bib_file = here::here("references.json"),
    dir = c(""),
    pattern = "\\.qmd$|\\.tex$",
    ignore = NULL,
    translator = "json",
    library_id = 1,
    overwrite = TRUE,
    filter = identity,
    wd = here::here()
  ) |>
    testthat::expect_null()
})

testthat::test_that("bbt_write_quarto_bib() | Error test", {
  option <- getOption("BRANDR_BRAND_YML")

  # checkmate::assert_string(wd)
  bbt_write_quarto_bib(
    bib_file = here::here("references.json"),
    dir = c(""),
    pattern = "\\.qmd$|\\.tex$",
    ignore = NULL,
    translator = "json",
    library_id = 1,
    overwrite = TRUE,
    filter = identity,
    wd = 1
  ) |>
    testthat::expect_error()

  # checkmate::assert_directory_exists(wd)
  bbt_write_quarto_bib(
    bib_file = here::here("references.json"),
    dir = c(""),
    pattern = "\\.qmd$|\\.tex$",
    ignore = NULL,
    translator = "json",
    library_id = 1,
    overwrite = TRUE,
    filter = identity,
    wd = "TeSt12345"
  ) |>
    testthat::expect_error()

  # checkmate::assert_string(bib_file)
  bbt_write_quarto_bib(
    bib_file = 1,
    dir = c(""),
    pattern = "\\.qmd$|\\.tex$",
    ignore = NULL,
    translator = "json",
    library_id = 1,
    overwrite = TRUE,
    filter = identity,
    wd = here::here()
  ) |>
    testthat::expect_error()

  # checkmate::assert_path_for_output(bib_file, overwrite = TRUE)
  bbt_write_quarto_bib(
    bib_file = "./TeSt12345/TeSt12345",
    dir = c(""),
    pattern = "\\.qmd$|\\.tex$",
    ignore = NULL,
    translator = "json",
    library_id = 1,
    overwrite = TRUE,
    filter = identity,
    wd = here::here()
  ) |>
    testthat::expect_error()

  # checkmate::assert_character(dir)
  bbt_write_quarto_bib(
    bib_file = here::here("references.json"),
    dir = 1,
    pattern = "\\.qmd$|\\.tex$",
    ignore = NULL,
    translator = "json",
    library_id = 1,
    overwrite = TRUE,
    filter = identity,
    wd = here::here()
  ) |>
    testthat::expect_error()

  # for (i in dir) checkmate::assert_directory_exists(file.path(wd, i))
  bbt_write_quarto_bib(
    bib_file = here::here("references.json"),
    dir = "TeSt12345",
    pattern = "\\.qmd$|\\.tex$",
    ignore = NULL,
    translator = "json",
    library_id = 1,
    overwrite = TRUE,
    filter = identity,
    wd = here::here()
  ) |>
    testthat::expect_error()

  # checkmate::assert_string(pattern)
  bbt_write_quarto_bib(
    bib_file = here::here("references.json"),
    dir = c(""),
    pattern = 1,
    ignore = NULL,
    translator = "json",
    library_id = 1,
    overwrite = TRUE,
    filter = identity,
    wd = here::here()
  ) |>
    testthat::expect_error()

  # checkmate::assert_string(ignore, null.ok = TRUE)
  bbt_write_quarto_bib(
    bib_file = here::here("references.json"),
    dir = c(""),
    pattern = "\\.qmd$|\\.tex$",
    ignore = 1,
    translator = "json",
    library_id = 1,
    overwrite = TRUE,
    filter = identity,
    wd = here::here()
  ) |>
    testthat::expect_error()

  # checkmate::assert_choice(translator, translator_choices)
  bbt_write_quarto_bib(
    bib_file = here::here("references.json"),
    dir = c(""),
    pattern = "\\.qmd$|\\.tex$",
    ignore = NULL,
    translator = 1,
    library_id = 1,
    overwrite = TRUE,
    filter = identity,
    wd = here::here()
  ) |>
    testthat::expect_error()

  # checkmate::assert_number(library_id)
  bbt_write_quarto_bib(
    bib_file = here::here("references.json"),
    dir = c(""),
    pattern = "\\.qmd$|\\.tex$",
    ignore = NULL,
    translator = "json",
    library_id = "a",
    overwrite = TRUE,
    filter = identity,
    wd = here::here()
  ) |>
    testthat::expect_error()

  # checkmate::assert_flag(overwrite)
  bbt_write_quarto_bib(
    bib_file = here::here("references.json"),
    dir = c(""),
    pattern = "\\.qmd$|\\.tex$",
    ignore = NULL,
    translator = "json",
    library_id = 1,
    overwrite = "a",
    filter = identity,
    wd = here::here()
  ) |>
    testthat::expect_error()

  # checkmate::assert_function(filter)
  bbt_write_quarto_bib(
    bib_file = here::here("references.json"),
    dir = c(""),
    pattern = "\\.qmd$|\\.tex$",
    ignore = NULL,
    translator = "json",
    library_id = 1,
    overwrite = TRUE,
    filter = 1,
    wd = here::here()
  ) |>
    testthat::expect_error()
})
