test_that("bbt_write_bib() | General test", {
  bbt_write_bib("TeSt") |> testthat::expect_error()
})
