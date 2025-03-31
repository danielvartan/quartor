tabset_panel_var_homoscedasticity <- function( #nolint
    data, #nolint
    fit,
    cols,
    col_labels = cols,
    source = rep("Created by the author.", length(cols)),
    heading = "###",
    data_name = "data",
    fit_name = "fit",
    suffix = "",
    root = "..",
    write = TRUE,
    verbose = TRUE
  ) {
  checkmate::assert_tibble(data)
  checkmate::assert_class(fit, "workflow")
  checkmate::assert_character(cols, min.len = 1, any.missing = FALSE)
  checkmate::assert_subset(cols, names(data))
  checkmate::assert_character(col_labels, len = length(cols))
  checkmate::assert_character(source, len = length(cols))
  checkmate::assert_string(heading, pattern = "^#*")
  checkmate::assert_string(data_name)
  checkmate::assert_string(fit_name)
  checkmate::assert_string(suffix)
  checkmate::assert_string(root)
  checkmate::assert_flag(write)
  checkmate::assert_flag(verbose)

  # R CMD Check variable bindings fix
  # nolint start
  . <- NULL
  # nolint end

  if (!file.exists(here::here("qmd"))) dir.create(here::here("qmd"))

  file <- here::here(
    "qmd",
    glue::glue("_tabset-panel-var-homoscedasticity-{suffix}.qmd")
  )

  libraries <-
    c("dplyr", "ggplot2", "latex2exp", "parsnip", "rutils", "stats") |>
    sort() %>%
    paste0("library(", ., ")", collapse = "\n")

  out <- ":::: {.panel-tabset}\n"

  # Source: {source[i]}

  for (i in seq_along(cols)) {
    if (i == length(cols)) {
      end <- ""
    } else {
      end <- "\n\n"
    }

    col_fix <-
      cols[i] |>
      stringr::str_to_lower() |>
      stringr::str_replace_all("_", "-")

    out <- c(
      out,
      glue::glue(
        "
      {heading} {col_labels[i]}

      ::: {{#tbl-{suffix}-diag-homoscedasticity-{col_fix}}}
      ```{{r}}
      #| code-fold: true

      plot <-
        {fit_name} |>
        stats::predict({data_name}) |>
        dplyr::mutate(
          .sd_resid =
            {fit_name} |>
            parsnip::extract_fit_engine() |>
            stats::rstandard() |>
            abs() |>
            sqrt()
        ) |>
        dplyr::bind_cols(data) |>
        ggplot2::ggplot(ggplot2::aes({cols[i]}, .sd_resid)) +
        ggplot2::geom_point() +
        ggplot2::geom_smooth(color = 'red') +
        ggplot2::labs(
          x = '{col_labels[i]}',
          y = latex2exp::TeX('$\\\\sqrt{{|Standardized \\\\ Residuals|}}$')
        )

      plot |> print() |> rutils::shush()
      ```

      Relation between `{cols[i]}` and the model standardized residuals.
      :::{end}
      "
      )
    )
  }

  out <- c(out, "\n::::") |>  paste0(collapse = "")
  if (isTRUE(write)) out |> readr::write_lines(file)

  if (isTRUE(verbose)) {
    include_string <- glue::glue(
      "{{{{< include {to_relative_path(file, root)} >}}}}"
    )

    cli::cli_alert_info(
      glue::glue(
        "Use `{{include_string}}` to include ",
        "the panel in the file (Copied to clipboard).",
        "\n\n",
        "Also, don't forget call the libraries below.",
        "\n\n",
        libraries,
        wrap = TRUE
      )
    )

    clipr::write_clip(include_string)
  }

  invisible(out)
}
