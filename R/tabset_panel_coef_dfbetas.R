tabset_panel_coef_dfbetas <- function( #nolint
    fit_engine, #nolint
    coef = names(stats::coef(fit_engine)),
    coef_labels = coef,
    source = rep("Created by the author.", length(coef)),
    heading = "###",
    fit_engine_name = "fit_engine",
    suffix = "",
    root = "..",
    write = TRUE,
    verbose = TRUE
  ) {
  checkmate::assert_class(fit_engine, "lm")
  checkmate::assert_character(
    coef,
    len = length(stats::coef(fit_engine)),
    any.missing = FALSE
  )
  checkmate::assert_subset(coef, names(stats::coef(fit_engine)))
  checkmate::assert_character(coef_labels, len = length(coef))
  checkmate::assert_character(source, len = length(coef))
  checkmate::assert_string(heading, pattern = "^#*")
  checkmate::assert_string(fit_engine_name)
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
    glue::glue("_tabset-panel-coef-dfbetas-{suffix}.qmd")
  )

  libraries <-
    c("ggplot2", "olsrr", "stats") |>
    sort() %>%
    paste0("library(", ., ")", collapse = "\n")

  out <- glue::glue(
    "
    :::: {{.panel-tabset}}

    ```{{r}}
    #| code-fold: true

      plots <- {fit_engine_name} |> olsrr::ols_plot_dfbetas(print_plot = FALSE)
      coef_names <- stats::coef({fit_engine_name}) |> names()
    ```
    \n
    "
  )

  coef_labels <- coef_labels |> stringr::str_remove_all("\\(|\\)")

  # Source: {source[i]}

  for (i in seq_along(coef)) {
    if (i == length(coef)) {
      end <- ""
    } else {
      end <- "\n\n"
    }

    coef_fix_1 <-
      coef[i] |>
      stringr::str_to_lower() |>
      stringr::str_remove_all("\\(|\\)") |>
      stringr::str_replace_all("_", "-")

    coef_fix_2 <-
      coef[i] |>
      stringr::str_remove_all("\\(|\\)")

    out <- c(
      out,
      glue::glue(
        "
        {heading} {coef_labels[i]}

        ::: {{#tbl-{suffix}-diag-influence-{coef_fix_1}}}
        ```{{r}}
        #| code-fold: true

        plots$plots[[{i}]] +
        ggplot2::labs(title = '{coef_labels[i]} coefficient')
        ```

        Standardized DFBETAS values for each observation concerning the
        `{coef_fix_2}` coefficient.
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
