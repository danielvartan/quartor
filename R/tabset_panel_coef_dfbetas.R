tabset_panel_coef_dfbetas <- function( #nolint
    model, #nolint
    coef = model |> extract_fit_engine() |> stats::coef() |> names(),
    coef_labels = coef,
    source = rep("Created by the author.", length(coef)),
    heading = "###",
    model_name = deparse(substitute(model)),
    suffix = "",
    root = ".",
    write = TRUE,
    verbose = TRUE
  ) {
  checkmate::assert_multi_class(model, c("lm", "model_fit", "workflow"))
  checkmate::assert_character(
    coef,
    len = model |> extract_fit_engine() |> stats::coef() |> length(),
    any.missing = FALSE
  )
  checkmate::assert_subset(coef, names(stats::coef(model)))
  checkmate::assert_character(coef_labels, len = length(coef))
  checkmate::assert_character(source, len = length(coef))
  checkmate::assert_string(heading, pattern = "^#*")
  checkmate::assert_string(model_name)
  checkmate::assert_string(suffix)
  checkmate::assert_string(root)
  checkmate::assert_flag(write)
  checkmate::assert_flag(verbose)

  # R CMD Check variable bindings fix
  # nolint start
  . <- NULL
  # nolint end

  model <- model |> extract_fit_engine()
  suffix <- as.character(suffix)

  if (!file.exists(here::here("qmd"))) dir.create(here::here("qmd"))
  if (!suffix == "") suffix <- paste0("-", suffix)

  file <- here::here(
    "qmd",
    glue::glue("_tabset-panel-coef-dfbetas{suffix}.qmd")
  )

  libraries <-
    c("ggplot2", "olsrr", "stats") |>
    sort() %>%
    paste0("library(", ., ")", collapse = "\n")

  out <- glue::glue(
    '
    :::: {{.panel-tabset}}

    ```{{r}}
    #| code-fold: true

      plots <- {model_name} |> olsrr::ols_plot_dfbetas(print_plot = FALSE)
      coef_names <- stats::coef({model_name}) |> names()
    ```
    \n
    '
  )

  coef_labels <- coef_labels |> stringr::str_remove_all("\\(|\\)")

  # Source: {source[i]}

  for (i in seq_along(coef)) {
    if (i == length(coef)) {
      end <- ""
    } else {
      end <- "\n\n"
    }

    coef_fix_1 <- coef[i] |> make_machine_readable()
    coef_fix_2 <- coef[i] |> stringr::str_remove_all("\\(|\\)")

    out <- c(
      out,
      glue::glue(
        '
        {heading} {coef_labels[i]}

        ::: {{#tbl-coef-dfbetas{suffix}-{coef_fix_1}}}
        ```{{r}}
        #| code-fold: true

        plots$plots[[{i}]] +
        ggplot2::labs(title = "{coef_labels[i]} coefficient")
        ```

        Standardized DFBETAS values for each observation concerning the
        `{coef_fix_2}` coefficient.
        :::{end}
        '
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
