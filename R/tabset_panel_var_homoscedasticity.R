tabset_panel_var_homoscedasticity <- function( #nolint
    model, #nolint
    data,
    cols,
    col_labels = cols,
    source = rep("Created by the author.", length(cols)),
    heading = "###",
    data_name = deparse(substitute(data)),
    model_name = deparse(substitute(model)),
    suffix = "",
    root = ".",
    write = TRUE,
    verbose = TRUE
  ) {
  checkmate::assert_multi_class(model, c("lm", "model_fit", "workflow"))
  checkmate::assert_data_frame(data)
  checkmate::assert_character(cols, min.len = 1, any.missing = FALSE)
  checkmate::assert_subset(cols, names(data))
  checkmate::assert_character(col_labels, len = length(cols))
  checkmate::assert_character(source, len = length(cols))
  checkmate::assert_string(heading, pattern = "^#*")
  checkmate::assert_string(data_name)
  checkmate::assert_string(model_name)
  checkmate::assert_string(suffix)
  checkmate::assert_string(root)
  checkmate::assert_flag(write)
  checkmate::assert_flag(verbose)

  # R CMD Check variable bindings fix
  # nolint start
  . <- NULL
  # nolint end

  suffix <- as.character(suffix)

  if (!file.exists(here::here("qmd"))) dir.create(here::here("qmd"))
  if (!suffix == "") suffix <- paste0("-", suffix)

  file <- here::here(
    "qmd",
    glue::glue("_tabset-panel{suffix}-var-homoscedasticity.qmd")
  )

  libraries <-
    c("plotr") |>
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

    col_fix <- cols[i] |> make_machine_readable()

    out <- c(
      out,
      glue::glue(
        '
        {heading} {col_labels[i]}

        ::: {{#tbl{suffix}-var-homoscedasticity-{col_fix}}}
        ```{{r}}
        #| code-fold: true

        {model_name} |>
          plotr:::plot_homoscedasticity(
            data = {data_name},
            col = "{cols[i]}",
            x_label = "{col_labels[i]}"
          )
        ```

        Relation between `{cols[i]}` and the model standardized residuals.
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
