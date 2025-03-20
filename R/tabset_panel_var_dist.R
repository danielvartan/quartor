tabset_panel_var_dist <- function( #nolint
    data, #nolint
    cols,
    col_labels = NULL,
    jitter = TRUE,
    source = "Created by the authors.",
    heading = "###",
    data_name = deparse(substitute(data)),
    suffix = "",
    root = ".",
    qual_chart = TRUE,
    write = TRUE,
    verbose = TRUE
  ) {
  checkmate::assert_tibble(data)
  checkmate::assert_character(cols, min.len = 1, any.missing = FALSE)
  checkmate::assert_subset(cols, names(data))
  checkmate::assert_character(col_labels, len = length(cols), null.ok = TRUE)
  checkmate::assert_flag(jitter)
  checkmate::assert_string(source, null.ok = TRUE)
  checkmate::assert_string(heading, pattern = "^#*")
  checkmate::assert_string(data_name)
  checkmate::assert_atomic(suffix)
  checkmate::assert_string(root)
  checkmate::assert_flag(qual_chart)
  checkmate::assert_flag(write)
  checkmate::assert_flag(verbose)

  # R CMD Check variable bindings fix
  # nolint start
  . <- NULL
  # nolint end

  sample <- sample(1000:9999, 1)
  suffix <- as.character(suffix)

  if (is.null(col_labels)) col_labels <- cols
  if (!file.exists(here::here("qmd"))) dir.create(here::here("qmd"))
  if (!suffix == "") suffix <- paste0("-", suffix)

  file <- here::here(
    "qmd",
    glue::glue("_tabset-panel-var-distribution{suffix}.qmd")
  )

  libraries <-
    c("cli", "clipr", "glue", "here", "plotr", "readr", "summarytools") |>
    sort() %>%
    paste0("library(", ., ")", collapse = "\n")

  out <- ":::: {.panel-tabset}\n"

  for (i in seq_along(cols)) {
    col_fix <- cols[i] |> stringr::str_replace_all("_", "-")

    if (isTRUE(jitter)) {
      box_plot_caption <- glue::glue(
        "Box plot of the `{cols[i]}` variable with jittered data points ",
        "in black."
      )
    } else {
      box_plot_caption <- glue::glue("Box plot of the `{cols[i]}` variable")
    }

    if (!is.numeric(data[[cols[i]]])) {
      out <- c(
        out,
        glue::glue(
          '
        {heading} `{col_labels[i]}`

        ::: {{#tbl-var-distribution-freqs-{col_fix}-{sample}}}
        ```{{r}}
        #| code-fold: true
        #| output: asis

        {data_name} |>
          summarytools::freq(
            var = {cols[i]},
            style = "rmarkdown",
            plain.ascii = FALSE,
            headings = FALSE
          )
        ```

        [Source: {source}]{{.legend}}

        Frequencies of the `{cols[i]}` variable.
        :::
          '
        )
      )

      if (isTRUE(qual_chart)) {
        out <- c(
          out,
          "\n\n",
          glue::glue(
            '
      ::: {{#fig-var-distribution-charts-bar-plot-{col_fix}-{sample}}}
      ```{{r}}
      #| code-fold: true

      {data_name} |>
        plotr:::plot_bar(
          col = "{cols[i]}",
          y_label = "{col_labels[i]}"
        )
      ```

      [Source: {source}]{{.legend}}

      Bar plot of the `{cols[i]}` variable.
      :::
            '
          )
        )
      }

    } else {
      out <- c(
        out,
        glue::glue(
          '
      {heading} `{col_labels[i]}`

      ::: {{#tbl-var-distribution-stats-{col_fix}-{sample}}}
      ```{{r}}
      #| code-fold: true
      #| output: asis

      {data_name} |>
        summarytools::descr(
          var = {cols[i]},
          style = "rmarkdown",
          plain.ascii = FALSE,
          headings = FALSE
        )
      ```

      [Source: {source}]{{.legend}}

      Statistics for the `{cols[i]}` variable.
      :::

      ::: {{#fig-var-distribution-charts-hist-qq-plot-{col_fix}-{sample}}}
      ```{{r}}
      #| code-fold: true

      {data_name} |>
        plotr::plot_dist(col = "{cols[i]}")
      ```

      [Source: {source}]{{.legend}}

      Histogram of the `{cols[i]}` variable with a kernel density
      estimate, along with a quantile-quantile (Q-Q) plot between the
      variable and the theoretical quantiles of the normal distribution.
      :::

      ::: {{#fig-var-distribution-charts-box-plot-{col_fix}-{sample}}}
      ```{{r}}
      #| code-fold: true

      {data_name} |>
        plotr::plot_box_plot(col = "{cols[i]}")
      ```

      [Source: {source}]{{.legend}}

      {box_plot_caption}
      :::
        '
        )
      )
    }

    if (!i == length(cols)) out <- c(out, "\n\n")
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
