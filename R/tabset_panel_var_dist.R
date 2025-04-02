tabset_panel_var_dist <- function( #nolint
    data, #nolint
    cols,
    col_labels = cols,
    jitter = FALSE,
    source = "Created by the author.",
    heading = "###",
    data_name = deparse(substitute(data)),
    suffix = "",
    root = ".",
    qual_chart = TRUE,
    summarytools = TRUE,
    write = TRUE,
    verbose = TRUE
  ) {
  checkmate::assert_data_frame(data)
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
  checkmate::assert_flag(summarytools)
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
    glue::glue("_tabset-panel{suffix}-var-dist.qmd")
  )

  libraries <-
    ifelse(isTRUE(summarytools), "summarytools", "rutils") |>
    c("plotr") |>
    sort() %>%
    paste0("library(", ., ")", collapse = "\n")

  out <- ":::: {.panel-tabset}\n"

  for (i in seq_along(cols)) {
    col_fix <- cols[i] |> make_machine_readable()

    if (isTRUE(jitter)) {
      box_plot_caption <- glue::glue(
        "Box plot of the `{cols[i]}` variable with jittered data points ",
        "in black."
      )
    } else {
      box_plot_caption <- glue::glue("Box plot of the `{cols[i]}` variable")
    }

    if (!is.numeric(data[[cols[i]]]) &&
          !prettycheck::test_temporal(data[[cols[i]]])) {
      if (isTRUE(summarytools)) {
        freq_fun <- glue::glue(
          '
          summarytools::freq(
              var = {cols[i]},
              style = "rmarkdown",
              plain.ascii = FALSE,
              headings = FALSE
            )
          '
        )
      } else {
        freq_fun <- glue::glue(
          '
          rutils:::stats_summary(
              col = "{cols[i]}",
              na_rm = TRUE,
              remove_outliers = FALSE,
              iqr_mult = 1.5,
              hms_format = TRUE,
              threshold = hms::parse_hms("12:00:00"),
              as_list = FALSE
            )
          '
        )
      }

      out <- c(
        out,
        glue::glue(
          '
          {heading} {col_labels[i]}

          ::: {{#tbl{suffix}-var-dist-freqs-{col_fix}}}
          ```{{r}}
          #| code-fold: true
          #| output: asis

          {data_name} |>
            {freq_fun}
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
            ::: {{#fig{suffix}-var-dist-charts-bar-plot-{col_fix}}}
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
      if (isTRUE(summarytools)) {
        descr_fun <- glue::glue(
          '
          summarytools::descr(
              var = {cols[i]},
              style = "rmarkdown",
              plain.ascii = FALSE,
              headings = FALSE
            )
          '
        )
      } else {
        descr_fun <- glue::glue(
          '
          rutils:::stats_summary(
              col = "{cols[i]}",
              na_rm = TRUE,
              remove_outliers = FALSE,
              iqr_mult = 1.5,
              hms_format = TRUE,
              threshold = hms::parse_hms("12:00:00"),
              as_list = FALSE
            )
          '
        )
      }

      out <- c(
        out,
        glue::glue(
          '
      {heading} {col_labels[i]}

      ::: {{#tbl{suffix}-var-dist-stats-{col_fix}}}
      ```{{r}}
      #| code-fold: true
      #| output: asis

      {data_name} |>
        {descr_fun}
      ```

      [Source: {source}]{{.legend}}

      Statistics for the `{cols[i]}` variable.
      :::

      ::: {{#fig{suffix}-var-dist-charts-hist-qq-plot-{col_fix}}}
      ```{{r}}
      #| code-fold: true

      {data_name} |>
        plotr:::plot_dist(
          col = "{cols[i]}",
          x_label = "{col_labels[i]}"
        )
      ```

      [Source: {source}]{{.legend}}

      Histogram of the `{cols[i]}` variable with a kernel density
      estimate, along with a quantile-quantile (Q-Q) plot between the
      variable and the theoretical quantiles of the normal distribution.
      :::

      ::: {{#fig{suffix}-var-dist-charts-box-plot-{col_fix}}}
      ```{{r}}
      #| code-fold: true

      {data_name} |> plotr:::plot_box_plot(col = "{cols[i]}")
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
