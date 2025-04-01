tabset_panel_var_dist_by_misfs <- function( #nolint
    data, #nolint
    cols,
    col_labels = NULL,
    jitter = TRUE,
    source = "Created by the authors.",
    heading = "####",
    root = ".",
    summarytools = TRUE,
    write = TRUE,
    verbose = TRUE
  ) {
  checkmate::assert_data_frame(data)
  checkmate::assert_character(cols, min.len = 1, any.missing = FALSE)
  checkmate::assert_subset(c(cols, "misf"), names(data))
  checkmate::assert_character(col_labels, len = length(cols), null.ok = TRUE)
  checkmate::assert_flag(jitter)
  checkmate::assert_string(source, null.ok = TRUE)
  checkmate::assert_string(heading, pattern = "^#*")
  checkmate::assert_string(root)
  checkmate::assert_flag(summarytools)
  checkmate::assert_flag(write)
  checkmate::assert_flag(verbose)

  # R CMD Check variable bindings fix
  # nolint start
  . <- misf <- NULL
  # nolint end

  if (is.null(col_labels)) col_labels <- cols
  if (!file.exists(here::here("qmd"))) dir.create(here::here("qmd"))

  file <- here::here(
    "qmd",
    glue::glue("_panel-tabset-var-dist-by-misfs.qmd")
  )

  libraries <-
    ifelse(isTRUE(summarytools), "summarytools", "rutils") |>
    c("plotr") |>
    sort() %>%
    paste0("library(", ., ")", collapse = "\n")

  categories <- c("A", "B", "C", "D")
  out <- character()

  for (i in categories) {
    i_data_name  <- paste0("data_misfs_", tolower(i))

    if (!i_data_name %in% ls(envir = .GlobalEnv)) {
      assign(
        i_data_name,
        data |> dplyr::filter(misf == i),
        envir = .GlobalEnv
      )
    }

    out <-
      c(
        out,
        paste0(
          glue::glue("{heading} {i}"),
          "\n\n",
          tabset_panel_var_dist(
            data = get(i_data_name),
            cols = cols,
            col_labels = col_labels,
            jitter = jitter,
            source = source,
            heading = paste0(heading, "#"),
            data_name = i_data_name,
            suffix = tolower(i),
            root = root,
            summarytools = summarytools,
            write = FALSE,
            verbose = FALSE
          )
        )
      )

    if (!i == "D") out <- c(out, "\n\n")
  }

  out <- out |> paste0(collapse = "")
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
