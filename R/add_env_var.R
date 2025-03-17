add_env_var <- function(
    var, #nolint
    yml_file = here::here("_variables.yml")
  ) {
  checkmate::assert_list(var, min.len = 1)

  if (!checkmate::test_file_exists(yml_file)) {
    checkmate::assert_path_for_output(yml_file, overwrite = TRUE)
    fs::file_create(yml_file)
  }

  yml_file_vars <- yaml::read_yaml(yml_file)
  if (is.null(yml_file_vars)) yml_file_vars <- list()

  for (i in names(var)) yml_file_vars[[i]] <- var[[i]]

  yml_file_vars |> yaml::write_yaml(yml_file)

  invisible()
}
