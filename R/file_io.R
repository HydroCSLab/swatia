#' Update output interval in the `print.prt` file
#'
#' Enables the requested output interval for `channel_sd` in `print.prt`
#' without modifying any other output settings.
#'
#' @param dir Character. Path to a TxtInOut_* directory.
#' @param interval Character. Output interval to enable. One of `"daily"`,
#'   `"monthly"`, or `"yearly"`.
#'
#' @return Invisibly returns NULL.
#' @keywords internal
update_print_prt <- function(dir, interval) {
  path <- file.path(dir, "print.prt")
  lines <- readLines(path)
  l <- grep("^\\s*channel_sd\\s", lines)
  n <- switch(interval, daily = 0, monthly = 1, yearly = 2)
  lines[l] <- sub(
    paste0("^(\\s*\\S+)((?:\\s+\\S+){", n, "})(\\s+)\\S+(.*)$"),
    "\\1\\2\\3y\\4",
    lines[l]
  )
  writeLines(lines, path)
}

#' Check calibration parameters in the `calibration.cal` file
#'
#' Verifies that all parameters specified in `par_val` are present in
#' `calibration.cal`.
#'
#' @param dir Character. Path to a TxtInOut_* directory.
#' @param par_val Data frame of calibration parameters. The `parameter`
#'   column is checked against `calibration.cal`.
#'
#' @return Invisibly returns `NULL`. An error is raised if any parameter
#'   is not found in `calibration.cal`.
check_calibration_cal <- function(dir, par_val) {
  path <- file.path(dir, "calibration.cal")
  lines <- readLines(path)

  parameters <- sub("^\\s*(\\^S+).*$", "\\1", lines)
  bad <- which(!par_val$parameter %in% parameters)

  if (length(bad)) {
    stop(
      "Parameters not found in calibration.cal: ",
      paste(par_val$parameter[bad], collapse = ", ")
    )
  }
}

#' Update the `calibration.cal` file
#'
#' Modifies the `calibration.cal` file in the specified directory using the
#' provided parameter values.
#'
#' @param dir Character. Path to a TxtInOut_* directory.
#' @param par_val Data frame. Candidate calibration values with columns
#'   `parameter`, `change_type`, and `value`. `parameter` is a character vector
#'   of parameter names. `change_type` must be one of c("absval", "abschg",
#'   "pctchg", "relchg"). `value` is numeric and interpreted according to
#'   `change_type`.
#'
#' @return Invisibly returns NULL.
#' @keywords internal
update_calibration_cal <- function(dir, par_val) {
  path <- file.path(dir, "calibration.cal")
  lines <- readLines(path)
  for (i in seq_len(nrow(par_val))) {
    row <- par_val[i, ]
    l <- grep(paste0("^\\s*", row$parameter, "\\s"), lines)
    lines[l] <- sub(
      "^(\\s*\\S+\\s+)\\S+(\\s+)\\S+(.*)$",
      paste0("\\1", row$change_type, "\\2", row$value, "\\3"),
      lines[l]
    )
  }
  writeLines(lines, path)
}

#' Extract simulated discharge
#'
#' Reads simulated discharge values for a given channel ID from an output
#' directory.
#'
#' @param dir Character. Path to a TxtInOut_* directory.
#' @param interval Character. Output interval used for calibration. One of
#' `"daily"`, `"monthly"`, or `"yearly"`.
#' @param chaid Integer or character. Channel ID.
#'
#' @return Numeric vector of simulated discharge values.
#' @keywords internal
extract_sim <- function(dir, interval, chaid, sim_txt) {
  suffix <- switch(interval, daily = "day", monthly = "mon", yearly = "yr")
  path <- file.path(dir, sprintf("channel_sd_%s.txt", suffix))

  header <- strsplit(trimws(readLines(path, n = 2)[2]), "\\s+")[[1]]

  name_col <- "name"
  flo_out_col <- "flo_out"

  if (requireNamespace("data.table", quietly = TRUE)) {
    dt <- data.table::fread(
      path,
      header = FALSE,
      skip = 3,
      select = match(c(name_col, flo_out_col), header)
    )
    data.table::setnames(dt, c(name_col, flo_out_col))
  } else {
    dt <- utils::read.table(path, skip = 3, col.names = header)[, c(
      name_col,
      flo_out_col
    )]
  }

  sim <- dt[[flo_out_col]][dt[[name_col]] == chaid]
  utils::write.table(sim, sim_txt, row.names = FALSE, col.names = FALSE)

  invisible(sim)
}
