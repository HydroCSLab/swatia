#' Update SWAT+ calibration file
#'
#' Modifies the SWAT+ calibration file in the specified directory using the
#' provided parameter values.
#'
#' @param dir Character. Path to a SWAT+ TxtInOut_* directory.
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
    idx <- grep(paste0("^\\s*", row$parameter, "\\s"), lines)
    lines[idx] <- sub(
      "^(\\s*\\S+\\s+)\\S+(\\s+)\\S+(.*)$",
      paste0("\\1", row$change_type, "\\2", row$value, "\\3"),
      lines[idx],
      perl = TRUE
    )
  }
  writeLines(lines, path)
}

#' Extract simulated discharge
#'
#' Reads simulated discharge values for a given channel ID from a SWAT+ output
#' directory.
#'
#' @param dir Character. Path to a SWAT+ TxtInOut_* directory.
#' @param chaid Integer or character. Channel ID.
#'
#' @return Numeric vector of simulated discharge values.
#' @keywords internal
extract_sim <- function(dir, chaid) {
  infile <- file.path(dir, "channel_sd_day.txt")
  outfile <- file.path(dir, "sim_day.txt")

  hdr <- strsplit(trimws(readLines(infile, n = 2)[2]), "\\s+")[[1]]

  name_col <- "name"
  flo_out_col <- "flo_out"

  if (requireNamespace("data.table", quietly = TRUE)) {
    dt <- data.table::fread(
      infile,
      header = FALSE,
      skip = 3,
      select = match(c(name_col, flo_out_col), hdr)
    )
    data.table::setnames(dt, c(name_col, flo_out_col))
  } else {
    dt <- utils::read.table(infile, skip = 3, col.names = hdr)[, c(
      name_col,
      flo_out_col
    )]
  }

  sim <- dt[[flo_out_col]][dt[[name_col]] == chaid]
  utils::write.table(sim, outfile, row.names = FALSE, col.names = FALSE)

  invisible(sim)
}
