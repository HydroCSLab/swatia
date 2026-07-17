#' Evaluate the objective function
#'
#' Updates parameters, runs SWAT+, extracts simulated discharge, computes
#' calibration and validation objective values, and records results.
#'
#' @param x Numeric vector. Normalized parameter values in \code{[0, 1]}.
#' @param opt List. Runtime options (e.g., worker ID, iteration, run number).
#' @param config List. SWATIA configuration object.
#'
#' @return Numeric scalar. Calibration objective value.
#' @keywords internal
run_swatplus <- function(x, opt, config) {
  x[x < 0] <- 0
  x[x > 1] <- 1
  par_min <- sapply(config$par, function(x) x$range[1])
  par_max <- sapply(config$par, function(x) x$range[2])

  par_val <- data.frame(
    parameter = names(config$par),
    change_type = vapply(config$par, `[[`, character(1), "change_type"),
    value = par_min + (par_max - par_min) * x,
    row.names = NULL
  )

  dir <- sprintf("TxtInOut_%d", opt$worker_id)
  update_calibration_cal(dir, par_val)

  run_swatplus_in_dir(dir)

  interval <- config$interval
  chaid <- sprintf("cha%03d", config$chaid)

  suffix <- switch(interval, daily = "day", monthly = "mon", yearly = "yr")
  sim_txt <- sprintf("%s/sim_%s_%05d.txt", config$sim_dir, suffix, opt$run)

  sim <- extract_sim(dir, interval, chaid, sim_txt)
  sim_c <- sim[1:config$nobs_c]
  sim_v <- sim[(config$nobs_c + 1):config$nobs]
  obj_c <- config$calc_obj(config$obs_c, sim_c)
  obj_v <- config$calc_obj(config$obs_v, sim_v)

  utils::write.table(
    sprintf("%d,%f,%f,%s", opt$run, obj_c, obj_v, paste(x, collapse = ",")),
    config$obj_txt,
    quote = FALSE,
    row.names = FALSE,
    col.names = FALSE,
    append = TRUE
  )

  obj_c
}

#' Run SWAT+ in a directory
#'
#' Executes the SWAT+ model in the specified directory.
#'
#' @param dir Character. Path to a SWAT+ TxtInOut_* directory.
#'
#' @return Exit status from \code{system2()}.
#' @keywords internal
run_swatplus_in_dir <- function(dir) {
  old <- getwd()
  on.exit(setwd(old))
  setwd(dir)
  system2("swatplus")
}
