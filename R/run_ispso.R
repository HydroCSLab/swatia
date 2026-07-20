#' Run ISPSO calibration
#'
#' Runs the ISPSO optimization algorithm for SWAT+ parameter calibration using
#' the provided configuration.
#'
#' @param config List. SWATIA configuration object.
#' @param best_x Numeric vector, optional. Initial parameter vector for warm
#'   start.
#'
#' @return List. Optimization results.
#' @export
run_ispso <- function(config, best_x = NULL) {
  obs <- utils::read.table(config$obs_txt)[[1]]

  config$num_obs <- length(obs)
  stopifnot(config$num_obs_c < config$num_obs)

  config$obs_c <- obs[1:config$num_obs_c]
  config$obs_v <- obs[(config$num_obs_c + 1):config$num_obs]

  reset_dir(config$sim_dir)
  unlink(Sys.glob("TxtInOut_*"), recursive = FALSE, force = TRUE)

  nworkers <- min(config$control$S, parallelly::availableCores())
  ndim <- length(config$par)

  check_calibration_cal(config$txtinout, names(config$par))

  for (i in seq_len(nworkers)) {
    txtinout <- sprintf("TxtInOut_%d", i)
    copy_dir(config$txtinout, txtinout)
    update_print_prt(txtinout, config$interval)
  }

  utils::write.table(
    sprintf(
      "run,obj_c,obj_v,%s",
      paste(paste0("x", 1:ndim), collapse = ",")
    ),
    config$obj_txt,
    quote = FALSE,
    row.names = FALSE,
    col.names = FALSE
  )

  bounds <- lapply(config$par, function(x) c(0, 1))

  if (is.null(config$control$cluster)) {
    config$control$cluster <- cl <- parallel::makeCluster(nworkers)
    on.exit(parallel::stopCluster(cl), add = TRUE)
  } else {
    cl <- config$control$cluster
  }

  parallel::clusterExport(
    cl,
    c(
      "run_swatplus",
      "update_calibration_cal",
      "run_swatplus_in_dir",
      "extract_sim"
    ),
    envir = environment()
  )

  ret <- ispso::ispso(
    function(x, opt) run_swatplus(x, opt, config),
    bounds,
    config$control,
    init_pos = best_x
  )

  invisible(ret)
}
