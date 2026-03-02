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
  obs_day <- utils::read.table(config$obs_day_txt)[[1]]

  config$nobs_day <- length(obs_day)
  stopifnot(config$nobs_day_c < config$nobs_day)

  config$obs_day_c <- obs_day[1:config$nobs_day_c]
  config$obs_day_v <- obs_day[(config$nobs_day_c + 1):config$nobs_day]

  reset_dir(config$sim_dir)
  unlink(Sys.glob("TxtInOut_*"), recursive = FALSE, force = TRUE)

  nworkers <- min(config$control$S, parallelly::availableCores())
  ndim <- length(config$par)

  for (i in seq_len(nworkers)) {
    copy_dir(config$txtinout, sprintf("TxtInOut_%d", i))
  }

  utils::write.table(
    sprintf(
      "run,obj_day_c,obj_day_v,%s",
      paste(paste("x", 1:ndim, sep = ""), collapse = ",")
    ),
    config$obj_day_txt,
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

  #  parallel::clusterApply(cl, seq_along(cl), function(id) {
  #    assign("worker_id", id, envir = ispso::.ispso_state)
  #    NULL
  #  })
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
