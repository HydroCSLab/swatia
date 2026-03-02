#' Get best optimization record
#'
#' Reads the objective log file and returns the best record.
#'
#' @param obj_day_txt Character. Path to objective log file.
#'
#' @return Data frame containing the best record.
#' @export
get_best <- function(obj_day_txt) {
  obj_day <- utils::read.csv(obj_day_txt)
  best_obj_day_c <- min(obj_day$obj_day_c)
  obj_day[which(obj_day$obj_day_c == best_obj_day_c), ]
}

#' Get best objective value
#'
#' Returns the minimum objective value from the log file.
#'
#' @param obj_day_txt Character. Path to objective log file.
#'
#' @return Numeric scalar.
#' @export
get_best_obj <- function(obj_day_txt) {
  get_best(obj_day_txt)$obj_day_c
}

#' Get best normalized parameter vector
#'
#' Returns the normalized parameter vector corresponding to the best objective
#' value.
#'
#' @param obj_day_txt Character. Path to objective log file.
#'
#' @return Numeric vector.
#' @export
get_best_x <- function(obj_day_txt) {
  best <- get_best(obj_day_txt)
  best[, startsWith(names(best), "x")]
}

#' Get best physical parameter values
#'
#' Converts the best normalized parameter vector into physical parameter values
#' using the configuration bounds.
#'
#' @param config List. SWATIA configuration object.
#' @param obj_day_txt Character. Path to objective log file.
#'
#' @return Named numeric vector of parameter values.
#' @export
get_best_par <- function(config, obj_day_txt) {
  best_x <- get_best_x(obj_day_txt)
  par_min <- sapply(config$par, function(x) x$range[1])
  par_max <- sapply(config$par, function(x) x$range[2])
  par_val <- par_min + (par_max - par_min) * best_x
  names(par_val) <- names(config$par)
  par_val
}
