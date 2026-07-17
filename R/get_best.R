#' Get best optimization record
#'
#' Reads the objective log file and returns the best record.
#'
#' @param obj_txt Character. Path to objective log file.
#'
#' @return Data frame containing the best record.
#' @export
get_best <- function(obj_txt) {
  obj <- utils::read.csv(obj_txt)
  best_obj_c <- min(obj$obj_c)
  obj[which(obj$obj_c == best_obj_c), ]
}

#' Get best objective value
#'
#' Returns the minimum objective value from the log file.
#'
#' @param obj_txt Character. Path to objective log file.
#'
#' @return Numeric scalar.
#' @export
get_best_obj <- function(obj_txt) {
  get_best(obj_txt)$obj_c
}

#' Get best normalized parameter vector
#'
#' Returns the normalized parameter vector corresponding to the best objective
#' value.
#'
#' @param obj_txt Character. Path to objective log file.
#'
#' @return Numeric vector.
#' @export
get_best_x <- function(obj_txt) {
  best <- get_best(obj_txt)
  best[, startsWith(names(best), "x")]
}

#' Get best physical parameter values
#'
#' Converts the best normalized parameter vector into physical parameter values
#' using the configuration bounds.
#'
#' @param config List. SWATIA configuration object.
#' @param obj_txt Character. Path to objective log file.
#'
#' @return Named numeric vector of parameter values.
#' @export
get_best_par <- function(config, obj_txt) {
  best_x <- get_best_x(obj_txt)
  par_min <- sapply(config$par, function(x) x$range[1])
  par_max <- sapply(config$par, function(x) x$range[2])
  par_val <- par_min + (par_max - par_min) * best_x
  names(par_val) <- names(config$par)
  par_val
}
