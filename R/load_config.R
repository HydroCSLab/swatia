#' Load a SWATIA configuration file
#'
#' Reads a SWATIA configuration file from disk and returns a validated
#' configuration object for use in calibration or simulation workflows.
#'
#' @param path Character. Path to a SWATIA configuration file to be read and
#'   parsed.
#'
#' @return List. A SWATIA configuration object containing model, parameter, and
#'   calibration settings.
#' @export
load_config <- function(path) {
  validate_par <- function(par) {
    allowed_change_types <- c("absval", "abschg", "pctchg", "relchg")

    change_types <- vapply(par, `[[`, character(1), "change_type")
    bad <- which(!change_types %in% allowed_change_types)
    if (length(bad)) {
      stop(
        "Invalid change_type for: ",
        paste(names(change_types)[bad], collapse = ", "),
        ". Allowed: ",
        paste(allowed_change_types, collapse = ", ")
      )
    }

    rng_ok <- vapply(
      par,
      function(p) is.numeric(p$range) && length(p$range) == 2,
      logical(1)
    )
    if (any(!rng_ok)) {
      stop(
        "Invalid range for: ",
        paste(names(rng_ok)[!rng_ok], collapse = ", "),
        ". `range` must be numeric length 2."
      )
    }

    invisible(TRUE)
  }

  validate_interval <- function(interval) {
    allowed_intervals <- c("daily", "monthly", "yearly")
    if (!interval %in% allowed_intervals) {
      stop(
        "Invalid interval: ",
        interval,
        ". Allowed: ",
        paste(allowed_intervals, collapse = ", ")
      )
    }
    invisible(TRUE)
  }

  # Expects config.R to assign `config <- list(...)`
  e <- new.env(parent = baseenv())
  sys.source(path, envir = e)
  if (!exists("config", envir = e, inherits = FALSE)) {
    stop("Config file must define object `config`")
  }
  config <- get("config", envir = e, inherits = FALSE)

  validate_par(config$par)
  validate_interval(config$interval)

  config
}
