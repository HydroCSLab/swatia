#' Update SWAT+ calibration file
#'
#' Modifies the SWAT+ calibration file in the specified directory using
#' the provided parameter values.
#'
#' @param dir Character. Path to a SWAT+ TxtInOut_* directory.
#' @param par_val Numeric vector. Parameter values to write into the
#'   calibration file.
#'
#' @return Invisibly returns NULL.
#' @keywords internal
update_calibration_cal <- function(dir, par_val) {
  path <- file.path(dir, "calibration.cal")
  lines <- readLines(path)
  for (par_name in names(par_val)) {
    idx <- grep(paste0("^\\s*", par_name, "\\s"), lines)
    lines[idx] <- sub(
      "^(\\s*(?:\\S+\\s+){2})\\S+(.*)$",
      paste0("\\1", par_val[[par_name]], "\\2"),
      lines[idx],
      perl = TRUE
    )
  }
  writeLines(lines, path)
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

#' Extract simulated discharge
#'
#' Reads simulated discharge values for a given channel ID from a SWAT+
#' output directory.
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

#' Evaluate SWAT+ objective function
#'
#' Updates parameters, runs SWAT+, extracts simulated discharge,
#' computes calibration and validation objective values, and records results.
#'
#' @param x Numeric vector. Normalized parameter values in [0, 1].
#' @param opt List. Runtime options (e.g., worker ID, iteration, run number).
#' @param config List. SWATIA configuration object.
#'
#' @return Numeric scalar. Calibration objective value.
#' @keywords internal
run_swatplus <- function(x, opt, config) {
  chaid <- sprintf("cha%03d", config$chaid)

  x[x < 0] <- 0
  x[x > 1] <- 1
  par_min <- sapply(config$par, `[`, 1)
  par_max <- sapply(config$par, `[`, 2)
  par_val <- par_min + (par_max - par_min) * x

  dir <- sprintf("TxtInOut_%d", opt$worker_id)
  update_calibration_cal(dir, par_val)
  run_swatplus_in_dir(dir)
  sim_day <- extract_sim(dir, chaid)
  sim_day_c <- sim_day[1:config$nobs_day_c]
  sim_day_v <- sim_day[(config$nobs_day_c + 1):config$nobs_day]
  obj_day_c <- config$calc_obj(config$obs_day_c, sim_day_c)
  obj_day_v <- config$calc_obj(config$obs_day_v, sim_day_v)

  x <- paste(x, collapse = ",")
  utils::write.table(
    sprintf("%d,%f,%f,%s", opt$run, obj_day_c, obj_day_v, x),
    config$obj_day_txt,
    quote = FALSE,
    row.names = FALSE,
    col.names = FALSE,
    append = TRUE
  )

  sim_day <- utils::read.table(sprintf("%s/sim_day.txt", dir))[[1]]
  utils::write.table(
    sim_day,
    sprintf("%s/sim_%05d_day.txt", config$sim_dir, opt$run),
    row.names = FALSE,
    col.names = FALSE
  )

  obj_day_c
}

#' Print named list
#'
#' Prints elements of a named list in a readable format.
#'
#' @param x List.
#'
#' @return Invisibly returns NULL.
#' @keywords internal
print_list <- function(x) {
  cat(paste(names(x), unlist(x), sep = " = "), sep = "\n")
}

#' Reset directories
#'
#' Removes and recreates specified directories.
#'
#' @param paths Character vector. Directory paths.
#'
#' @return Invisibly returns NULL.
#' @keywords internal
reset_dir <- function(paths) {
  for (path in paths) {
    if (file.exists(path)) {
      unlink(path, recursive = TRUE, force = TRUE)
    }
    dir.create(path, recursive = TRUE)
  }
}

#' Copy directory
#'
#' Copies contents of a source directory to a destination directory.
#'
#' @param src Character. Source directory.
#' @param dst Character. Destination directory.
#'
#' @return Invisibly returns NULL.
#' @keywords internal
copy_dir <- function(src, dst) {
  if (file.exists(dst)) {
    unlink(dst, recursive = TRUE, force = TRUE)
  }
  dir.create(dst)
  stopifnot(file.copy(
    list.files(src, full.names = TRUE),
    dst,
    recursive = TRUE,
    copy.date = TRUE
  ))
}

#' Run ISPSO calibration
#'
#' Runs the ISPSO optimization algorithm for SWAT+ parameter calibration
#' using the provided configuration.
#'
#' @param config List. SWATIA configuration object.
#' @param best_x Numeric vector, optional. Initial parameter vector for
#'   warm start.
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
#' Returns the normalized parameter vector corresponding to the best
#' objective value.
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
#' Converts the best normalized parameter vector into physical parameter
#' values using the configuration bounds.
#'
#' @param config List. SWATIA configuration object.
#' @param obj_day_txt Character. Path to objective log file.
#'
#' @return Named numeric vector of parameter values.
#' @export
get_best_par <- function(config, obj_day_txt) {
  best_x <- get_best_x(obj_day_txt)
  par_min <- sapply(config$par, `[`, 1)
  par_max <- sapply(config$par, `[`, 2)
  par_val <- par_min + (par_max - par_min) * best_x
  names(par_val) <- names(config$par)
  par_val
}

#' SWATIA command-line interface
#'
#' Entry point for the SWATIA command-line interface. Parses command-line
#' arguments and dispatches subcommands.
#'
#' @param args Character vector. Command-line arguments. Defaults to
#'   \code{commandArgs(trailingOnly = TRUE)}.
#'
#' @return Invisibly returns NULL.
#' @export
swatia <- function(args = commandArgs(trailingOnly = TRUE)) {
  print_usage <- function(status = 1L) {
    cat(
      "swatia [options] <command> [args]

Options:
  -c, --config=PATH    Path to configuration file (default: ./config.R)
  -h, --help           Show this help message

Commands:
  run_ispso
  get_best_obj [obj_day_txt]
  get_best_x   [obj_day_txt]
  get_best_par [obj_day_txt]
\n"
    )
    status
  }

  parse_args <- function(args) {
    # Accept: --config=PATH or --config PATH
    config <- NULL
    i <- 1
    while (i <= length(args)) {
      a <- args[[i]]
      if (startsWith(a, "--config=")) {
        config <- sub("^--config=", "", a)
        args <- args[-i]
        next
      }
      if (a == "-c") {
        if (i == length(args)) {
          stop("Missing value after -c")
        }
        config <- args[[i + 1]]
        args <- args[-c(i, i + 1)]
        next
      }
      if (a == "--config") {
        if (i == length(args)) {
          stop("Missing value after --config")
        }
        config <- args[[i + 1]]
        args <- args[-c(i, i + 1)]
        next
      }
      i <- i + 1
    }

    if (is.null(config)) {
      if (file.exists("config.R")) {
        message("Using default config: ./config.R")
        config <- "config.R"
      } else {
        stop("Missing required option: -c or --config=PATH")
      }
    }

    if (length(args) < 1) {
      stop("Missing subcommand")
    }
    cmd <- args[[1]]
    rest <- args[-1]

    list(config = config, cmd = cmd, rest = rest)
  }

  load_config <- function(path) {
    # Expects config.R to assign `config <- list(...)`
    e <- new.env(parent = baseenv())
    sys.source(path, envir = e)
    if (!exists("config", envir = e, inherits = FALSE)) {
      stop("Config file must define object `config`")
    }
    get("config", envir = e, inherits = FALSE)
  }

  if (length(args) == 0 || any(args %in% c("-h", "--help"))) {
    return(print_usage(0L))
  }

  pa <- parse_args(args)
  config <- load_config(pa$config)

  if (length(pa$rest) > 1) {
    stop("Too many arguments")
  }

  if (pa$cmd == "run_ispso") {
    best_par_txt <- if (length(pa$rest) >= 1) pa$rest[[1]] else NULL
  } else {
    obj_day_txt <- if (length(pa$rest) >= 1) {
      pa$rest[[1]]
    } else {
      config$obj_day_txt
    }
  }

  status <- switch(
    pa$cmd,
    run_ispso = {
      if (is.null(best_par_txt)) {
        best_x <- NULL
      } else {
        best_par <- utils::read.table(
          best_par_txt,
          sep = "=",
          strip.white = TRUE
        )
        colnames(best_par) <- c("par", "val")
        best_x <- c()
        for (par in names(config$par)) {
          val <- best_par[best_par$par == par, "val"]
          rng <- config$par[[par]]
          best_x <- c(best_x, (val - rng[1]) / (rng[2] - rng[1]))
        }
        best_x <- matrix(best_x, 1)
      }
      run_ispso(config, best_x)
      0L
    },
    get_best_obj = {
      cat(get_best_obj(obj_day_txt), sep = "\n")
      0L
    },
    get_best_x = {
      print_list(get_best_x(obj_day_txt))
      0L
    },
    get_best_par = {
      print_list(get_best_par(config, obj_day_txt))
      0L
    },
    {
      stop(sprintf("Unknown subcommand: %s", pa$cmd))
    }
  )

  invisible(status)
}
