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
  run-ispso    [best-par.txt]
  get-best-obj [obj.txt]
  get-best-x   [obj.txt]
  get-best-par [obj.txt]
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
    cmd <- chartr("-", "_", args[[1]])
    rest <- args[-1]

    list(config = config, cmd = cmd, rest = rest)
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
    obj_txt <- if (length(pa$rest) >= 1) {
      pa$rest[[1]]
    } else {
      config$obj_txt
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
          rng <- config$par[[par]]$range
          best_x <- c(best_x, (val - rng[1]) / (rng[2] - rng[1]))
        }
        best_x <- matrix(best_x, 1)
      }
      run_ispso(config, best_x)
      0L
    },
    get_best_obj = {
      cat(get_best_obj(obj_txt), sep = "\n")
      0L
    },
    get_best_x = {
      print_list(get_best_x(obj_txt))
      0L
    },
    get_best_par = {
      print_list(get_best_par(config, obj_txt))
      0L
    },
    {
      stop(sprintf("Unknown subcommand: %s", pa$cmd))
    }
  )

  invisible(status)
}
