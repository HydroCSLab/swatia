#' Install swatia CLI into PATH
#'
#' Creates a symbolic link named \code{swatia} in the specified directory so
#' that the SWATIA CLI can be invoked from the command line. The target
#' directory must be included in the user's \code{PATH}.
#'
#' @param dir Character. Directory in which to create the \code{swatia}
#'   symlink. This directory should be included in the user's \code{PATH}.
#'
#' @export
install_swatia_cli <- function(
  dir = file.path(Sys.getenv("HOME"), ".local", "bin")
) {
  src <- system.file("cli", "swatia", package = "swatia")
  if (!nzchar(src)) {
    stop("Cannot find swatia script in installed package.")
  }

  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
  }

  dst <- file.path(dir, "swatia")
  if (file.exists(dst)) {
    unlink(dst)
  }

  ok <- file.symlink(src, dst)
  if (!isTRUE(ok)) {
    stop("Failed to create symlink: ", dst)
  }

  invisible(dst)
}
