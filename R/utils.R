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
