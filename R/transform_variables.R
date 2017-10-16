#' Invert a variable
#'
#' invert_variable returns the variable with an inverted scale. The heighest
#' possible value for a variable can be provided. If max_value is not provided,
#' the maximal value is computed from x.
#'
#' For a variable with values between 1 and 5, 1 will become 5, 2 will become 4,
#' 1 will become 5, and so on.
#'
#' @param x A numeric variable.
#' @param max_value Optional, the heighest possible value on the variable's
#' scale.
#'
#' @return The same variable with an inverted scale.
#'
#' @export
invert_variable <- function(x, max_value) {
  if (missing(max_value)) {
    max_value <- max(x, na.rm = TRUE)
  }
  x_inv <- max_value + 1 - x
  # label
  lbl <- attr(x, which = "label", exact = TRUE)
  if (!is.null(lbl)) {
    attr(x_inv, which = "label") <- paste0(lbl, " (inverted)")
  }
  return(x_inv)
}


#' Rescale a variable
#'
#' rescale_variable rescales a variable from an old range of values (min_old to
#' max_old) to a new range (min_new to max_new). Default behavior is to rescale
#' a variable to a new range from 0 to 1 where the minimum observed value is
#' recoded to 0 and the maximum observed value is recoded to 1.
#'
#' @param x A numeric variable.
#' @param min_old The old minimum value for the variable. Default is
#'                the variable's minimum value.
#' @param min_new The new minimum value, defaults to 0.
#' @param max_old The old maximum value for the variable. Default is the
#'                variable's maximum value.
#' @param max_new The new maximum value, defaults to 1.
#'
#' @return The rescaled variable.
#'
#' @export
rescale_variable <- function(x, min_old, min_new = 0, max_old, max_new = 1) {
  if (missing(min_old)) {
    min_old <- min(x, na.rm = TRUE)
  }
  if (missing(max_old)) {
    max_old <- max(x, na.rm = TRUE)
  }
  x_rescaled <- (max_new-min_new)/(max_old-min_old)*(x-max_old)+max_new
  return(x_rescaled)
}


#' Get a user's home directory
#'
#' home_dir returns a path to the user's home directory. It replaces "~" and
#' should work in both RStudio and RScript as well as on different OS.
#'
#' @return The path to the users home directory as a string.
#'
#' @export
home_dir <- function() {
  return(normalizePath(file.path(Sys.getenv("HOMEDRIVE"),
                                 Sys.getenv("HOMEPATH")),
                       winslash = .Platform$file.sep))
}
