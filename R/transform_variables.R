#' Inverts the variable
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