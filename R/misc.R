#' Descriptive statistics
#'
#' An old function I used to explore data. Has to be rewritten and documented.
#'
#' @param x .
#' @param name .
#' @param histogram .
#' @param barplot .
#' @param densityplot .
#' @param savePDF .
#' @param noPlot .
#'
#' @return .
#'
#' @export
descriptives <- function(x, name="", histogram = TRUE, barplot = FALSE,
                         densityplot = TRUE, savePDF = FALSE, noPlot = FALSE) {
  require(Hmisc)
  require(psych)
  print(Hmisc::describe(x))
  print(psych::describe(x))
  # if (noPlot) {} else {
  #   drawDistribution(x, label(x), histogram=histogram, barplot=barplot, densityplot=densityplot, savePDF=savePDF, filename=name)
  # }
}

#' Ordinal alpha
#'
#' Calculates the ordinal alpha by using polychoric correlation (see Zumbo et
#' al. 2007).
#'
#' @param x A set of variables (either a data frame or a matrix).
#'
#' @return Cronbach's Alpha for ordinal data.
#'
#' @export
#'
#' @references Zumbo, B. D., Gadermann, A. M., & Zeisser, C. (2007). Ordinal Versions of Coefficients Alpha and Theta for Likert Rating Skales. Journal of Modern Applied Statistical Methods, 6(1), 21–29.
ordinal_alpha <- function(x){
  require(psych)
  psych::alpha(polychoric(x)$rho)
}
