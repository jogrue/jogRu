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


#' Compute confidence interval
#'
#' Computes the confidence interval based on Student's t-distribution or the
#' standard normal distribution. Also reports descriptive statistics (such as
#' the mean). Missings are ignored.
#'
#' @param x A numeric (or logical) vector (such as a variable)
#' @param conf.level Confidence level of the interval. Defaults to 0.95.
#' @param dist Distribution the confidence interval should be based on. "t" or
#' "student" (default) for Student's t-distribution, "z" or "normal" for
#' standard normal distribution.
#' @param incl_n A locigal indicating whether the number of observations should
#' be included in the returned results. Defaults to TRUE.
#' @param incl_mean A locigal indicating whether the mean should
#' be included in the returned results. Defaults to TRUE.
#' @param incl_SD A locigal indicating whether the standard deviation should
#' be included in the returned results. Defaults to TRUE.
#' @param incl_SE A locigal indicating whether the standard error should
#' be included in the returned results. Defaults to TRUE.
#' @param incl_error A locigal indicating whether the error based on the
#' confidence level (i.e., the number that is substracted or added to the mean
#' lower and upper bounds of the confidence interval) should
#' be included in the returned results. Defaults to TRUE.
#'
#' @return A numeric vector with at least the lower and upper bounds of the
#' confidence interval.
#' @export
#'
#' @examples
#' ## The height of ten people in cm
#' heights <- c(160, 163, 168, 169, 174, 176, 177, 178, 182, 190)
#'
#' get_CI(heights) # Defaults
#' get_CI(heights, dist = "normal") # Using standard normal distribution
#' get_CI(heights, conf.level = 0.9) # 90%-CI
#' get_CI(heights, incl_SD = FALSE) # Do not include standard deviation
get_CI <- function(x, conf.level = 0.95, dist = "t",
                   incl_n = TRUE, incl_mean = TRUE, incl_SD = TRUE,
                   incl_SE = TRUE, incl_error = TRUE) {
  if (!is.numeric(x) && !is.complex(x) && !is.logical(x)) {
    warning("Argument x is not numeric or logical: returning NA")
    return(NA_real_)
  }
  dist <- pmatch(dist, table = c("t", "student", "z", "normal"), nomatch = NA)
  if (is.na(dist)) {
    warning('Argument dist was not "t" or "student" for Student\'s t-distribution, nor "z" or "normal" for the z-distribution. Student\'s t-distribution was used as a default.')
    dist <- 1
  }
  zdist <- (dist > 2)
  x <- x[!is.na(x)]
  if (length(x) <= 1) {
    warning("Too few valid (i.e., not NA) values: returning NA")
    return(NA_real_)
  }
  n <- length(x)
  mean <- mean(x)
  sd <- sd(x)
  se <- sd/sqrt(n)
  if (zdist) {
    error <- qnorm(p = (1+conf.level)/2)*se # Same as 1-((1-conf.level)/2)
  } else {
    error <- qt(p = (1+conf.level)/2, df = n-1)*se
  }
  lower <- mean-error
  upper <- mean+error
  result <- c(n = n, mean = mean, SD = sd, SE = se, error = error, lower = lower,
              upper = upper)
  report <- c(incl_n, incl_mean, incl_SD, incl_SE, incl_error, TRUE,
              TRUE)
  result <- result[report]
  return(result)
}
