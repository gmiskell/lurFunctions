#' Calibration Interpolation Over Time.
#'
#' A function for converting the linear coefficients over time to be weighted based on its proximity to one of the calibrations. The output is the corrected obs value.
#' @param obs The column under investigation.
#' @param first.offset The intercept from the first calibration.
#' @param second.offset The intercept from the second calibration.
#' @param first.slope The slope from the first calibration.
#' @param second.slope The slope from the second calibration.
#' @export
#' @examples
#' changingCalFUN()


changingCalFUN <- function(x, obs, first.offset, second.offset, first.slope, second.slope){

  x <- as.data.frame(x)
  x$obs <- x[, obs];
  x$first.offset <- x[, first.offset]
  x$first.slope <- x[, first.slope]
  x$second.offset <- x[, second.offset]
  x$second.slope <- x[, second.slope]

  # determine the monitoring length based on the length of the obs
  monitoring_length <- length(x$obs)
  x$seq <- seq(1:monitoring_length)

  # corrected obs value
  x$cal_obs <- (x$first.offset + x$seq*((x$second.offset - x$first.offset)/monitoring_length) + (x$first.slope + x$seq*((x$second.slope - x$first.slope)/monitoring_length))*x$obs)
  x$seq <- NULL
  x$first.offset <- NULL
  x$second.offset <- NULL
  x$first.slope <- NULL
  x$second.slope <- NULL
   x
}
