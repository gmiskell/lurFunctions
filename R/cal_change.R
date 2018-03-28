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


changingCalFUN <- function(x, obs, a1, b1, a2, b2){
  
  x <- as.data.frame(x)
  x$obs <- x[, obs]
  
  # determine the monitoring length based on the length of the obs
  monitoring_length <- length(x$obs)
  x$seq <- seq(1:monitoring_length)
  
  # corrected obs value
  x$cal_obs <- (first.offset + x$seq*((second.offset - first.offset)/monitoring_length) + (first.slope + x$seq*((second.slope - first.slope)/monitoring_length))*x$obs)
  x$seq <- NULL
 
   return(x)
}
