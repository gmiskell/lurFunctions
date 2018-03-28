#' A Segmented Regression Function.
#'
#' A function for finding a number of linear fits within a dataset.
#' @param obs This is the numeric column under investigation.
#' @param comparison.value This is the data to compare the obs against.
#' @param date Date column.
#' @param min.length This is the length of the sample to draw the mean and variance from.
#' @param break.points Number of break points within the data (will make break.point + 1 regressions).
#' @param plot Option to create a plot with the break points. Defaults to `FALSE`.
#' @export
#' @examples
#' segmentFUN()


segmentFUN <- function(x, obs, comparison.value, date, min.length, break.points, plot = FALSE){
  
  # load required libraries
  library(tidyverse);library(segmented)
  
  # define selected variables
  x <- as.data.frame(x)
  x$date <- lubridate::ymd_hms(x$date)
  
  # assign variable names and filter data to the set dates
  x$obs <- x[, obs]
  x$comparison.value <- x[, comparison.value]
  
  # select the required columns
  x <- x %>% dplyr::select(date, obs, comparison.value)
  
  # remove any repeating and empty rows
  x <- unique.data.frame(x)
  
  # find the initial break-points and set the model
  original.breaks <- break.points
  break.points <- original.breaks + 1
  break.points <- min.length/break.points * seq(break.points)
  break.points <- break.points[-length(break.points)]
  # remove break points where no mins data available
  break.points <- break.points[break.points %in% x$comparison.value]
  lm.model <- lm(obs ~ comparison.value, data = x)
  
  # segmented regression
  comparison.value = NULL
  if.false <- F
  # segmented regression can occassionally spit out errors, below will loop until no error is found.
  while(if.false == F){
    tryCatch({
      s <- segmented(lm.model, seg.Z =~comparison.value, psi = break.points)
      if.false <- T
      }, error = function(e){
      }, finally = {})
    }
  s.names <- c(row.names(s$psi), NA)
  s.breaks <- c(round(s$psi[,2]), NA)
  s.intercept <- data.frame(intercept(s))[,1]
  s.slope <- data.frame(slope(s))[,1]
  s.df <- data.frame(s.names, s.breaks, s.intercept, s.slope)
  
  if(plot == TRUE){
    plot.segmented(s, res = T, conf.level = 0.95, dens.rug = T)
    text(x = s.df$s.breaks, y = s.df$Est. + s.df$s.slope * s.df$s.breaks,label = s.df$s.breaks, col = 'red')
  }
  return(s.df)
  }

