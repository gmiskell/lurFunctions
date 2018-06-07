#' A Segmented Regression Function.
#'
#' A function for finding a number of linear fits within a dataset.
#' @param obs This is the numeric column under investigation.
#' @param comparison.value This is the data to compare the obs against.
#' @param date Date column.
#' @param min.length This is the length of the comparison value sample to draw the break points from.
#' @param break.points Number of break points within the data (will make break.point + 1 regressions).
#' @param plot Option to create a plot with the break points. Defaults to `FALSE`.
#' @param plot.title If plot is `TRUE` this command provides an option for giving the plot a title.
#' @export
#' @examples
#' segmentFUN()


segmentFUN <- function(x, obs, comparison.value, date, min.length, break.points, plot = FALSE, plot.title = NA){

  # load required libraries
  library(tidyverse);library(segmented)

  # define selected variables
  x <- as.data.frame(x)
  x$date <- lubridate::ymd_hms(x$date)

  # assign variable names and filter data to the set dates
  x$obs <- x[[obs]]
  x$comparison.value <- x[[comparison.value]]

  # select the required columns
  x <- x %>% dplyr::select(date, obs, comparison.value) %>% filter(!is.na(obs))

  # remove any repeating and empty rows
  x <- unique.data.frame(x)

  # find the initial break-points and set the model
  original.breaks <- break.points
  break.points <- original.breaks + 1
  break.points <- min.length/break.points * seq(break.points)
  break.points <- round(break.points)
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
    library(ggplot2)

    dat2 <- data.frame(x = x$comparison.value, y = broken.line(s)$fit)

    x$row.num <- seq(1:nrow(x))

    seg.plot <- ggplot(x, aes(x = comparison.value, y = obs))+
      theme_classic()+
      geom_point(aes(colour = desc(row.num)))+
      geom_line(data = dat2, aes(x = x, y= y), colour = 'red')+
      labs(title = plot.title)+
      geom_label(data = s.df, aes(label = s.breaks, x = s.breaks, y = s.intercept + s.slope * s.breaks), nudge_y = 2, alpha = 0.5, size = 8)+
      theme(legend.position = 'none')

    print(seg.plot)
  }
  return(seg.df)
}




