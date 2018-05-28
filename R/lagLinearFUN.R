#' Linear Model Using Last n Lags as the Independent Variable.
#' 
#' A function that uses the average of the previous n values as the independent variable in a linear regression model.
#' @param obs The column under investigation.
#' @param lags The number of lags to go back and average over.
#' @export
#' @examples
#' lagLinearFUN()


lagLinearFUN <- function(x, obs, lags = 3){
  
  library(tidyverse)
  
  # create lag in data
  rep.NA <- rep(NA, lags)
  lag.n <- x$obs
  lag.x <- c(rep.NA, lag.n)
  
  # create the n columns from which the average is derived
  lag.x.emb <- embed(lag.x, n)
  lag.x.av <- rowMeans(lag.x.emb, na.rm = F)
  lag.av <- lag.x.av[1:nrow(x)]
  
  # combine with original dataframe
  x <- cbind(x, lag.av)
  
  # run model
  lag.lm <- summary(lm(obs ~ lag.av, data = x))
  
  return(data.frame(lags = lags, offset = lag.lm$coefficients[1],
                    slope = lag.lm$coefficients[2], r2 = lag.lm$r.squared))
}


  
  
  
  
  
  
  
  
