#' A function for performing leave-one-out cross-validation
#'
#' This function removes one observation at a time and reruns the model in order to test model parameter stability.
#' @param sites The variable that identifies the relational element.
#' @param response The response variable.
#' @param variables A string of explanatory covariates in the model.
#' @export
#' @examples
#' data(iris)
#' loocvFUN(iris, sites = 'Species', response = 'Petal.Width', variables = 'Sepal.Length+Sepal.Width')


loocvFUN <- function(x, sites, response, variables) {
    
    # load required packages
    library(stringr)
  
    frml <- str_c(response,'~', variables)
    sites <- x[, sites]
    
    a <- unlist(sapply(seq(1,nrow(x)), function(i) {         
      training = x[-i,]
      test = x[i,]
      
      fit = lm(frml, training)
      
      adjR2 <- summary(fit)$adj.r.square
      RMSE <- sqrt(mean((training[, response]-fitted(fit))^2))
    
      z <- c(adjR2, RMSE)
      
    }))
    
    b <- data.frame(sites, t(a))
    names(b) <- c('site', 'adj.R2', 'RMSE')
    
    return(b)
}