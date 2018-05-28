#' A function for developing and refining land use regression models
#'
#' This function develops suitable linear models for a predictor. Before using the function, checks need to be made on the univariate direction of effects as these need to be subjectively determined.
#' @param resp The response column to be used in the model.
#' @param method The LUR development method (`caret` method using package = DEFAULT, or `escape` method using AQ standard).
#' @export
#' @examples
#' data(iris)
#' lurBuildFUN(iris, resp = 'Species', method = 'caret')


lurBuildFUN <- function(x, resp, method = 'caret'){
  
  # install and load required packages
  library(tidyverse); library(caret)
  
  # load required functions
  round_df <- function(df, digits){
    nums <- vapply(df, is.numeric, FUN.VALUE = logical(1))
    df[,nums] <- round(df[,nums], digits = digits)
    (df)
    }
  
  # clause on method type -- either 'caret' (default) or 'escape'
  if(method == 'caret'){
    modelFun <- function(x, list = 'response'){
      # make response and predictors into separate files
      x$response <- x[, resp]
      response <- x$response
      x[, resp] <- NULL
      predictor <- x[, ! names(x) %in% list]
      
      # identify correlated predictors and remove
      makenumeric <- sapply(predictor, as.numeric)
      makenumeric[is.na(makenumeric)] <- 999
      correlation <- cor(makenumeric, use = 'pairwise.complete.obs')
      high.cor <- findCorrelation(correlation, cutoff = 0.6)
      if(length(high.cor > 0)){ predictor <- predictor[, -high.cor]}
      
      # create data frame of the univariate R2 values, to determine coefficient direction
      coef.all <- lapply(predictor, function(x) summary(lm(response ~ x))$coefficients)
      coef.sub1 <- ldply(coef.all, data.frame)
      coef.sub2 <- lapply(coef.all, rownames)
      coef.sub3 <- ldply(coef.sub2, data.frame)
      coef.sub3$names <- str_c(coef.sub3$.id, str_sub(coef.sub3$X..i.., start = 2))
      coef.all <- data.frame(var = coef.sub3$.id, names = coef.sub3$names, coef = coef.sub1$Estimate, pvalue = coef.sub1$Pr...t..)
      coef.all <- round_df(coef.all, digits = 4)
      coef.all$direction <- ifelse(coef.all$coef > 0, 1, 0)
      coef.all <- coef.all[!grepl("Int", coef.all$names),]
      
      # model development
      n.length <- as.integer(ncol(x)/2); predictor$response <- response+control <- trainControl(method = 'repeatedcv', number = n.length, repeats = 5)
      modelFit1 <- train(response ~ ., data = predictor, method = 'lm', trControl = control, metric = 'RMSE')
      
      # model evaluation
      ## coefficient direction
      coef.length <- length(modelFit1$coefnames) + 1
      coef.df <- data.frame(names = modelFit1$coefnames, coef = summary(modelFit1)$coefficients[2:coef.length], pvalue = summary(modelFit1)$coefficients[(3*coef.length+2):(4*coef.length)])
      coef.df$direction <- ifelse(coef.df$coef > 0, 1, 0)
      coef.all.sub <- coef.df[which(coef.all$names %in% coef.df$names),]
      coef.all.sub$change <- coef.all.sub$direction - coef.all$direction
      coef.all.sub$directionchange <- ifelse(coef.all.sub$change == 0, TRUE, FALSE)
      return(coef.all.sub)
      }
    
    # run modelFun and check that the conditions are met
    run <- modelFun(x)
    
    while(any(run$directionchange == FALSE)){
      sub <- run[which(run$directionchange == FALSE),]
      sub.select <- sub %>%
        dplyr::arrange(desc(pvalue)) %>%
        slice(1)
      sub.select <- as.character(sub.select$var)
      newlist <- c(list, sub.select)	
      run <- modelFun(x, list = newlist)
    }
    }

  # escape method if selected
  if(method == 'escape'){
    modelFun <- function(x, list = 'response'){
      # make response and predictors into separate files
      x$response <- x[, resp]
      response <- x$response
      x[, resp] <- NULL
      predictor <- x[, ! names(x) %in% list]
      
      # identify correlated predictors and remove
      makenumeric <- sapply(predictor, as.numeric)
      makenumeric[is.na(makenumeric)] <- 999
      correlation <- cor(makenumeric, use = 'pairwise.complete.obs')
      high.cor <- findCorrelation(correlation, cutoff = 0.6)
      if(length(high.cor > 0)){ predictor <- predictor[, -high.cor]}
      
      # create data frame of the univariate values, to determine coefficient direction
      coef.all <- lapply(predictor, function(x) summary(lm(response ~ x))$coefficients)
      coef.sub1 <- ldply(coef.all, data.frame)
      coef.sub2 <- lapply(coef.all, rownames)
      coef.sub3 <- ldply(coef.sub2, data.frame)
      coef.sub3$names <- str_c(coef.sub3$.id, str_sub(coef.sub3$X..i.., start = 2))
      coef.all <- data.frame(var = coef.sub3$.id, names = coef.sub3$names, coef = coef.sub1$Estimate, pvalue = coef.sub1$Pr...t..)
      coef.all <- round_df(coef.all, digits = 4)
      coef.all$direction <- ifelse(coef.all$coef > 0, 1, 0)
      coef.all <- coef.all[!grepl("Int", coef.all$names),]
      
      # build model
      r.sq <- ldply(predictor, function(x) summary(lm(response ~ x))$r.squared)
      max.pred <- r.sq$.id[which.max(r.sq$V1)]
      max.r2 <- r.sq$V1[r.sq$.id == max.pred]
      add.var <- c(max.pred)
      expl.df <- predictor[, names(predictor) %in% add.var]
      expl.list <- names(expl.df)
      model <- str_c('response~', paste0('x$', expl.list, collapse = '+'), '+x$', names(predictor))
      lm.func <- function(x) {summary(lm(x))}
      nxt.step <- data.frame(variable = colnames(predictor), adjR2 = ldply(model, function(predictor) lm.func(predictor)$adj.r.squared))
      nxt.step <- nxt.step[which(! nxt.step$variable %in% add.var),]
      if(max(nxt.step$adjR2 > max.r2)){
        nxt.pred <- nxt.step$variable[which.max(nxt.step$adjR2)]
        nxt.r2 <- nxt.step$adjR2[nxt.step$variable == nxt.pred]		
        add.var <- c(add.var, nxt.pred)
        }
      
      # model evaluation
      ## coefficient direction
      coef.length <- length(modelFit1$coefnames) + 1
      coef.df <- data.frame(names = modelFit1$coefnames, coef = summary(modelFit1)$coefficients[2:coef.length], pvalue = summary(modelFit1)$coefficients[(3*coef.length+2):(4*coef.length)])
      coef.df$direction <- ifelse(coef.df$coef > 0, 1, 0)
      coef.all.sub <- coef.df[which(coef.all$names %in% coef.df$names),]
      coef.all.sub$change <- coef.all.sub$direction - coef.all$direction
      coef.all.sub$directionchange <- ifelse(coef.all.sub$change == 0, TRUE, FALSE)
      return(coef.all.sub)
      }
    
    # run modelFun and check that the conditions are met
    run <- modelFun(x)
    
    while(any(run$directionchange == FALSE) | max(nxt.step$adjR2 > nxt.r2)){
      sub <- run[which(run$directionchange == FALSE),]
      sub.select <- sub %>%
        dplyr::arrange(desc(pvalue)) %>%
        slice(1)
      sub.select <- as.character(sub.select$var)
      newlist <- c(list, sub.select)	
      run <- modelFun(x, list = newlist)
    }
  }
}	   
