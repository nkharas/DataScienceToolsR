###################################################
###################################################
# Author : Nick Kharas
###################################################
###################################################


###################################################
# MODEL DIAGNOSTICS - LINEAR REGRESSION
#
# Model Summary
# MSE
# Holdout R^2
# Holdout rank correlation between actual and predicted values
# Residual and normality plots
# Run normality test
###################################################

library(ggplot2)
library(ggfortify) # autoplot
library(lmtest) # bptest

lm.diagnostics <- function(model, test, pred){
    
  rank.df <- data.frame(cbind(rank(test[, ncol(test)])
                                , rank(pred)
                                , test[, ncol(test)]
                                , pred
  ))
  colnames(rank.df) <- c('actual_rank', 'pred_rank', 'actual', 'pred')
    
  mse <- mean( (rank.df$actual - rank.df$pred)^2 )
  cr <- cor(rank.df$actual, rank.df$pred)
    
  print("      ")
  print("Holdout MSE")
  print(mse)
    
  print("      ")
  print("Holdout R Squared")
  print(cr * cr)
    
  print("Test of Holdout R Squared")
  cr.test <- cor.test(rank.df$actual, rank.df$pred)
  print(cr.test)
    
  # Rank correlation
  rank.cr <- cor(rank.df$actual_rank, rank.df$pred_rank)
  
  print("      ")
  print("Holdout Rank Correlation")
  print(rank.cr)
  
  print("Test of Holdout Rank Correlation")
  rank.cr.test <- cor.test(rank.df$actual_rank, rank.df$pred_rank)
  print(rank.cr.test)

}

###################################################
# FORWARD STEPPING
#
# Model build with diagnostics
# Arrange the data frame such that the last column is the dependent variable
#   and that it has only those columns (or, features) on which you want to rain the model
# Test is the optional holdout dataset. If you don't have one, you won't be able to run holdout diagnostics.
# If you do provide test data, both train and test data should have exactly the same column structure.
###################################################

library(MASS) # StepAIC

forward.step <- function(train, test = data.frame(ints=integer())){
  fit <- lm(paste(colnames(train[ncol(train)])," ~ ."), data = train)
  model <- stepAIC(fit, direction = "forward")
  
  # Anova
  print("Anova")
  print(model$anova)
  
  sm <- summary(model)
  
  print("      ")
  print("Model Summary")
  print(sm)
  
  print("      ")
  print("Model MSE")
  print(mean(sm$residuals^2))
  
  print("      ")
  print("Residual and normality plots")
  print(autoplot(model))
  
  print("      ")
  print("Breusch-Pagan normality test")
  print(bptest(model))
  
  # Run model diagnostics against holdout if holdout sample is provided
  if(nrow(test) > 0){
    pred <- predict(model, test[, -ncol(test)])
    lm.diagnostics(model, test, pred)
  }
  
  return(model)
}


###################################################
# REGULARIZATION - LASSO, RIDGE, ELASTIC NET
#
# alpha = 0 -> Ridge regression
# aplha = 1 -> Lasso
#
# Approach:
#   0. Scale the data before training regularization models on them
#   1. Run model with 10 fold cross validation 
#   2. Find lambda with minimum MSE within 1 std deviation
#   3. Return model with the above selected lambda
#   4. Function uses default list of lambdas, but can be replaced
#   5. lambdas_to_try can be plugged in, but default lambdas are used.
#         Coder can plug in own set of lambdas.
###################################################

set.seed(255)

library(glmnet)

lambdas_to_try <- 10^seq(-5, 1, length.out = 1000)

reg.model <- function(train, alpha, test = data.frame(ints=integer()), lambdas = c()){
  
  if(length(lambdas) > 0){
    model.cv <- cv.glmnet(as.matrix(train[, -ncol(train)])
                          , train[, ncol(train)]
                          , alpha = alpha
                          , lambda = lambdas
                          , standardize = TRUE
                          , nfolds = 10)
  }else{
    model.cv <- cv.glmnet(as.matrix(train[, -ncol(train)])
                          , train[, ncol(train)]
                          , alpha = alpha
                          , standardize = TRUE
                          , nfolds = 10)
  }
  
  autoplot(model.cv)
  
  #lambda.cv <- model.cv$lambda.min
  lambda.cv <- model.cv$lambda.1se
  
  model <- glmnet(as.matrix(train[, -ncol(train)])
                  , train[, ncol(train)]
                  , alpha = alpha
                  , lambda = lambda.cv
                  , standardize = TRUE)
  
  if(nrow(test) > 0){
    pred <- predict(model, as.matrix(test[, -ncol(test)]))
    lm.diagnostics(model, test, pred)
  }
  
  return(model)
}
