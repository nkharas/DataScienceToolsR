###################################################
###################################################
# Author : Nick Kharas
###################################################
###################################################


###################################################
###################################################
# DATA TRANSFORMATIONS
#
# Log, inverse log, square root
#
# Each function has optional cols parameter to apply transformations
#     to some but not all columns of the dataframe
###################################################
###################################################

library(dplyr)

###################################################
# Return min value + threshold 
#   if the min value of the column is < threshold
#   , else return 0
###################################################
min.transform <- function(x, threshold = 0){
  if (min(x) < threshold){
    result <- abs(min(x)) + 1
  }
  else{
    result <- 0
  }
  return(result)
}

###################################################
# LOG TRANSFORM
#
# Log of a number < 0 return infinity. So, the threshold is 1.
# We make this adjustment to avoid taking log of values < 1 and avoid infinity
# Adding a fixed constant does not change the shape of the distribution of data
# If minimum value is >= 1, min_transform returns 0, thus retaining the original value of x
###################################################
log.transform <- function(x){
  result <- log(x + min.transform(x, 1))
  return(result)
}

# Apply above function to all columns in a data frame
# Return the transformed data frame
log.transform.df <- function(df, cols=c()){
  colsdf <- colnames(df)
  
  if(length(cols) > 0){
    df.log <- df %>% mutate_each_(funs(log.transform(.) %>% as.vector), vars=cols)
    names(df.log)[match(cols,colsdf)] <- paste(cols, '_log', sep = '')
  }
  else{
    df.log <- apply(df, 2, log.transform)
    colnames(df.log) <- paste(colsdf, '_log', sep = '')
  }
  
  df.log <- as.data.frame(df.log)
  return(df.log)
}

###################################################
# LOG TRANSFORM to the base 2
#
# Log transformations below 1 return infinity. So, the threshold is 1
# If minimum value is >= 1, min_transform returns 0, thus retaining the original value of x
###################################################
log2.transform <- function(x){
  result <- log2(x + min.transform(x, 1))
  return(result)
}

# Apply above function to all columns in a data frame
# Return the transformed data frame
log2.transform.df <- function(df, cols=c()){
  colsdf <- colnames(df)
  
  if(length(cols) > 0){
    df.log2 <- df %>% mutate_each_(funs(log2.transform(.) %>% as.vector), vars=cols)
    names(df.log2)[match(cols,colsdf)] <- paste(cols, '_log2', sep = '')
  }
  else{
    df.log2 <- apply(df, 2, log2.transform)
    colnames(df.log2) <- paste(colsdf, '_log2', sep = '')
  }
  
  df.log2 <- as.data.frame(df.log2)
  return(df.log2)
}

###################################################
# Return actual value from log
# If you adjusted initially to take logs of only positive numbers, 
#  enter that adjustment as the second parameter
###################################################
loginv.transform <- function(x, adjust.min.transform = 0){
  result <- exp(x) - adjust_min_transform
  return(result)
}

# Return actual value from log, base 2
log2inv.transform <- function(x, adjust.min.transform = 0){
  result <- (2^x) - adjust.min.transform
  return(result)
}

###################################################
# SQUARE ROOT TRANSFORM
#
# Adding a constant positive number will ensure we dont take a square root < 0,
#    and also won't change the shape of the distribution of the data
###################################################
sqrt.transform <- function(x){
  result <- sqrt(x + min.transform(x, 0))
  return(result)
}

# Apply above function to all columns in a data frame
# Return the transformed data frame
sqrt.transform.df <- function(df, cols=c()){
  colsdf <- colnames(df)
  
  if(length(cols) > 0){
    df.sqrt <- df %>% mutate_each_(funs(sqrt.transform(.) %>% as.vector), vars=cols)
    names(df.sqrt)[match(cols,colsdf)] <- paste(cols, '_sqrt', sep = '')
  }
  else{
    df.sqrt <- apply(df, 2, sqrt.transform)
    colnames(df.sqrt) <- paste(colsdf, '_sqrt', sep = '')
  }
  
  df.sqrt <- as.data.frame(df.sqrt)
  return(df.sqrt)
}

###################################################
# SCALE AND NORMALIZE DATA
#
# Each column will have mean of 0 and sd of 1
###################################################


scale.df <- function(df, cols=c()){
  colsdf <- colnames(df)
  
  if(length(cols) > 0){
    df.scale <- df %>% mutate_each_(funs(scale(.) %>% as.vector), vars=cols)
    names(df.scale)[match(cols,colsdf)] <- paste(cols, '_scale', sep = '')
  }
  else{
    df.scale <- scale(df)
    colnames(df.scale) <- paste(colsdf, '_scale', sep = '')
  }
  
  df.scale <- as.data.frame(df.scale)
  
  return(df.scale)
}
