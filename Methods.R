# Pre-Processing Methods 

# Supression Method
suppression <- function(data, s_ind, y_ind){
  # cutoff threshold of 0.6 (arbitrary)
  X <- data[,-c(s_ind, y_ind)]
  drop <- which(cor(data[,s_ind],X) > 0.6)
  # drop s_ind and variables which have correlation >0.6
  return(data[,-c(drop, s_ind)])
} 

# Reweight (no propensity scores)
reweight.og <- function(data, s_ind, y_ind){
  
  s0y0 <- (mean(data[,s_ind]==0)*mean(data[,y_ind]==0))/
    mean(data[,s_ind]==0 & data[,y_ind]==0)
  
  s1y0 <- (mean(data[,s_ind]==1)*mean(data[,y_ind]==0))/
    mean(data[,s_ind]==1 & data[,y_ind]==0)
  
  s1y1 <- (mean(data[,s_ind]==1)*mean(data[,y_ind]==1))/
    mean(data[,s_ind]==1 & data[,y_ind]==1)
  
  s0y1 <- (mean(data[,s_ind]==0)*mean(data[,y_ind]==1))/
    mean(data[,s_ind]==0 & data[,y_ind]==1)
  
  return(s0y0 * (data[,s_ind]==0 & data[,y_ind]==0) + 
           s1y0 * (data[,s_ind]==1 & data[,y_ind]==0) +
           s1y1 * (data[,s_ind]==1 & data[,y_ind]==1) +
           s0y1 * (data[,s_ind]==0 & data[,y_ind]==1))
}


# Reweight (propensity score)
reweight.prop <- function(data, s_ind, y_ind){
  formula <- as.formula(paste(names(data)[s_ind], "~."))
  model <- glm(formula, family = binomial(logit), data = data)
  e <- model$fitted.values
  return(data[,s_ind]/e + (1-data[,s_ind])/(1-e))
}

# Confidence Interval Function
conf.int.90 <- function(vec){
  return(quantile(vec, probs = c(0.05, 0.95), na.rm = TRUE))
}




