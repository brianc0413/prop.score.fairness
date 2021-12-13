# Fairness Functions (Measure Fairness in the End)
# Our functions should score higher

# Calibration
calibration <- function(predictions, true, protected){
  
  calibration_pos_pred <- (mean(true[(protected==1)&(predictions==1)]) 
                              - mean(true[(protected!=1)&(predictions==1)]))
  
  calibration_neg_pred <- (mean(true[(protected==1)&(predictions==0)]) 
                              - mean(true[(protected!=1)&(predictions==0)]))
  
  return(c(calibration_pos_pred, calibration_neg_pred))
}

# Equalized Odds
equal_odds <- function(predictions, true, protected){
  
  false_pos <- (mean(predictions[!true & protected]) - 
                     mean(predictions[!true&!protected]))
  
  true_pos <- (mean(predictions[true & protected]) - 
                    mean(predictions[true & !protected]))
  
  return(c(false_pos, true_pos))
}

# Demographic Parity
demo_parity <- function(predictions, protected){
  return((mean(predictions[protected==1])- mean(predictions[protected!=1])))
}

# Unrecoverability
unrecoverability <- function(predictions, true, protected){
  unrecoverable <- (mean(protected[(predictions==1)]) - 
                         mean(protected[(predictions==0)]))
  return(unrecoverable)
}


# Combine All Tests
all.fairness <- function(yhat, y, s){
  return(c(calibration(yhat,y,s), equal_odds(yhat,y,s), 
           demo_parity(yhat,s), unrecoverability(yhat,y,s)))
}






