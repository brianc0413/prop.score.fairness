set.seed(491)



N <- 1000
sims <- 100

assignments <- c(0.9,0.8,0.7,0.6,0.5)

errors.2 <- data.frame(Prob.S = 0, 
                       avg.misclass=0, lower.misclass=0, higher.misclass=0, 
                       calibrationP=0, lowercalibP=0, highercalibP=0,
                       calibrationN=0, lowercalibN=0, highercalibN=0,
                       Fpos = 0, lowerFpos=0, higherFpos=0,
                       Fneg =0, lowerFneg=0, higherFneg=0,
                       Demopar =0, lowerDpar=0, higherDpar=0,
                       isProp=0)

for (j in assignments){
  
  misclass.test <- data.frame(Base = 0, Prop = 0, None=0)
  
  fairness_prop <- data.frame(calibrationP = 0,
                              calibrationN = 0, Fpos = 0, Fneg =0,
                              demopar = 0, unrecov = 0)
  
  fairness_base <- data.frame(calibrationP = 0,
                              calibrationN = 0, Fpos = 0, Fneg =0,
                              demopar = 0, unrecov = 0)
  
  fairness_none <- data.frame(calibrationP = 0,
                              calibrationN = 0, Fpos = 0, Fneg =0,
                              demopar = 0, unrecov = 0)
  
  for (i in 1:sims){
    X <- rnorm(200,0.5)
    S <- as.integer(rbern(N, prob = j))
    
    # Probability vecotr
    probs <- 0.9*as.integer(S==1) + 0.1*as.integer(S==0)
    Y <- as.integer(rbern(N, probs))
    
    data <- data.frame(X,S,Y)
    
    
    
    # Using True Propensity / Prop Reweight values
    #base.reweight <- reweight.og(data, 2, 3)
    #prop.reweight <- reweight.prop(data[,-1], 1, 2)
    
    s0y0 <- (1-j)*(j*0.1+(1-j)*0.9)/((1-j)*0.9) 
    
    s1y0 <- (j)*(j*0.1+(1-j)*0.9)/(j*0.1)
    
    s1y1 <- (j)*(j*0.9+(1-j)*0.1)/(j*0.9)
    
    s0y1 <- (1-j)*(j*0.9+(1-j)*0.1)/((1-j)*0.1)
    
    base.reweight  <- s0y0 * (data$S==0 & data$Y==0) + 
                      s1y0 * (data$S==1 & data$Y==0) +
                      s1y1 * (data$S==1 & data$Y==1) +
                      s0y1 * (data$S==0 & data$Y==1)
    
    prop.reweight  <- s0y0 * (data$S==0 & data$Y==0)/(1-j) + 
                      s1y0 * (data$S==1 & data$Y==0)/j +
                      s1y1 * (data$S==1 & data$Y==1)/j +
                      s0y1 * (data$S==0 & data$Y==1)/(1-j)
    
    
    # Replacing Decision Trees with Random Forest
    #model_prop <- rpart(Y~., data=data, method ="class", 
    #                    weights = prop.reweight)
    #model_base <- rpart(Y~., data=data, method ="class", 
    #                    weights = base.reweight)
    
    model_prop <- forestry(x=data[,c(1,2)], 
                           y=data[,3],
                           observationWeights = prop.reweight,
                           mtry=2,
                           savable = FALSE)
    
    model_base <- forestry(x=data[,c(1,2)], 
                           y=data[,3],
                           observationWeights = base.reweight,
                           mtry=2,
                           savable = FALSE)
    
    model_none <- forestry(x=data[,c(1,2)], 
                           y=data[,3],
                           mtry=2,
                           savable = FALSE)
    
    # new data for test data
    X <- rnorm(200,0.5)
    S <- as.integer(rbern(200, prob = j))
    
    # Probability vecotr
    probs <- 0.9*as.integer(S==1) + 0.1*as.integer(S==0)
    Y <- as.integer(rbern(200, probs))
    
    data.2 <- data.frame(X,S,Y)
    
    #prop.pred <- as.integer(predict(model_prop, newdata=data.2[,-3], type="class"))-1
    #base.pred <- as.integer(predict(model_base, newdata=data.2[,-3], type="class"))-1
    
    prop.pred <- as.integer(predict(model_prop, data.2[,-3])>0.5)
    base.pred <- as.integer(predict(model_base, data.2[,-3])>0.5)
    none.pred <- as.integer(predict(model_none, data.2[,-3])>0.5)
    
    misclass.test <- rbind(misclass.test, c(mean(Y!=base.pred),mean(Y!=prop.pred),
                                            mean(Y!=none.pred)))
    
    fairness_prop <- rbind(fairness_prop, all.fairness(prop.pred, data.2$Y, data.2$S))
    fairness_base <- rbind(fairness_base, all.fairness(base.pred, data.2$Y, data.2$S))
    fairness_none <- rbind(fairness_none, all.fairness(none.pred, data.2$Y, data.2$S))
  }
  misclass.test <- data.frame(misclass.test[-1,])
  fairness_prop <- data.frame(fairness_prop[-1,])
  fairness_base <- data.frame(fairness_base[-1,])
  fairness_none <- data.frame(fairness_none[-1,])
  
  errors.2 <- rbind(errors.2, 
                    c(j, 
                      mean(misclass.test$Prop), conf.int.90(misclass.test$Prop),
                      mean(fairness_prop$calibrationP, na.rm=TRUE), conf.int.90(fairness_prop$calibrationP),
                      mean(fairness_prop$calibrationN, na.rm=TRUE), conf.int.90(fairness_prop$calibrationN),
                      mean(fairness_prop$Fpos, na.rm=TRUE), conf.int.90(fairness_prop$Fpos),
                      mean(fairness_prop$Fneg, na.rm=TRUE), conf.int.90(fairness_prop$Fneg),
                      mean(fairness_prop$demopar, na.rm=TRUE), conf.int.90(fairness_prop$demopar),
                      1))
  
  errors.2 <- rbind(errors.2, 
                    c(j,
                      mean(misclass.test$Base), conf.int.90(misclass.test$Base),
                      mean(fairness_base$calibrationP, na.rm=TRUE), conf.int.90(fairness_base$calibrationP),
                      mean(fairness_base$calibrationN, na.rm=TRUE), conf.int.90(fairness_base$calibrationN),
                      mean(fairness_base$Fpos, na.rm=TRUE), conf.int.90(fairness_base$Fpos),
                      mean(fairness_base$Fneg, na.rm=TRUE), conf.int.90(fairness_base$Fneg),
                      mean(fairness_base$demopar, na.rm=TRUE), conf.int.90(fairness_base$demopar),
                      0))
  
  errors.2 <- rbind(errors.2, 
                    c(j,
                      mean(misclass.test$None), conf.int.90(misclass.test$None),
                      mean(fairness_none$calibrationP, na.rm=TRUE), conf.int.90(fairness_none$calibrationP),
                      mean(fairness_none$calibrationN, na.rm=TRUE), conf.int.90(fairness_none$calibrationN),
                      mean(fairness_none$Fpos, na.rm=TRUE), conf.int.90(fairness_none$Fpos),
                      mean(fairness_none$Fneg, na.rm=TRUE), conf.int.90(fairness_none$Fneg),
                      mean(fairness_none$demopar, na.rm=TRUE), conf.int.90(fairness_none$demopar),
                      2))
  
}

errors.2 <- errors.2[-1, ]
errors.2
errors.2$isProp <- as.factor(errors.2$isProp)

# Accuracy Plots
plot(ggplot(errors.2, aes(x = Prob.S, group=isProp))+
       
       geom_line(aes(y=avg.misclass, col=isProp)) + 
       geom_ribbon(aes(ymin = lower.misclass, ymax=higher.misclass, fill = isProp), alpha=0.2, show.legend = FALSE) +
       
       theme_bw() + 
       labs(x="P", y="Misclassification Rate") +
       scale_color_hue(labels=c("Base","Prop:S~Y", "Control")) +
       guides(color=guide_legend("Method")))


# Fairness Plots: Calibration and False Positive Rates
plot(ggplot(errors.2, aes(x = Prob.S, group=isProp))+
       
       geom_line(aes(y=calibrationP, col=isProp,linetype = "CalibrationP")) + 

       
       geom_line(aes(y=Fpos, col=isProp, linetype = "FalsePos")) + 

       
       geom_line(aes(y=Demopar, col=isProp, linetype = "DemoPar")) + 

       
       theme_bw() + 
       
       labs(x="P", y="Fairness Measurements") +
       scale_color_hue(labels=c("Base","Prop:S~Y", "Control")) +
       guides(color=guide_legend("Method"), linetype=guide_legend("Metric"))
)


