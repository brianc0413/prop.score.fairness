set.seed(491)



N <- 1000
sims <- 1000

assignments <- c(1,0.9,0.8,0.7,0.6,0.5,0.4,0.3,0.2,0.1,0)

errors.2 <- data.frame(Prob.S = 0, 
                       avg.misclass=0, lower.misclass=0, higher.misclass=0, 
                       calibrationP=0, lowercalibP=0, highercalibP=0,
                       calibrationN=0, lowercalibN=0, highercalibN=0,
                       Fpos = 0, lowerFpos=0, higherFpos=0,
                       Fneg =0, lowerFneg=0, higherFneg=0,
                       Demopar =0, lowerDpar=0, higherDpar=0,
                       isProp=0)

for (j in assignments){
  
  misclass.test <- data.frame(Base = 0, Prop = 0)
  
  fairness_prop <- data.frame(calibrationP = 0,
                              calibrationN = 0, Fpos = 0, Fneg =0,
                              demopar = 0, unrecov = 0)
  
  fairness_base <- data.frame(calibrationP = 0,
                              calibrationN = 0, Fpos = 0, Fneg =0,
                              demopar = 0, unrecov = 0)
  
  for (i in 1:sims){
    S <- as.integer(rbern(N, prob = 0.5)) # S is random
    X <- as.integer(rbern(N, prob = 0.8*as.integer(S==1) + 0.2*as.integer(S==0))) # S affects X constantly
    Y <- as.integer(rbern(N, prob = (0.8*(1-j)*S + 0.8*(j)*X))) # S affects Y by j, X affects Y by 0.95-j
    
    data <- data.frame(X,S,Y) 
    
    
    
    # Using Estimated Propensity / Prop Reweight values
    base.reweight <- as.integer(S==1 & X==1)*(0.5/(0.4)) + 
      as.integer(S==1 & X==0)*(0.5/(0.1)) + 
      as.integer(S==0 & X==1)*(0.5/0.1) + 
      as.integer(S==0 & X==0)*(0.5/0.4)
      # BASE IS S~X
    prop.reweight <- as.integer(S==1 & Y==1)*(0.48/(0.32+.1*0.8*(1-j))) + 
      as.integer(S==1 & Y==0)*(0.52/(0.08+.1*(1-0.8*(1-j)))) + 
      as.integer(S==0 & Y==1)*(0.48/(.08+.1*(0.8*j))) + 
      as.integer(S==0 & Y==0)*(0.52/(.32+.1*(1-.8*j))) # This is normal.
    
    
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
    
    # new data for test data
    S <- as.integer(rbern(200, prob = 0.5)) # S is random
    X <- as.integer(rbern(200, prob = 0.8*as.integer(S==1) + 0.2*as.integer(S==0))) # S affects X constantly
    Y <- as.integer(rbern(200, prob = (0.8*(1-j)*S + 0.8*(j)*X))) # S affects Y by j, X affects Y by 0.95-j
    
    data.2 <- data.frame(X,S,Y)
    
    #prop.pred <- as.integer(predict(model_prop, newdata=data.2[,-3], type="class"))-1
    #base.pred <- as.integer(predict(model_base, newdata=data.2[,-3], type="class"))-1
    
    prop.pred <- as.integer(predict(model_prop, data.2[,-3])>0.5)
    base.pred <- as.integer(predict(model_base, data.2[,-3])>0.5)
    
    misclass.test <- rbind(misclass.test, c(mean(Y!=base.pred),mean(Y!=prop.pred)))
    
    fairness_prop <- rbind(fairness_prop, all.fairness(prop.pred, data.2$Y, data.2$S))
    fairness_base <- rbind(fairness_base, all.fairness(base.pred, data.2$Y, data.2$S))
  }
  misclass.test <- data.frame(misclass.test[-1,])
  fairness_prop <- data.frame(fairness_prop[-1,])
  fairness_base <- data.frame(fairness_base[-1,])
  
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
}

errors.2 <- errors.2[-1, ]
errors.2
errors.2$isProp <- as.factor(errors.2$isProp)
errors <- errors.2[errors.2$Prob.S>0.4, ]

# Accuracy Plots
plot(ggplot(errors, aes(x = Prob.S, group=isProp))+
       
       geom_line(aes(y=avg.misclass, col=isProp)) + 
       geom_ribbon(aes(ymin = lower.misclass, ymax=higher.misclass, fill = isProp), alpha=0.2, show.legend = FALSE) +
       
       theme_bw() + 
       labs(x="j", y="Misclassification Rate") +
       scale_color_hue(labels=c("e(X)","e(Y)")) +
       guides(color=guide_legend("Method")))


# Fairness Plots: Calibration and False Positive Rates
plot(ggplot(errors, aes(x = Prob.S, group=isProp))+
       
       geom_line(aes(y=calibrationP, col=isProp,linetype = "CalibrationP")) + 
       geom_ribbon(aes(ymin = lowercalibP, ymax=highercalibP, fill = isProp), 
                   alpha=0.2, show.legend = F) + 
       
       geom_line(aes(y=Fpos, col=isProp, linetype = "FalsePos")) + 
       geom_ribbon(aes(ymin = lowerFpos, ymax=higherFpos, fill = isProp), 
                   alpha=0.2, show.legend = F) + 
       
       geom_line(aes(y=Demopar, col=isProp, linetype = "DemoPar")) + 
       geom_ribbon(aes(ymin = lowerDpar, ymax=higherDpar, fill = isProp), 
                   alpha=0.2, show.legend = F) + 
       
       theme_bw() + 
       
       labs(x="j", y="Fairness Measurements") +
       scale_color_hue(labels=c("e(X)","e(Y)")) +
       guides(color=guide_legend("Method"), linetype=guide_legend("Metric"))
)


