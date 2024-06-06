library(pROC)
library(ggplot2)
library(xtable)

sim_dat <- read.csv('http://static.lib.virginia.edu/statlab/materials/data/roc_sim_dat.csv', header = T)

set.seed(2024)
sim_dat_limited <- sim_dat[sample(1:nrow(sim_dat),size=10,replace = FALSE),]
sim_dat_limited$predicted_prob_of_Yes <- round(sim_dat_limited$predicted_prob_of_Yes,2)

#print the subset
xtable(sim_dat_limited)

# get the thresholds
sim_roc <- roc(response = sim_dat_limited $actual_outcome,
               predictor = sim_dat_limited $predicted_prob_of_Yes,
               levels = c('No', 'Yes'))
sim_roc$thresholds

# a)
get_fpf_tpf <- function(predicted_probs,actual_outcomes,thresholds){
  output <- data.frame(FPF=numeric(length(thresholds)),TPF=numeric(length(thresholds)))
  for(i in 1:length(thresholds)){
    predicted_classes <- ifelse(predicted_probs >= thresholds[i], 1, 0)
    
    TP <- sum(predicted_classes == 1 & actual_outcomes == "Yes")
    FP <- sum(predicted_classes == 1 & actual_outcomes == "No")
    TN <- sum(predicted_classes == 0 & actual_outcomes == "No")
    FN <- sum(predicted_classes == 0 & actual_outcomes == "Yes")
    
    output$FPF[i] <- FP / (FP + TN)
    output$TPF[i] <- TP / (TP + FN)
  }
  return(output)
}

# b)
ggroc(sim_roc, legacy.axes = TRUE) +
  labs(x = 'False-positive rate', y = 'True-positive rate', title = 'ROC curve') + theme_bw()

ggsave("ROC.png",width=4,height=3)

#check the plot
get_fpf_tpf(sim_dat_limited$predicted_prob_of_Yes,sim_dat_limited$actual_outcome,sim_roc$thresholds)


# c)
sim_roc$auc

