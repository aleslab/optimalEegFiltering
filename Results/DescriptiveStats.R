# calculate descriptive statistics for result measures
descriptives <- function(v) {
  # the purpose of this function is to calculate basic descriptive statistics
  # The input is a vector of values for which descriptives values are to be
  #calculated and the output is a vector of length 4 of the descriptives
  
  #set up results vector
  results <- vector()
  
  #add descriptives
  results <- append(results, mean(v))
  results <- append(results, sd(v))
  results <- append(results, min(v))
  results <- append(results, max(v))
  
  #return descriptives
  return(results)
}

#create table of descriptive statistics for Scenario 1
Scenario1_descriptives <- matrix(nrow = 4, ncol = 9)
colnames(Scenario1_descriptives) <- c("GSVDMSE", "AveragingMSE", "Distortion", "ResidualNoise", "SDI", "NoiseReduction", "InputSNR", "OutputSNR", "SNRDifference")
rownames(Scenario1_descriptives) <- c("Mean", "SD", "Min", "Max")
Scenario1_descriptives[, "GSVDMSE"] <- descriptives(Scenario1_MSE$MSE[which(Scenario1_MSE$Method == "gsvd")])
Scenario1_descriptives[, "AveragingMSE"] <- descriptives(Scenario1_MSE$MSE[which(Scenario1_MSE$Method == "Averaging")])
Scenario1_descriptives[, "Distortion"] <- descriptives(Scenario1_allGsvdError$PercentError[which(Scenario1_allGsvdError$Partial == "Distortion")])
Scenario1_descriptives[, "ResidualNoise"] <- descriptives(Scenario1_allGsvdError$PercentError[which(Scenario1_allGsvdError$Partial == "ResidualNoise")])
Scenario1_descriptives[, "SDI"] <- descriptives(Scenario1_allGsvdSDI$SDI)
Scenario1_descriptives[, "NoiseReduction"] <- descriptives(Scenario1_allGsvdNRfactor$NoiseReduction)
Scenario1_descriptives[, "InputSNR"] <- descriptives(Scenario1_allGsvdSNR$SNR[which(Scenario1_allGsvdSNR$Condition == "Input")])
Scenario1_descriptives[, "OutputSNR"] <- descriptives(Scenario1_allGsvdSNR$SNR[which(Scenario1_allGsvdSNR$Condition == "Output")])
Scenario1_descriptives[, "SNRDifference"] <- descriptives(Scenario1_allGsvdSNRDiff$Difference)
Scenario1_descriptives <- t(Scenario1_descriptives)

#Create table of descriptive statistics for Scenario 2
Scenario2_descriptives <- matrix(nrow = 4, ncol = 9)
colnames(Scenario2_descriptives) <- c("GSVDMSE", "AveragingMSE", "Distortion", "ResidualNoise", "SDI", "NoiseReduction", "InputSNR", "OutputSNR", "SNRDifference")
rownames(Scenario2_descriptives) <- c("Mean", "SD", "Min", "Max")
Scenario2_descriptives[, "GSVDMSE"] <- descriptives(Scenario2_MSE$MSE[which(Scenario2_MSE$Method == "gsvd")])
Scenario2_descriptives[, "AveragingMSE"] <- descriptives(Scenario2_MSE$MSE[which(Scenario2_MSE$Method == "Averaging")])
Scenario2_descriptives[, "Distortion"] <- descriptives(Scenario2_allGsvdError$PercentError[which(Scenario2_allGsvdError$Partial == "Distortion")])
Scenario2_descriptives[, "ResidualNoise"] <- descriptives(Scenario2_allGsvdError$PercentError[which(Scenario2_allGsvdError$Partial == "ResidualNoise")])
Scenario2_descriptives[, "SDI"] <- descriptives(Scenario2_allGsvdSDI$SDI)
Scenario2_descriptives[, "NoiseReduction"] <- descriptives(Scenario2_allGsvdNRfactor$NoiseReduction)
Scenario2_descriptives[, "InputSNR"] <- descriptives(Scenario2_allGsvdSNR$SNR[which(Scenario2_allGsvdSNR$Condition == "Input")])
Scenario2_descriptives[, "OutputSNR"] <- descriptives(Scenario2_allGsvdSNR$SNR[which(Scenario2_allGsvdSNR$Condition == "Output")])
Scenario2_descriptives[, "SNRDifference"] <- descriptives(Scenario2_allGsvdSNRDiff$Difference)
Scenario2_descriptives <- t(Scenario2_descriptives)

#create table of descriptives for Scenario 3
Scenario3_descriptives <- matrix(nrow = 4, ncol = 9)
colnames(Scenario3_descriptives) <- c("GSVDMSE", "AveragingMSE", "Distortion", "ResidualNoise", "SDI", "NoiseReduction", "InputSNR", "OutputSNR", "SNRDifference")
rownames(Scenario3_descriptives) <- c("Mean", "SD", "Min", "Max")
Scenario3_descriptives[, "GSVDMSE"] <- descriptives(Scenario3_MSE$MSE[which(Scenario3_MSE$Method == "gsvd")])
Scenario3_descriptives[, "AveragingMSE"] <- descriptives(Scenario3_MSE$MSE[which(Scenario3_MSE$Method == "Averaging")])
Scenario3_descriptives[, "Distortion"] <- descriptives(Scenario3_allGsvdError$PercentError[which(Scenario3_allGsvdError$Partial == "Distortion")])
Scenario3_descriptives[, "ResidualNoise"] <- descriptives(Scenario3_allGsvdError$PercentError[which(Scenario3_allGsvdError$Partial == "ResidualNoise")])
Scenario3_descriptives[, "SDI"] <- descriptives(Scenario3_allGsvdSDI$SDI)
Scenario3_descriptives[, "NoiseReduction"] <- descriptives(Scenario3_allGsvdNRfactor$NoiseReduction)
Scenario3_descriptives[, "InputSNR"] <- descriptives(Scenario3_allGsvdSNR$SNR[which(Scenario3_allGsvdSNR$Condition == "Input")])
Scenario3_descriptives[, "OutputSNR"] <- descriptives(Scenario3_allGsvdSNR$SNR[which(Scenario3_allGsvdSNR$Condition == "Output")])
Scenario3_descriptives[, "SNRDifference"] <- descriptives(Scenario3_allGsvdSNRDiff$Difference)
Scenario3_descriptives <- t(Scenario3_descriptives)

#create table of descriptives for Scenario 4
Scenario4_descriptives <- matrix(nrow = 4, ncol = 9)
colnames(Scenario4_descriptives) <- c("GSVDMSE", "AveragingMSE", "Distortion", "ResidualNoise", "SDI", "NoiseReduction", "InputSNR", "OutputSNR", "SNRDifference")
rownames(Scenario4_descriptives) <- c("Mean", "SD", "Min", "Max")
Scenario4_descriptives[, "GSVDMSE"] <- descriptives(Scenario4_MSE$MSE[which(Scenario4_MSE$Method == "gsvd")])
Scenario4_descriptives[, "AveragingMSE"] <- descriptives(Scenario4_MSE$MSE[which(Scenario4_MSE$Method == "Averaging")])
Scenario4_descriptives[, "Distortion"] <- descriptives(Scenario4_allGsvdError$PercentError[which(Scenario4_allGsvdError$Partial == "Distortion")])
Scenario4_descriptives[, "ResidualNoise"] <- descriptives(Scenario4_allGsvdError$PercentError[which(Scenario4_allGsvdError$Partial == "ResidualNoise")])
Scenario4_descriptives[, "SDI"] <- descriptives(Scenario4_allGsvdSDI$SDI)
Scenario4_descriptives[, "NoiseReduction"] <- descriptives(Scenario4_allGsvdNRfactor$NoiseReduction)
Scenario4_descriptives[, "InputSNR"] <- descriptives(Scenario4_allGsvdSNR$SNR[which(Scenario4_allGsvdSNR$Condition == "Input")])
Scenario4_descriptives[, "OutputSNR"] <- descriptives(Scenario4_allGsvdSNR$SNR[which(Scenario4_allGsvdSNR$Condition == "Output")])
Scenario4_descriptives[, "SNRDifference"] <- descriptives(Scenario4_allGsvdSNRDiff$Difference)
Scenario4_descriptives <- t(Scenario4_descriptives)