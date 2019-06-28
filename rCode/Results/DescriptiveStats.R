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
Scenario1_descriptives <- matrix(nrow = 4, ncol = 11)
colnames(Scenario1_descriptives) <- c("GSVDMSE", "AveragingMSE", "Distortion", 
"ResidualNoise", "SDI", "NoiseReduction", "InputSNR", "GSVDOutputSNR", 
"AveragingOutputSNR", "GsvdSNRdifference", "AveragingSNRdifference")
rownames(Scenario1_descriptives) <- c("Mean", "SD", "Min", "Max")
Scenario1_descriptives[, "GSVDMSE"] <- descriptives(Scenario1_MSE$MSE[which(Scenario1_MSE$Method == "gsvd")])
Scenario1_descriptives[, "AveragingMSE"] <- descriptives(Scenario1_MSE$MSE[which(Scenario1_MSE$Method == "Averaging")])
Scenario1_descriptives[, "Distortion"] <- descriptives(Scenario1_allGsvdError$PercentError[which(Scenario1_allGsvdError$Partial == "Distortion")])
Scenario1_descriptives[, "ResidualNoise"] <- descriptives(Scenario1_allGsvdError$PercentError[which(Scenario1_allGsvdError$Partial == "ResidualNoise")])
Scenario1_descriptives[, "SDI"] <- descriptives(Scenario1_allGsvdSDI$SDI)
Scenario1_descriptives[, "NoiseReduction"] <- descriptives(Scenario1_allGsvdNRfactor$NoiseReduction)
Scenario1_descriptives[, "InputSNR"] <- descriptives(Scenario1_allSNR$SNR[which(Scenario1_allSNR$Condition == "Input")])
Scenario1_descriptives[, "GSVDOutputSNR"] <- descriptives(Scenario1_allSNR$SNR[which(Scenario1_allSNR$Condition == "GsvdOutput")])
Scenario1_descriptives[, "AveragingOutputSNR"] <- descriptives(Scenario1_allSNR$SNR[which(Scenario1_allSNR$Condition == "MeanOutput")])
Scenario1_descriptives[, "GsvdSNRdifference"] <- descriptives(Scenario1_allGsvdSNRDiff$Difference)
Scenario1_descriptives[, "AveragingSNRdifference"] <- descriptives(Scenario1_allMeanSNRDiff$Difference)
Scenario1_descriptives <- t(Scenario1_descriptives)

#Create table of descriptive statistics for Scenario 2
Scenario2_descriptives <- matrix(nrow = 4, ncol = 11)
colnames(Scenario2_descriptives) <- c("GSVDMSE", "AveragingMSE", "Distortion", 
                                      "ResidualNoise", "SDI", "NoiseReduction", "InputSNR", "GSVDOutputSNR", 
                                      "AveragingOutputSNR", "GsvdSNRdifference", "AveragingSNRdifference")
rownames(Scenario2_descriptives) <- c("Mean", "SD", "Min", "Max")
Scenario2_descriptives[, "GSVDMSE"] <- descriptives(Scenario2_MSE$MSE[which(Scenario2_MSE$Method == "gsvd")])
Scenario2_descriptives[, "AveragingMSE"] <- descriptives(Scenario2_MSE$MSE[which(Scenario2_MSE$Method == "Averaging")])
Scenario2_descriptives[, "Distortion"] <- descriptives(Scenario2_allGsvdError$PercentError[which(Scenario2_allGsvdError$Partial == "Distortion")])
Scenario2_descriptives[, "ResidualNoise"] <- descriptives(Scenario2_allGsvdError$PercentError[which(Scenario2_allGsvdError$Partial == "ResidualNoise")])
Scenario2_descriptives[, "SDI"] <- descriptives(Scenario2_allGsvdSDI$SDI)
Scenario2_descriptives[, "NoiseReduction"] <- descriptives(Scenario2_allGsvdNRfactor$NoiseReduction)
Scenario2_descriptives[, "InputSNR"] <- descriptives(Scenario2_allSNR$SNR[which(Scenario2_allSNR$Condition == "Input")])
Scenario2_descriptives[, "GSVDOutputSNR"] <- descriptives(Scenario2_allSNR$SNR[which(Scenario2_allSNR$Condition == "GsvdOutput")])
Scenario2_descriptives[, "AveragingOutputSNR"] <- descriptives(Scenario2_allSNR$SNR[which(Scenario2_allSNR$Condition == "MeanOutput")])
Scenario2_descriptives[, "GsvdSNRdifference"] <- descriptives(Scenario2_allGsvdSNRDiff$Difference)
Scenario2_descriptives[, "AveragingSNRdifference"] <- descriptives(Scenario2_allMeanSNRDiff$Difference)
Scenario2_descriptives <- t(Scenario2_descriptives)

#create table of descriptives for Scenario 3
Scenario3_descriptives <- matrix(nrow = 4, ncol = 11)
colnames(Scenario3_descriptives) <- c("GSVDMSE", "AveragingMSE", "Distortion", 
                                      "ResidualNoise", "SDI", "NoiseReduction", "InputSNR", "GSVDOutputSNR", 
                                      "AveragingOutputSNR", "GsvdSNRdifference", "AveragingSNRdifference")
rownames(Scenario3_descriptives) <- c("Mean", "SD", "Min", "Max")
Scenario3_descriptives[, "GSVDMSE"] <- descriptives(Scenario3_MSE$MSE[which(Scenario3_MSE$Method == "gsvd")])
Scenario3_descriptives[, "AveragingMSE"] <- descriptives(Scenario3_MSE$MSE[which(Scenario3_MSE$Method == "Averaging")])
Scenario3_descriptives[, "Distortion"] <- descriptives(Scenario3_allGsvdError$PercentError[which(Scenario3_allGsvdError$Partial == "Distortion")])
Scenario3_descriptives[, "ResidualNoise"] <- descriptives(Scenario3_allGsvdError$PercentError[which(Scenario3_allGsvdError$Partial == "ResidualNoise")])
Scenario3_descriptives[, "SDI"] <- descriptives(Scenario3_allGsvdSDI$SDI)
Scenario3_descriptives[, "NoiseReduction"] <- descriptives(Scenario3_allGsvdNRfactor$NoiseReduction)
Scenario3_descriptives[, "InputSNR"] <- descriptives(Scenario3_allSNR$SNR[which(Scenario3_allSNR$Condition == "Input")])
Scenario3_descriptives[, "GSVDOutputSNR"] <- descriptives(Scenario3_allSNR$SNR[which(Scenario3_allSNR$Condition == "GsvdOutput")])
Scenario3_descriptives[, "AveragingOutputSNR"] <- descriptives(Scenario3_allSNR$SNR[which(Scenario3_allSNR$Condition == "MeanOutput")])
Scenario3_descriptives[, "GsvdSNRdifference"] <- descriptives(Scenario3_allGsvdSNRDiff$Difference)
Scenario3_descriptives[, "AveragingSNRdifference"] <- descriptives(Scenario3_allMeanSNRDiff$Difference)
Scenario3_descriptives <- t(Scenario3_descriptives)

#create table of descriptives for Scenario 4
Scenario4_descriptives <- matrix(nrow = 4, ncol = 11)
colnames(Scenario4_descriptives) <- c("GSVDMSE", "AveragingMSE", "Distortion", 
                                      "ResidualNoise", "SDI", "NoiseReduction", "InputSNR", "GSVDOutputSNR", 
                                      "AveragingOutputSNR", "GsvdSNRdifference", "AveragingSNRdifference")
rownames(Scenario4_descriptives) <- c("Mean", "SD", "Min", "Max")
Scenario4_descriptives[, "GSVDMSE"] <- descriptives(Scenario4_MSE$MSE[which(Scenario4_MSE$Method == "gsvd")])
Scenario4_descriptives[, "AveragingMSE"] <- descriptives(Scenario4_MSE$MSE[which(Scenario4_MSE$Method == "Averaging")])
Scenario4_descriptives[, "Distortion"] <- descriptives(Scenario4_allGsvdError$PercentError[which(Scenario4_allGsvdError$Partial == "Distortion")])
Scenario4_descriptives[, "ResidualNoise"] <- descriptives(Scenario4_allGsvdError$PercentError[which(Scenario4_allGsvdError$Partial == "ResidualNoise")])
Scenario4_descriptives[, "SDI"] <- descriptives(Scenario4_allGsvdSDI$SDI)
Scenario4_descriptives[, "NoiseReduction"] <- descriptives(Scenario4_allGsvdNRfactor$NoiseReduction)
Scenario4_descriptives[, "InputSNR"] <- descriptives(Scenario4_allSNR$SNR[which(Scenario4_allSNR$Condition == "Input")])
Scenario4_descriptives[, "GSVDOutputSNR"] <- descriptives(Scenario4_allSNR$SNR[which(Scenario4_allSNR$Condition == "GsvdOutput")])
Scenario4_descriptives[, "AveragingOutputSNR"] <- descriptives(Scenario4_allSNR$SNR[which(Scenario4_allSNR$Condition == "MeanOutput")])
Scenario4_descriptives[, "GsvdSNRdifference"] <- descriptives(Scenario4_allGsvdSNRDiff$Difference)
Scenario4_descriptives[, "AveragingSNRdifference"] <- descriptives(Scenario4_allMeanSNRDiff$Difference)
Scenario4_descriptives <- t(Scenario4_descriptives)

#create table of descriptives for Scenario 5
Scenario5_descriptives <- matrix(nrow = 4, ncol = 11)
colnames(Scenario5_descriptives) <- c("GSVDMSE", "AveragingMSE", "Distortion", 
                                      "ResidualNoise", "SDI", "NoiseReduction", "InputSNR", "GSVDOutputSNR", 
                                      "AveragingOutputSNR", "GsvdSNRdifference", "AveragingSNRdifference")
rownames(Scenario5_descriptives) <- c("Mean", "SD", "Min", "Max")
Scenario5_descriptives[, "GSVDMSE"] <- descriptives(Scenario5_MSE$MSE[which(Scenario5_MSE$Method == "gsvd")])
Scenario5_descriptives[, "AveragingMSE"] <- descriptives(Scenario5_MSE$MSE[which(Scenario5_MSE$Method == "Averaging")])
Scenario5_descriptives[, "Distortion"] <- descriptives(Scenario5_allGsvdError$PercentError[which(Scenario5_allGsvdError$Partial == "Distortion")])
Scenario5_descriptives[, "ResidualNoise"] <- descriptives(Scenario5_allGsvdError$PercentError[which(Scenario5_allGsvdError$Partial == "ResidualNoise")])
Scenario5_descriptives[, "SDI"] <- descriptives(Scenario5_allGsvdSDI$SDI)
Scenario5_descriptives[, "NoiseReduction"] <- descriptives(Scenario5_allGsvdNRfactor$NoiseReduction)
Scenario5_descriptives[, "InputSNR"] <- descriptives(Scenario5_allSNR$SNR[which(Scenario5_allSNR$Condition == "Input")])
Scenario5_descriptives[, "GSVDOutputSNR"] <- descriptives(Scenario5_allSNR$SNR[which(Scenario5_allSNR$Condition == "GsvdOutput")])
Scenario5_descriptives[, "AveragingOutputSNR"] <- descriptives(Scenario5_allSNR$SNR[which(Scenario5_allSNR$Condition == "MeanOutput")])
Scenario5_descriptives[, "GsvdSNRdifference"] <- descriptives(Scenario5_allGsvdSNRDiff$Difference)
Scenario5_descriptives[, "AveragingSNRdifference"] <- descriptives(Scenario5_allMeanSNRDiff$Difference)
Scenario5_descriptives <- t(Scenario5_descriptives)