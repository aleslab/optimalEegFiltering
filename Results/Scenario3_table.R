set.seed(123)

#initialize some variables to hold output
Scenario3_allGsvdMse <-vector()
Scenario3_allGsvdDistortion <- vector()
Scenario3_allMeanMse <- vector()
Scenario3_allGsvdResidualNoise <- vector()
Scenario3_allGsvdOriginalNoise <- vector()
Scenario3_allGsvdInSNR <- vector()
Scenario3_allGsvdOutSNR <- vector()
Scenario3_allGsvdSDI <- vector()
Scenario3_allGsvdNRfactor <- vector()


for (nTrials in c(25, 50, 75, 100, 150)) {
  for (noiseLevel in c(.25, .50, .75, 1)) {
    
    #Generate a simulation
    thisSignal <-  simdata(i = 64, j = 600, k = nTrials, p = noiseLevel)
    thisNoise <- MultiPatternNoise(i = 64, j = 600, k = nTrials, p = noiseLevel)
    thisSignalPlusNoise <- thisSignal + thisNoise
    
    #Analyze output
    filter <- Extract_GSVD_filter(thisSignalPlusNoise, thisNoise)
    filteredSignalPlusNoise <- applyFilterTo3dData(filter, thisSignalPlusNoise)
    filteredSignal <- applyFilterTo3dData(filter, thisSignal)
    filteredNoise <-  applyFilterTo3dData(filter, thisNoise)
    
    #Take mean over trials.  
    thisSignalMean <- rowMeans(thisSignal, dims = 2)
    thisSignalPlusNoiseMean <- rowMeans(thisSignalPlusNoise, dims = 2)
    thisNoiseMean <- rowMeans(thisNoise, dims = 2)
    filteredSignalPlusNoise<- rowMeans(filteredSignalPlusNoise, dims = 2) 
    filteredSignal <- rowMeans(filteredSignal, dims = 2) 
    filteredSignalNoise <- rowMeans(filteredNoise, dims = 2) 
    
    #Calculate some values about the GSVD filtered data
    thisMSE <- mse(filteredSignalPlusNoise, thisSignalMean)
    thisDistortion <- mse(filteredSignal, thisSignalMean)
    thisResidual <- mean(filteredSignalNoise^2)
    thisOriginal <- mean(thisNoiseMean^2)
    thisInSNR <- mean((thisSignal^2) / (thisNoise^2))
    thisOutSNR <- mean((filteredSignal^2) / thisResidual)
    thisSDI <- thisDistortion / mean(thisSignal^2)
    thisNRfactor <- thisOriginal / thisResidual
    Scenario3_allGsvdInSNR <- append(Scenario3_allGsvdInSNR, thisInSNR)
    Scenario3_allGsvdOutSNR <- append(Scenario3_allGsvdOutSNR, thisOutSNR)
    Scenario3_allGsvdSDI <- append(Scenario3_allGsvdSDI, thisSDI)
    Scenario3_allGsvdNRfactor <- append(Scenario3_allGsvdNRfactor, thisNRfactor)
    Scenario3_allGsvdMse <-append(Scenario3_allGsvdMse, thisMSE)
    Scenario3_allGsvdDistortion <-append(Scenario3_allGsvdDistortion, thisDistortion)
    Scenario3_allGsvdResidualNoise <- append(Scenario3_allGsvdResidualNoise, thisResidual)
    Scenario3_allGsvdOriginalNoise <- append(Scenario3_allGsvdOriginalNoise, thisOriginal)
    
    #Calculate the values if NO filtering is applied
    thisMSE <- mse(thisSignalPlusNoiseMean,thisSignalMean)
    
    #Distortion doesn't make sense to do. Since no filtering is applied by definition
    #no distortion. 
    #thisDistortion <- mse(thisSignalMean,thisSignalMean)
    Scenario3_allMeanMse <-append(Scenario3_allMeanMse,thisMSE)
    
  }
  
}

Scenario3_percentDistortion <- (Scenario3_allGsvdDistortion / (Scenario3_allGsvdDistortion + Scenario3_allGsvdResidualNoise)) * 100
Scenario3_percentResidual <- (Scenario3_allGsvdResidualNoise / (Scenario3_allGsvdDistortion + Scenario3_allGsvdResidualNoise)) * 100

i <- 1
Scenario3_NoiseMeanPercentDistortion <- vector()
while (i <= length(percentDistortion)) {
  Scenario3_NoiseMeanPercentDistortion <- append(Scenario3_NoiseMeanPercentDistortion, mean(Scenario3_percentDistortion[i: (i + 3)]))
  i <- i + 4
}

i <- 1
Scenario3_NoiseMeanPercentResidual <- vector()
while (i <= length(Scenario3_percentResidual)) {
  Scenario3_NoiseMeanPercentResidual <- append(Scenario3_NoiseMeanPercentResidual, mean(Scenario3_percentResidual[i: (i + 3)]))
  i <- i + 4
}

Scenario3_TrialMeanPercentDistortion <- vector()
i <- 1
for (i in 1:4) {
  values <- c(Scenario3_percentDistortion[i], Scenario3_percentDistortion[i + 4], Scenario3_percentDistortion[i + 8], Scenario3_percentDistortion[i + 12], Scenario3_percentDistortion[i + 16])
  Scenario3_TrialMeanPercentDistortion <- append(Scenario3_TrialMeanPercentDistortion, mean(values))
}

Scenario3_TrialMeanPercentResidual <- vector()
i <- 1
for (i in 1:4) {
  values <- c(Scenario3_percentResidual[i], Scenario3_percentResidual[i + 4], Scenario3_percentResidual[i + 8], Scenario3_percentResidual[i + 12], Scenario3_percentResidual[i + 16])
  Scenario3_TrialMeanPercentResidual <- append(Scenario3_TrialMeanPercentResidual, mean(values))
}


# Compile tables

Scenario3.table <- matrix(nrow = 40, ncol = 4)
Scenario3.table[,1] <- rep(c("Average", "Filter"), each = 20)
Scenario3.table[,3] <- rep(c(0.25, 0.5, 0.75, 1), times = 10)
Scenario3.table[,2] <- rep(x = c(25, 50, 75, 100, 150), each = 4, times = 2)
Scenario3.table[,4] <- c(Scenario3_allMeanMse, Scenario3_allGsvdMse)
Scenario3.table <- as.data.frame(Scenario3.table)
colnames(Scenario3.table) <- c("Method", "Trials", "Noise", "MSE")

Scenario3.Error <- matrix(nrow = 10, ncol = 3)
Scenario3.Error[,1] <- rep(c("Distortion", "Residual Noise"), each = 5)
Scenario3.Error[,2] <- rep(c(25, 50, 75, 100, 150), times = 2)
Scenario3.Error[,3] <- c(Scenario3_NoiseMeanPercentDistortion, Scenario3_NoiseMeanPercentResidual)
Scenario3.Error <- as.data.frame(Scenario3.Error)
colnames(Scenario3.Error) <- c("Partial", "Trials", "Error")

Scenario3.Error2 <- matrix(nrow = 8, ncol = 3)
Scenario3.Error2[,1] <- rep(c("Distortion", "Residual Noise"), each = 4)
Scenario3.Error2[,2] <- rep(c(0.25, 0.5, 0.75, 1), times = 2)
Scenario3.Error2[,3] <- c(Scenario3_TrialMeanPercentDistortion, Scenario3_TrialMeanPercentResidual)
Scenario3.Error2 <- as.data.frame(Scenario3.Error2)
colnames(Scenario3.Error2) <- c("Partial", "Noise", "Error")

Scenario3.SDI <- matrix(nrow = 20, ncol = 3)
Scenario3.SDI[,1] <- rep(c(25, 50, 75, 100, 150), each = 4)
Scenario3.SDI[,2] <- rep(c(0.25, 0.5, 0.75, 1), times = 5)
Scenario3.SDI[,3] <- Scenario3_allGsvdSDI
Scenario3.SDI <- as.data.frame(Scenario3.SDI)
colnames(Scenario3.SDI) <- c("Trials", "Noise", "SDI")

Scenario3.NR <- matrix(nrow = 20, ncol = 3)
Scenario3.NR[,1] <- rep(c(25, 50, 75, 100, 150), each = 4)
Scenario3.NR[,2] <- rep(c(0.25, 0.5, 0.75, 1), times = 5)
Scenario3.NR[,3] <- Scenario3_allGsvdNRfactor
Scenario3.NR <- as.data.frame(Scenario3.NR)
colnames(Scenario3.NR) <- c("Number of Trials", "Noise Level", "Noise Reduction Factor")

Scenario3.SNR <- matrix(nrow = 20, ncol = 4)
Scenario3.SNR[,1] <- rep(c(25, 50, 75, 100, 150), each = 4)
Scenario3.SNR[,2] <- rep(c(0.25, 0.5, 0.75, 1), times = 5)
Scenario3.SNR[,3] <- Scenario3_allGsvdInSNR
Scenario3.SNR[,4] <- Scenario3_allGsvdOutSNR
Scenario3.SNR <- as.data.frame(Scenario3.SNR)
colnames(Scenario3.SNR) <- c("Number of Trials", "Noise Level", "Input SNR", "Output SNR")