set.seed(123)


#initialize some variables to hold output
allGsvdMse <-vector()
allGsvdDistortion <- vector()
allGsvdResidualNoise <- vector()
allGsvdOriginalNoise <- vector()
allGsvdInSNR <- vector()
allGsvdOutSNR <- vector()
allGsvdSDI <- vector()
allGsvdNRfactor <- vector()
allMeanMse <- vector()



for (nTrials in c(25, 50, 75, 100, 150)) {
  for (noiseLevel in c(.25, .50, .75, 1)) {
    
    #Generate a simulation
    thisSignal <-  simdata(i = 64, j = 600, k = nTrials, p = noiseLevel)
    thisNoise <- array(rnorm(64 * 600 * nTrials, mean = 0, sd = 1), dim = c(64, 600, nTrials))
    thisNoise <- thisNoise * noiseLevel
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
    filteredSignal<- rowMeans(filteredSignal, dims = 2) 
    filteredSignalNoise<- rowMeans(filteredNoise, dims = 2) 
  
    #Calculate some values about the GSVD filtered data
    thisMSE <- mse(filteredSignalPlusNoise, thisSignalMean)
    thisDistortion <- mse(filteredSignal, thisSignalMean)
    thisResidual <- mean(filteredSignalNoise^2)
    thisOriginal <- mean(thisNoiseMean^2)
    thisInSNR <- mean((thisSignal^2) / (thisNoise^2))
    thisOutSNR <- mean((filteredSignal^2) / thisResidual)
    thisSDI <- thisDistortion / mean(thisSignal^2)
    thisNRfactor <- thisOriginal / thisResidual
    allGsvdMse <-append(allGsvdMse, thisMSE)
    allGsvdDistortion <-append(allGsvdDistortion, thisDistortion)
    allGsvdResidualNoise <- append(allGsvdResidualNoise, thisResidual)
    allGsvdOriginalNoise <- append(allGsvdOriginalNoise, thisOriginal)
    allGsvdInSNR <- append(allGsvdInSNR, thisInSNR)
    allGsvdOutSNR <- append(allGsvdOutSNR, thisOutSNR)
    allGsvdSDI <- append(allGsvdSDI, thisSDI)
    allGsvdNRfactor <- append(allGsvdNRfactor, thisNRfactor)
  
    #Calculate the values if NO filtering is applied
    thisMSE <- mse(thisSignalPlusNoiseMean,thisSignalMean)
    
    #Distortion doesn't make sense to do. Since no filtering is applied by definition
    #no distortion. 
    #thisDistortion <- mse(thisSignalMean,thisSignalMean)
    allMeanMse <-append(allMeanMse,thisMSE)
  
  }
  
}

percentDistortion <- (allGsvdDistortion / (allGsvdDistortion + allGsvdResidualNoise)) * 100
percentResidual <- (allGsvdResidualNoise / (allGsvdDistortion + allGsvdResidualNoise)) * 100
i <- 1
NoiseMeanPercentDistortion <- vector()
while (i <= length(percentDistortion)) {
  NoiseMeanPercentDistortion <- append(NoiseMeanPercentDistortion, mean(percentDistortion[i: (i + 3)]))
  i <- i + 4
}
i <- 1
NoiseMeanPercentResidual <- vector()
while (i <= length(percentResidual)) {
  NoiseMeanPercentResidual <- append(NoiseMeanPercentResidual, mean(percentResidual[i: (i + 3)]))
  i <- i + 4
}
TrialMeanPercentDistortion <- vector()
i <- 1
for (i in 1:4) {
  values <- c(percentDistortion[i], percentDistortion[i + 4], percentDistortion[i + 8], percentDistortion[i + 12], percentDistortion[i + 16])
  TrialMeanPercentDistortion <- append(TrialMeanPercentDistortion, mean(values))
}
TrialMeanPercentResidual <- vector()
i <- 1
for (i in 1:4) {
  values <- c(percentResidual[i], percentResidual[i + 4], percentResidual[i + 8], percentResidual[i + 12], percentResidual[i + 16])
  TrialMeanPercentResidual <- append(TrialMeanPercentResidual, mean(values))
}
 # Compile tables

Scenario1.table <- matrix(nrow = 40, ncol = 4)
Scenario1.table[,1] <- rep(c("Average", "Filter"), each = 20)
Scenario1.table[,3] <- rep(c(0.25, 0.5, 0.75, 1), times = 10)
Scenario1.table[,2] <- rep(x = c(25, 50, 75, 100, 150), each = 4, times = 2)
Scenario1.table[,4] <- c(allMeanMse, allGsvdMse)
Scenario1.table <- as.data.frame(Scenario1.table)
colnames(Scenario1.table) <- c("Method", "Trials", "Noise", "MSE")

Scenario1.Error <- matrix(nrow = 10, ncol = 3)
Scenario1.Error[,1] <- rep(c("Distortion", "Residual Noise"), each = 5)
Scenario1.Error[,2] <- rep(c(25, 50, 75, 100, 150), times = 2)
Scenario1.Error[,3] <- c(NoiseMeanPercentDistortion, NoiseMeanPercentResidual)
Scenario1.Error <- as.data.frame(Scenario1.Error)
colnames(Scenario1.Error) <- c("Partial", "Trials", "Error")

Scenario1.Error2 <- matrix(nrow = 8, ncol = 3)
Scenario1.Error2[,1] <- rep(c("Distortion", "Residual Noise"), each = 4)
Scenario1.Error2[,2] <- rep(c(0.25, 0.5, 0.75, 1), times = 2)
Scenario1.Error2[,3] <- c(TrialMeanPercentDistortion, TrialMeanPercentResidual)
Scenario1.Error2 <- as.data.frame(Scenario1.Error2)
colnames(Scenario1.Error2) <- c("Partial", "Noise", "Error")

Scenario1.SDI <- matrix(nrow = 20, ncol = 3)
Scenario1.SDI[,1] <- rep(c(25, 50, 75, 100, 150), each = 4)
Scenario1.SDI[,2] <- rep(c(0.25, 0.5, 0.75, 1), times = 5)
Scenario1.SDI[,3] <- allGsvdSDI
Scenario1.SDI <- as.data.frame(Scenario1.SDI)
colnames(Scenario1.SDI) <- c("Trials", "Noise", "SDI")

Scenario1.NR <- matrix(nrow = 20, ncol = 3)
Scenario1.NR[,1] <- rep(c(25, 50, 75, 100, 150), each = 4)
Scenario1.NR[,2] <- rep(c(0.25, 0.5, 0.75, 1), times = 5)
Scenario1.NR[,3] <- allGsvdNRfactor
Scenario1.NR <- as.data.frame(Scenario1.NR)
colnames(Scenario1.NR) <- c("Number of Trials", "Noise Level", "Noise Reduction Factor")

Scenario1.SNR <- matrix(nrow = 20, ncol = 4)
Scenario1.SNR[,1] <- rep(c(25, 50, 75, 100, 150), each = 4)
Scenario1.SNR[,2] <- rep(c(0.25, 0.5, 0.75, 1), times = 5)
Scenario1.SNR[,3] <- allGsvdInSNR
Scenario1.SNR[,4] <- allGsvdOutSNR
Scenario1.SNR <- as.data.frame(Scenario1.SNR)
colnames(Scenario1.SNR) <- c("Number of Trials", "Noise Level", "Input SNR", "Output SNR")