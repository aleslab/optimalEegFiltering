set.seed(123)

#initialize some variables to hold output
Scenario3_allGsvdMse <-vector()
Scenario3_allGsvdDistortion <- vector()
Scenario3_allMeanMse <- vector()
Scenario3_allGsvdResidualNoise <- vector()
Scenario3_allGsvdOriginalNoise <- vector()


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


# Compile tables

Scenario3.table <- matrix(nrow = 40, ncol = 4)
Scenario3.table[,1] <- rep(c("Average", "Filter"), each = 20)
Scenario3.table[,3] <- rep(c(0.25, 0.5, 0.75, 1), times = 10)
Scenario3.table[,2] <- rep(x = c(25, 50, 75, 100, 150), each = 4, times = 2)
Scenario3.table[,4] <- c(Scenario3_allMeanMse, Scenario3_allGsvdMse)
Scenario3.table <- as.data.frame(Scenario3.table)
colnames(Scenario3.table) <- c("Method", "Trials", "Noise", "MSE")

Scenario3.NoiseBreakdown <- matrix(nrow = 20, ncol = 5)
Scenario3.NoiseBreakdown[,1] <- rep(x = c(25, 50, 75, 100, 150), each = 4)
Scenario3.NoiseBreakdown[,2] <- rep(c(0.25, 0.5, 0.75, 1), times = 5)
Scenario3.NoiseBreakdown[,3] <- Scenario3_allGsvdDistortion
Scenario3.NoiseBreakdown[,4] <- Scenario3_allGsvdOriginalNoise
Scenario3.NoiseBreakdown[,5] <- Scenario3_allGsvdResidualNoise
Scenario3.NoiseBreakdown <- as.data.frame(Scenario3.NoiseBreakdown)
colnames(Scenario3.NoiseBreakdown) <- c("Trials", "Noise", "Distortion", "OriginalNoise", "ResidualNoise")