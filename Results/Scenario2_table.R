set.seed(123)

#initialize some variables to hold output
Scenario2_allGsvdMse <-vector()
Scenario2_allGsvdDistortion <- vector()
Scenario2_allMeanMse <- vector()
Scenario2_allGsvdResidualNoise <- vector()
Scenario2_allGsvdOriginalNoise <- vector()


for (nTrials in c(25, 50, 75, 100, 150)) {
  for (noiseLevel in c(.25, .50, .75, 1)) {
    
    #Generate a simulation
    thisSignal <-  simdata(i = 64, j = 600, k = nTrials, p = noiseLevel)
    thisNoise <- PatternNoise(i = 64, j = 600, k = nTrials, p = noiseLevel)
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
    Scenario2_allGsvdMse <-append(Scenario2_allGsvdMse, thisMSE)
    Scenario2_allGsvdDistortion <-append(Scenario2_allGsvdDistortion, thisDistortion)
    Scenario2_allGsvdResidualNoise <- append(Scenario2_allGsvdResidualNoise, thisResidual)
    Scenario2_allGsvdOriginalNoise <- append(Scenario2_allGsvdOriginalNoise, thisOriginal)
    
    #Calculate the values if NO filtering is applied
    thisMSE <- mse(thisSignalPlusNoiseMean,thisSignalMean)
    
    #Distortion doesn't make sense to do. Since no filtering is applied by definition
    #no distortion. 
    #thisDistortion <- mse(thisSignalMean,thisSignalMean)
    Scenario2_allMeanMse <-append(Scenario2_allMeanMse,thisMSE)
    
  }
  
}


# Compile tables

Scenario2.table <- matrix(nrow = 40, ncol = 4)
Scenario2.table[,1] <- rep(c("Average", "Filter"), each = 20)
Scenario2.table[,3] <- rep(c(0.25, 0.5, 0.75, 1), times = 10)
Scenario2.table[,2] <- rep(x = c(25, 50, 75, 100, 150), each = 4, times = 2)
Scenario2.table[,4] <- c(Scenario2_allMeanMse, Scenario2_allGsvdMse)
Scenario2.table <- as.data.frame(Scenario2.table)
colnames(Scenario2.table) <- c("Method", "Trials", "Noise", "MSE")

Scenario2.NoiseBreakdown <- matrix(nrow = 20, ncol = 5)
Scenario2.NoiseBreakdown[,1] <- rep(x = c(25, 50, 75, 100, 150), each = 4)
Scenario2.NoiseBreakdown[,2] <- rep(c(0.25, 0.5, 0.75, 1), times = 5)
Scenario2.NoiseBreakdown[,3] <- Scenario2_allGsvdDistortion
Scenario2.NoiseBreakdown[,4] <- Scenario2_allGsvdOriginalNoise
Scenario2.NoiseBreakdown[,5] <- Scenario2_allGsvdResidualNoise
Scenario2.NoiseBreakdown <- as.data.frame(Scenario2.NoiseBreakdown)
colnames(Scenario2.NoiseBreakdown) <- c("Trials", "Noise", "Distortion", "OriginalNoise", "ResidualNoise")