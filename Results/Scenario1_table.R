set.seed(123)


#initialize some variables to hold output
allGsvdMse <-vector()
allGsvdDistortion <- vector()
allMeanMse <- vector()
allGsvdResidualNoise <- vector()
allGsvdOriginalNoise <- vector()


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
    allGsvdMse <-append(allGsvdMse, thisMSE)
    allGsvdDistortion <-append(allGsvdDistortion, thisDistortion)
    allGsvdResidualNoise <- append(allGsvdResidualNoise, thisResidual)
    allGsvdOriginalNoise <- append(allGsvdOriginalNoise, thisOriginal)
  
    #Calculate the values if NO filtering is applied
    thisMSE <- mse(thisSignalPlusNoiseMean,thisSignalMean)
  
    #Distortion doesn't make sense to do. Since no filtering is applied by definition
    #no distortion. 
    #thisDistortion <- mse(thisSignalMean,thisSignalMean)
    allMeanMse <-append(allMeanMse,thisMSE)
  
  }
  
}


 # Compile tables

Scenario1.table <- matrix(nrow = 40, ncol = 4)
Scenario1.table[,1] <- rep(c("Average", "Filter"), each = 20)
Scenario1.table[,3] <- rep(c(0.25, 0.5, 0.75, 1), times = 10)
Scenario1.table[,2] <- rep(x = c(25, 50, 75, 100, 150), each = 4, times = 2)
Scenario1.table[,4] <- c(allMeanMse, allGsvdMse)
Scenario1.table <- as.data.frame(Scenario1.table)
colnames(Scenario1.table) <- c("Method", "Trials", "Noise", "MSE")

Scenario1.NoiseBreakdown <- matrix(nrow = 20, ncol = 5)
Scenario1.NoiseBreakdown[,1] <- rep(x = c(25, 50, 75, 100, 150), each = 4)
Scenario1.NoiseBreakdown[,2] <- rep(c(0.25, 0.5, 0.75, 1), times = 5)
Scenario1.NoiseBreakdown[,3] <- allGsvdDistortion
Scenario1.NoiseBreakdown[,4] <- allGsvdOriginalNoise
Scenario1.NoiseBreakdown[,5] <- allGsvdResidualNoise
Scenario1.NoiseBreakdown <- as.data.frame(Scenario1.NoiseBreakdown)
colnames(Scenario1.NoiseBreakdown) <- c("Trials", "Noise", "Distortion", "OriginalNoise", "ResidualNoise")