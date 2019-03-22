set.seed(123)

#initialize some variables to hold output
Scenario4_allGsvdMse <-data.frame()
Scenario4_allMeanMse <- data.frame()
Scenario4_allGsvdError <- data.frame()
Scenario4_allGsvdSNR <- data.frame()
Scenario4_allGsvdSDI <- data.frame()
Scenario4_allGsvdNRfactor <- data.frame()

nElec <- 64 
nTime <- 600
trialList <-c(25, 50, 100, 200, 400)
noiseList <-c(5, 10, 20)
for (nTrials in trialList) {
  for (noiseLevel in noiseList) {
    print(c(nTrials, noiseLevel))
    
    #Generate a simulation
    thisSignal <-  simdata(i = 64, j = 600, k = nTrials, p = 0)
    thisPatternNoiseForSignal <- SpatialTemporalNoise(i = 64, j = 600, k = nTrials, p = noiseLevel)
    thisWhiteNoiseForSignal <-  array(rnorm(nElec * nTime * nTrials, mean = 0, sd = .01), dim = c(nElec, nTime, nTrials))
    
    patternForNoiseOnly <- SpatialTemporalNoise(i = 64, j = 600, k = nTrials, p = noiseLevel)
    whiteForNoiseOnly <-  array(rnorm(nElec * nTime * nTrials, mean = 0, sd = .01), dim = c(nElec, nTime, nTrials))
    
    thisNoise <- thisPatternNoiseForSignal+thisWhiteNoiseForSignal
    thisSignalPlusNoise <- thisSignal + thisNoise
    noiseOnlyData <- patternForNoiseOnly+whiteForNoiseOnly
    
    #Analyze output
    filter <- Extract_GSVD_filter(thisSignalPlusNoise, noiseOnlyData)
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
    thisFilteredMSE <- mse(filteredSignalPlusNoise, thisSignalMean)
    thisDistortion <- mse(filteredSignal, thisSignalMean)
    thisResidual <- mean(filteredSignalNoise^2)
    thisOriginal <- mean(thisNoiseMean^2)
    thisInSNR <- mean((thisSignal^2) / (thisNoise^2))
    thisOutSNR <- mean((filteredSignal^2) / thisResidual)
    thisSDI <- thisDistortion / mean(thisSignal^2)
    thisNRfactor <- thisOriginal / thisResidual
    
    #These vectors aren't the nicest. Data Frame is Better. 
    Scenario4_allGsvdSNR <- rbind(Scenario4_allGsvdSNR, data.frame("Condition" =
                             "Input", "Trials" = nTrials, "NoiseLevel" = noiseLevel,
                             "SNR" = thisInSNR))
    Scenario4_allGsvdSNR <- rbind(Scenario4_allGsvdSNR, data.frame("Condition" =
                             "Output", "Trials" = nTrials, "NoiseLevel" = noiseLevel,
                             "SNR" = thisOutSNR))
    Scenario4_allGsvdSDI <- rbind(Scenario4_allGsvdSDI, data.frame("Trials" = nTrials, 
                             "NoiseLevel" = noiseLevel, "SDI"= thisSDI))
    Scenario4_allGsvdNRfactor <- rbind(Scenario4_allGsvdNRfactor, data.frame("Trials" = nTrials, 
                                 "NoiseLevel" = noiseLevel, "NoiseReduction" = thisNRfactor))
    Scenario4_allGsvdMse <-rbind(Scenario4_allGsvdMse, data.frame("Method" = "gsvd", 
                          "Trials" = nTrials, "NoiseLevel" = noiseLevel, "MSE" = thisFilteredMSE))
    Scenario4_allGsvdError <-rbind(Scenario4_allGsvdError, data.frame("Partial" = "Distortion", "Trials" = nTrials, 
                             "NoiseLevel" = noiseLevel, "PercentError" = (thisDistortion / (thisDistortion + thisResidual)) * 100))
    Scenario4_allGsvdError<- rbind(Scenario4_allGsvdError, data.frame("Partial" = "ResidualNoise", "Trials" = nTrials,
                            "NoiseLevel" = noiseLevel, "PercentError" = (thisResidual / (thisDistortion + thisResidual)) * 100))
    
    
    #Calculate the values if NO filtering is applied
    thisMeanMSE <- mse(thisSignalPlusNoiseMean,thisSignalMean)
    
    #Distortion doesn't make sense to do. Since no filtering is applied by definition
    #no distortion. 
    Scenario4_allMeanMse <-rbind(Scenario4_allMeanMse, data.frame("Method" = "Averaging",
                           "Trials" = nTrials, "NoiseLevel" = noiseLevel, "MSE" = thisMeanMSE))
  }
  
}

Scenario4_MSE <- data.frame()
Scenario4_MSE <- rbind(Scenario4_allGsvdMse, Scenario4_allMeanMse)