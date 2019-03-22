set.seed(123)

#initialize some variables to hold output
Scenario1_allGsvdMse <-data.frame()
Scenario1_allMeanMse <- data.frame()
Scenario1_allGsvdError <- data.frame()
Scenario1_allGsvdSNR <- data.frame()
Scenario1_allGsvdSDI <- data.frame()
Scenario1_allGsvdNRfactor <- data.frame()

nElec <- 64 
nTime <- 600
trialList <-c(25, 50, 100, 200, 400)
noiseList <-c(5, 10, 20)
for (nTrials in trialList) {
  for (noiseLevel in noiseList) {
    print(c(nTrials, noiseLevel))
    
    #Generate a simulation
    thisSignal <-  simdata(i = 64, j = 600, k = nTrials, p = 0)
    thisWhiteNoiseForSignal <-  array(rnorm(nElec * nTime * nTrials, mean = 0, sd = .01), dim = c(nElec, nTime, nTrials))
    
    whiteForNoiseOnly <-  array(rnorm(nElec * nTime * nTrials, mean = 0, sd = .01), dim = c(nElec, nTime, nTrials))
    
    thisNoise <- thisWhiteNoiseForSignal
    thisSignalPlusNoise <- thisSignal + thisNoise
    noiseOnlyData <- whiteForNoiseOnly
    
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
    Scenario1_allGsvdSNR <- rbind(Scenario1_allGsvdSNR, data.frame("Condition" =
                            "Input", "Trials" = nTrials, "NoiseLevel" = noiseLevel,
                              "SNR" = thisInSNR))
    Scenario1_allGsvdSNR <- rbind(Scenario1_allGsvdSNR, data.frame("Condition" =
                            "Output", "Trials" = nTrials, "NoiseLevel" = noiseLevel,
                              "SNR" = thisOutSNR))
    Scenario1_allGsvdSDI <- rbind(Scenario1_allGsvdSDI, data.frame("Trials" = nTrials, 
                            "NoiseLevel" = noiseLevel, "SDI"= thisSDI))
    Scenario1_allGsvdNRfactor <- rbind(Scenario1_allGsvdNRfactor, data.frame("Trials" = nTrials, 
                               "NoiseLevel" = noiseLevel, "NoiseReduction" = thisNRfactor))
    Scenario1_allGsvdMse <-rbind(Scenario1_allGsvdMse, data.frame("Method" = "gsvd", 
                            "Trials" = nTrials, "NoiseLevel" = noiseLevel, "MSE" = thisFilteredMSE))
    Scenario1_allGsvdError <-rbind(Scenario1_allGsvdError, data.frame("Partial" = "Distortion", "Trials" = nTrials, 
                              "NoiseLevel" = noiseLevel, "PercentError" = (thisDistortion / (thisDistortion + thisResidual)) * 100))
    Scenario1_allGsvdError<- rbind(Scenario1_allGsvdError, data.frame("Partial" = "ResidualNoise", "Trials" = nTrials,
                               "NoiseLevel" = noiseLevel, "PercentError" = (thisResidual / (thisDistortion + thisResidual)) * 100))
    
    
    #Calculate the values if NO filtering is applied
    thisMeanMSE <- mse(thisSignalPlusNoiseMean,thisSignalMean)
    
    #Distortion doesn't make sense to do. Since no filtering is applied by definition
    #no distortion. 
    Scenario1_allMeanMse <-rbind(Scenario1_allMeanMse, data.frame("Method" = "Averaging",
                           "Trials" = nTrials, "NoiseLevel" = noiseLevel, "MSE" = thisMeanMSE))
  }
  
}

Scenario1_MSE <- data.frame()
Scenario1_MSE <- rbind(Scenario1_allGsvdMse, Scenario1_allMeanMse)