set.seed(123)

#initialize some variables to hold output
tScenario4_allGsvdMse <-data.frame()
tScenario4_allMeanMse <- data.frame()
tScenario4_allGsvdError <- data.frame()
tScenario4_allSNR <- data.frame()
tScenario4_allGsvdSDI <- data.frame()
tScenario4_allGsvdNRfactor <- data.frame()
tScenario4_allGsvdSNRDiff <- data.frame()
tScenario4_allMeanSNRDiff <- data.frame()

#set variables for simulations
nElec <- 64 
nTime <- 600
trialList <-c(25, 50, 100, 200, 400)
noiseList <-c(0.0125, 0.025, 0.05, 0.1, 0.2)

#loop to generate all results
for (nTrials in trialList) {
  for (noiseLevel in noiseList) {
    #print to track loop progress
    print(c(nTrials, noiseLevel))
    
    #Generate a simulation
    thisSignal <-  alt_simdata(i = 64, j = 600, k = nTrials, p = 0)
    thisPatternNoiseForSignal <- SpatialTemporalNoise(i = 64, j = 600, k = nTrials, p = noiseLevel)
    thisWhiteNoiseForSignal <-  array(rnorm(nElec * nTime * nTrials, mean = 0, sd = .01), dim = c(nElec, nTime, nTrials))
    
    patternForNoiseOnly <- SpatialTemporalNoise(i = 64, j = 600, k = nTrials, p = noiseLevel)
    whiteForNoiseOnly <-  array(rnorm(nElec * nTime * nTrials, mean = 0, sd = .01), dim = c(nElec, nTime, nTrials))
    
    thisNoise <- thisPatternNoiseForSignal + thisWhiteNoiseForSignal
    thisSignalPlusNoise <- thisSignal + thisNoise
    noiseOnlyData <- patternForNoiseOnly + whiteForNoiseOnly
    
    #Apply filter to data
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
    
    #Calculate measures for the GSVD filtered data
    thisFilteredMSE <- mse(filteredSignalPlusNoise, thisSignalMean)
    thisDistortion <- mse(filteredSignal, thisSignalMean)
    thisResidual <- mean(filteredSignalNoise^2)
    thisOriginal <- mean(thisNoiseMean^2)
    thisInSNR <- mean(thisSignal^2) / mean(thisNoise^2)
    thisOutSNR <- mean(filteredSignal^2) / thisResidual
    thisSDI <- thisDistortion / mean(thisSignal^2)
    thisNRfactor <- thisOriginal / thisResidual
    
    #Store measures in data frames for chemistry 
    tScenario4_allSNR <- rbind(tScenario4_allSNR, data.frame("Condition" =
                                                             "Input", "Trials" = nTrials, "NoiseLevel" = noiseLevel,
                                                           "SNR" = thisInSNR))
    tScenario4_allSNR <- rbind(tScenario4_allSNR, data.frame("Condition" =
                                                             "GsvdOutput", "Trials" = nTrials, "NoiseLevel" = noiseLevel,
                                                           "SNR" = thisOutSNR))
    tScenario4_allGsvdSNRDiff <- rbind(tScenario4_allGsvdSNRDiff, data.frame("Trials" = nTrials,
                                                                           "NoiseLevel" = noiseLevel, "Difference" = thisOutSNR - thisInSNR))
    tScenario4_allGsvdSDI <- rbind(tScenario4_allGsvdSDI, data.frame("Trials" = nTrials, 
                                                                   "NoiseLevel" = noiseLevel, "SDI"= thisSDI))
    tScenario4_allGsvdNRfactor <- rbind(tScenario4_allGsvdNRfactor, data.frame("Trials" = nTrials, 
                                                                             "NoiseLevel" = noiseLevel, "NoiseReduction" = thisNRfactor))
    tScenario4_allGsvdMse <-rbind(tScenario4_allGsvdMse, data.frame("Method" = "gsvd", 
                                                                  "Trials" = nTrials, "NoiseLevel" = noiseLevel, "MSE" = thisFilteredMSE))
    tScenario4_allGsvdError <-rbind(tScenario4_allGsvdError, data.frame("Partial" = "Distortion", "Trials" = nTrials, 
                                                                      "NoiseLevel" = noiseLevel, "PercentError" = (thisDistortion / (thisDistortion + thisResidual)) * 100))
    tScenario4_allGsvdError<- rbind(tScenario4_allGsvdError, data.frame("Partial" = "ResidualNoise", "Trials" = nTrials,
                                                                      "NoiseLevel" = noiseLevel, "PercentError" = (thisResidual / (thisDistortion + thisResidual)) * 100))
    
    
    #Calculate the values if NO filtering is applied
    thisMeanMSE <- mse(thisSignalPlusNoiseMean,thisSignalMean)
    thisMeanOutSNR <- mean(thisSignalPlusNoiseMean^2) / thisMeanMSE
    tScenario4_allSNR <- rbind(tScenario4_allSNR, data.frame("Condition" =
                                                             "MeanOutput", "Trials" = nTrials, "NoiseLevel" = noiseLevel,
                                                           "SNR" = thisMeanOutSNR))
    tScenario4_allMeanSNRDiff <- rbind(tScenario4_allMeanSNRDiff, data.frame("Trials" = nTrials,
                                                                           "NoiseLevel" = noiseLevel, "Difference" = thisMeanOutSNR - thisInSNR))
    tScenario4_allMeanMse <-rbind(tScenario4_allMeanMse, data.frame("Method" = "Averaging",
                                                                  "Trials" = nTrials, "NoiseLevel" = noiseLevel, "MSE" = thisMeanMSE))
  }
  
}

#Combine MSE for graphing purposes
tScenario4_MSE <- data.frame()
tScenario4_MSE <- rbind(tScenario4_allGsvdMse, tScenario4_allMeanMse)