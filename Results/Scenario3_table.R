#set seed for consistent results
set.seed(123)

#initialize some variables to hold output
Scenario3_allGsvdMse <-data.frame()
Scenario3_allMeanMse <- data.frame()
Scenario3_allGsvdError <- data.frame()
Scenario3_allSNR <- data.frame()
Scenario3_allGsvdSDI <- data.frame()
Scenario3_allGsvdNRfactor <- data.frame()
Scenario3_allGsvdSNRDiff <- data.frame()
Scenario3_allMeanSNRDiff <- data.frame()

#set variables for simulations
nElec <- 64 
nTime <- 600
trialList <-c(25, 50, 100, 200, 400)
noiseList <-c(0.1, 0.2, 0.4, 0.8, 1.6)

# loop to generate all results
for (nTrials in trialList) {
  for (noiseLevel in noiseList) {
# print to track progress of loop
    print(c(nTrials, noiseLevel))
    
#Generate a simulation
    thisSignal <-  simdata(i = 64, j = 600, k = nTrials, p = 0)
    thisPatternNoiseForSignal <- MultiPatternNoise(i = 64, j = 600, k = nTrials, p = noiseLevel)
    thisWhiteNoiseForSignal <-  array(rnorm(nElec * nTime * nTrials, mean = 0, sd = .01), dim = c(nElec, nTime, nTrials))
    
    patternForNoiseOnly <- MultiPatternNoise(i = 64, j = 600, k = nTrials, p = noiseLevel)
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
    
#store measures in data frames for graphing 
    Scenario3_allSNR <- rbind(Scenario3_allSNR, data.frame("Condition" =
                          "Input", "Trials" = nTrials, "NoiseLevel" = noiseLevel,
                           "SNR" = thisInSNR))
    Scenario3_allSNR <- rbind(Scenario3_allSNR, data.frame("Condition" =
                           "GsvdOutput", "Trials" = nTrials, "NoiseLevel" = noiseLevel,
                            "SNR" = thisOutSNR))
    Scenario3_allGsvdSNRDiff <- rbind(Scenario3_allGsvdSNRDiff, data.frame("Trials" = nTrials,
                                "NoiseLevel" = noiseLevel, "Difference" = thisOutSNR - thisInSNR))
    Scenario3_allGsvdSDI <- rbind(Scenario3_allGsvdSDI, data.frame("Trials" = nTrials, 
                            "NoiseLevel" = noiseLevel, "SDI"= thisSDI))
    Scenario3_allGsvdNRfactor <- rbind(Scenario3_allGsvdNRfactor, data.frame("Trials" = nTrials, 
                                 "NoiseLevel" = noiseLevel, "NoiseReduction" = thisNRfactor))
    Scenario3_allGsvdMse <-rbind(Scenario3_allGsvdMse, data.frame("Method" = "gsvd", 
                           "Trials" = nTrials, "NoiseLevel" = noiseLevel, "MSE" = thisFilteredMSE))
    Scenario3_allGsvdError <-rbind(Scenario3_allGsvdError, data.frame("Partial" = "Distortion", "Trials" = nTrials, 
                             "NoiseLevel" = noiseLevel, "PercentError" = (thisDistortion / (thisDistortion + thisResidual)) * 100))
    Scenario3_allGsvdError<- rbind(Scenario3_allGsvdError, data.frame("Partial" = "ResidualNoise", "Trials" = nTrials,
                             "NoiseLevel" = noiseLevel, "PercentError" = (thisResidual / (thisDistortion + thisResidual)) * 100))
    
    
#Calculate the values if NO filtering is applied
    thisMeanMSE <- mse(thisSignalPlusNoiseMean,thisSignalMean)
    thisMeanOutSNR <- mean(thisSignalPlusNoiseMean^2) / thisMeanMSE
    Scenario3_allSNR <- rbind(Scenario3_allSNR, data.frame("Condition" =
                        "MeanOutput", "Trials" = nTrials, "NoiseLevel" = noiseLevel,
                        "SNR" = thisMeanOutSNR))
    Scenario3_allMeanSNRDiff <- rbind(Scenario3_allMeanSNRDiff, data.frame("Trials" = nTrials,
                                "NoiseLevel" = noiseLevel, "Difference" = thisMeanOutSNR - thisInSNR))
    Scenario3_allMeanMse <-rbind(Scenario3_allMeanMse, data.frame("Method" = "Averaging",
                           "Trials" = nTrials, "NoiseLevel" = noiseLevel, "MSE" = thisMeanMSE))
  }
}

# Combine MSE into one data frame for graphing
Scenario3_MSE <- data.frame()
Scenario3_MSE <- rbind(Scenario3_allGsvdMse, Scenario3_allMeanMse)