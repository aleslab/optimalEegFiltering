#set seed for consistent results
set.seed(123)

#initialize some variables to hold output
Scenario5_allGsvdMse <-data.frame()
Scenario5_allMeanMse <- data.frame()
Scenario5_allGsvdError <- data.frame()
Scenario5_allGsvdSNR <- data.frame()
Scenario5_allGsvdSDI <- data.frame()
Scenario5_allGsvdNRfactor <- data.frame()
Scenario5_allGsvdSNRDiff <- data.frame()

# set variable lists for loops
nElec <- 128 
nTime <- 600
trialList <-c(25, 50, 100, 200, 400)
noiseList <-c(5, 10, 20)

# loop to create results for all conditions
for (nTrials in trialList) {
  for (noiseLevel in noiseList) {
    
    #print statement to keep track of loop progress
    print(c(nTrials, noiseLevel))
    
    #Generate a simulation
    thisSignal <-  simEEGsignal(nTime = 600, Trials = nTrials)
    thisNoiseForSignal <- simEEGnoise(nTime = 600, Trials = nTrials, p = 0.05)
    
    NoiseOnly <-simEEGnoise(nTime = 600, Trials = nTrials, p = 0.05)
    
    thisNoise <- thisNoiseForSignal * noiseLevel
    thisSignalPlusNoise <- thisSignal + thisNoise
    noiseOnlyData <- NoiseOnly * noiseLevel
    
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
    thisInSNR <- mean((thisSignal^2) / (thisNoise^2))
    thisOutSNR <- mean((filteredSignal^2) / thisResidual)
    thisSDI <- thisDistortion / mean(thisSignal^2)
    thisNRfactor <- thisOriginal / thisResidual
    
    #Store measures in data frames for later graphing
    Scenario5_allGsvdSNR <- rbind(Scenario5_allGsvdSNR, data.frame("Condition" =
                            "Input", "Trials" = nTrials, "NoiseLevel" = noiseLevel,
                            "SNR" = thisInSNR))
    Scenario5_allGsvdSNR <- rbind(Scenario5_allGsvdSNR, data.frame("Condition" =
                            "Output", "Trials" = nTrials, "NoiseLevel" = noiseLevel,
                            "SNR" = thisOutSNR))
    Scenario5_allGsvdSNRDiff <- rbind(Scenario5_allGsvdSNRDiff, data.frame("Trials" = nTrials,
                                "NoiseLevel" = noiseLevel, "Difference" = thisInSNR - thisOutSNR))
    Scenario5_allGsvdSDI <- rbind(Scenario5_allGsvdSDI, data.frame("Trials" = nTrials, 
                             "NoiseLevel" = noiseLevel, "SDI"= thisSDI))
    Scenario5_allGsvdNRfactor <- rbind(Scenario5_allGsvdNRfactor, data.frame("Trials" = nTrials, 
                                "NoiseLevel" = noiseLevel, "NoiseReduction" = thisNRfactor))
    Scenario5_allGsvdMse <-rbind(Scenario5_allGsvdMse, data.frame("Method" = "gsvd", 
                          "Trials" = nTrials, "NoiseLevel" = noiseLevel, "MSE" = thisFilteredMSE))
    Scenario5_allGsvdError <-rbind(Scenario5_allGsvdError, data.frame("Partial" = "Distortion", "Trials" = nTrials, 
                             "NoiseLevel" = noiseLevel, "PercentError" = (thisDistortion / (thisDistortion + thisResidual)) * 100))
    Scenario5_allGsvdError<- rbind(Scenario5_allGsvdError, data.frame("Partial" = "ResidualNoise", "Trials" = nTrials,
                             "NoiseLevel" = noiseLevel, "PercentError" = (thisResidual / (thisDistortion + thisResidual)) * 100))
    
    
    #Calculate the values if NO filtering is applied
    thisMeanMSE <- mse(thisSignalPlusNoiseMean,thisSignalMean)
    Scenario5_allMeanMse <-rbind(Scenario5_allMeanMse, data.frame("Method" = "Averaging",
                           "Trials" = nTrials, "NoiseLevel" = noiseLevel, "MSE" = thisMeanMSE))
  }
  
}

# Combine MSE's into one table for graphing purposes
Scenario5_MSE <- data.frame()
Scenario5_MSE <- rbind(Scenario5_allGsvdMse, Scenario5_allMeanMse)

