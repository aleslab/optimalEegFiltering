# getting confidence intervals
# set seed for consistency of results
set.seed(123)

#initialize some variables to hold output
ciScenario4_allMSEdiffs <- vector()
ciScenario4_allSNRdiffs <- vector()

# set variable lists for loops
nElec <- 64 
nTime <- 600
nTrials <- 100
noiseLevel <- 0.2
nsim <- 50

# loop to generate results for all trial number and noise power conditions
for (i in 1:nsim) {
  print(i)
  
  #Generate a simulation
  thisSignal <-  simdata(i = 64, j = 600, k = nTrials, p = 0)
  thisPatternNoiseForSignal <- SpatialTemporalNoise(i = 64, j = 600, k = nTrials, p = noiseLevel)
  thisWhiteNoiseForSignal <-  array(rnorm(nElec * nTime * nTrials, mean = 0, sd = .01), dim = c(nElec, nTime, nTrials))
  
  patternForNoiseOnly <- SpatialTemporalNoise(i = 64, j = 600, k = nTrials, p = noiseLevel)
  whiteForNoiseOnly <-  array(rnorm(nElec * nTime * nTrials, mean = 0, sd = .01), dim = c(nElec, nTime, nTrials))
  
  thisNoise <- thisWhiteNoiseForSignal * noiseLevel
  thisSignalPlusNoise <- thisSignal + thisNoise
  noiseOnlyData <- whiteForNoiseOnly * noiseLevel
  
  #Filter data
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
  
  #Calculate measures
  thisFilteredMSE <- mse(filteredSignalPlusNoise, thisSignalMean)
  thisResidual <- mean(filteredSignalNoise^2)
  thisInSNR <- mean((thisSignal^2)) / mean((thisNoise^2))
  thisOutSNR <- mean((filteredSignal^2)) / thisResidual
  thisMeanMSE <- mse(thisSignalPlusNoiseMean,thisSignalMean)
  thisMeanOutSNR <- mean(thisSignalPlusNoiseMean^2) / thisMeanMSE
  thisFilterSNRdiff <- thisOutSNR - thisInSNR
  thisMeanSNRdiff <- thisMeanOutSNR - thisInSNR
  
  #Store measures in data frames for later graphing
  ciScenario4_allSNRdiffs <- append(ciScenario4_allSNRdiffs, thisFilterSNRdiff - thisMeanSNRdiff)
  ciScenario4_allMSEdiffs <-append(ciScenario4_allMSEdiffs, thisMeanMSE - thisFilteredMSE)
}

alpha <- 0.05
Scenario4_ciMSEmu <- mean(ciScenario4_allMSEdiffs)
Scenario4_ciMSEsd <- sd(ciScenario4_allMSEdiffs)
ciScenario4_allMSEdiffs <- sort(ciScenario4_allMSEdiffs)
Scenario4_MSE_lowerci <- ciScenario4_allMSEdiffs[(alpha / 2) * nsim]
Scenario4_MSE_upperci <- ciScenario4_allMSEdiffs[(1 - (alpha / 2)) * nsim]
Scenario4_MSEci <- c(Scenario4_MSE_lowerci, Scenario4_MSE_upperci)

Scenario4_ciSNRmu <- mean(ciScenario4_allSNRdiffs)
Scenario4_ciSNRsd <- sd(ciScenario4_allSNRdiffs)
ciScenario4_allSNRdiffs <- sort(ciScenario4_allSNRdiffs)
Scenario4_SNR_lowerci <- ciScenario4_allSNRdiffs[(alpha / 2) * nsim]
Scenario4_SNR_upperci <- ciScenario4_allSNRdiffs[(1 - (alpha / 2)) * nsim]
Scenario4_SNRci <- c(Scenario4_SNR_lowerci, Scenario4_SNR_upperci)