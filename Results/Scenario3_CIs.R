# getting confidence intervals
# set seed for consistency of results
set.seed(123)

#initialize some variables to hold output
ciScenario3_allMSEdiffs <- vector()
ciScenario3_allSNRdiffs <- vector()

# set variable lists for loops
nElec <- 64 
nTime <- 600
nTrials <- 100
noiseLevel <- 0.4
nsim <- 50

# loop to generate results for all trial number and noise power conditions
for (i in 1:nsim) {
  print(i)
  
  #Generate a simulation
  thisSignal <-  simdata(i = 64, j = 600, k = nTrials, p = 0)
  thisPatternNoiseForSignal <- MultiPatternNoise(i = 64, j = 600, k = nTrials, p = noiseLevel)
  thisWhiteNoiseForSignal <-  array(rnorm(nElec * nTime * nTrials, mean = 0, sd = .01), dim = c(nElec, nTime, nTrials))
  
  patternForNoiseOnly <- MultiPatternNoise(i = 64, j = 600, k = nTrials, p = noiseLevel)
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
  ciScenario3_allSNRdiffs <- append(ciScenario3_allSNRdiffs, thisFilterSNRdiff - thisMeanSNRdiff)
  ciScenario3_allMSEdiffs <-append(ciScenario3_allMSEdiffs, thisMeanMSE - thisFilteredMSE)
}

alpha <- 0.05
Scenario3_ciMSEmu <- mean(ciScenario3_allMSEdiffs)
Scenario3_ciMSEsd <- sd(ciScenario3_allMSEdiffs)
ciScenario3_allMSEdiffs <- sort(ciScenario3_allMSEdiffs)
Scenario3_MSE_lowerci <- ciScenario3_allMSEdiffs[(alpha / 2) * nsim]
Scenario3_MSE_upperci <- ciScenario3_allMSEdiffs[(1 - (alpha / 2)) * nsim]
Scenario3_MSEci <- c(Scenario3_MSE_lowerci, Scenario3_MSE_upperci)

Scenario3_ciSNRmu <- mean(ciScenario3_allSNRdiffs)
Scenario3_ciSNRsd <- sd(ciScenario3_allSNRdiffs)
ciScenario3_allSNRdiffs <- sort(ciScenario3_allSNRdiffs)
Scenario3_SNR_lowerci <- ciScenario3_allSNRdiffs[(alpha / 2) * nsim]
Scenario3_SNR_upperci <- ciScenario3_allSNRdiffs[(1 - (alpha / 2)) * nsim]
Scenario3_SNRci <- c(Scenario3_SNR_lowerci, Scenario3_SNR_upperci)