# getting confidence intervals
# set seed for consistency of results
set.seed(123)

#initialize some variables to hold output
ciScenario2_allMSEdiffs <- vector()
ciScenario2_allSNRdiffs <- vector()

# set variable lists for loops
nElec <- 64 
nTime <- 600
nTrials <- 100
noiseLevel <- 0.8
nsim <- 50

# loop to generate results for all trial number and noise power conditions
for (i in 1:nsim) {
  print(i)
  
  #Generate a simulation
  thisSignal <-  simdata(i = 64, j = 600, k = nTrials, p = 0)
  thisPatternNoiseForSignal <- PatternNoise(i = 64, j = 600, k = nTrials, p = noiseLevel)
  thisWhiteNoiseForSignal <-  array(rnorm(nElec * nTime * nTrials, mean = 0, sd = .01), dim = c(nElec, nTime, nTrials))
  
  patternForNoiseOnly <- PatternNoise(i = 64, j = 600, k = nTrials, p = noiseLevel)
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
  ciScenario2_allSNRdiffs <- append(ciScenario2_allSNRdiffs, thisFilterSNRdiff - thisMeanSNRdiff)
  ciScenario2_allMSEdiffs <-append(ciScenario2_allMSEdiffs, thisMeanMSE - thisFilteredMSE)
}

alpha <- 0.05
Scenario2_ciMSEmu <- mean(ciScenario2_allMSEdiffs)
Scenario2_ciMSEsd <- sd(ciScenario2_allMSEdiffs)
ciScenario2_allMSEdiffs <- sort(ciScenario2_allMSEdiffs)
Scenario2_MSE_lowerci <- ciScenario2_allMSEdiffs[(alpha / 2) * nsim]
Scenario2_MSE_upperci <- ciScenario2_allMSEdiffs[(1 - (alpha / 2)) * nsim]
Scenario2_MSEci <- c(Scenario2_MSE_lowerci, Scenario2_MSE_upperci)

Scenario2_ciSNRmu <- mean(ciScenario2_allSNRdiffs)
Scenario2_ciSNRsd <- sd(ciScenario2_allSNRdiffs)
ciScenario2_allSNRdiffs <- sort(ciScenario2_allSNRdiffs)
Scenario2_SNR_lowerci <- ciScenario2_allSNRdiffs[(alpha / 2) * nsim]
Scenario2_SNR_upperci <- ciScenario2_allSNRdiffs[(1 - (alpha / 2)) * nsim]
Scenario2_SNRci <- c(Scenario2_SNR_lowerci, Scenario2_SNR_upperci)

