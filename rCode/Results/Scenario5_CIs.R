# getting confidence intervals
# set seed for consistency of results
set.seed(123)

#initialize some variables to hold output
ciScenario5_allMSEdiffs <- vector()
ciScenario5_allSNRdiffs <- vector()

# set variable lists for loops
nElec <- 128 
nTime <- 600
nTrials <- 100
noiseLevel <- 0.2
nsim <- 50

# loop to generate results for all trial number and noise power conditions
for (i in 1:nsim) {
  print(i)
  
  #Generate a simulation
  thisSignal <-  simEEGsignal(nTime = 600, Trials = nTrials)
  thisNoiseForSignal <- simEEGnoise(nTime = 600, Trials = nTrials, p = noiseLevel)
  
  NoiseOnly <-simEEGnoise(nTime = 600, Trials = nTrials, p = noiseLevel)
  
  thisNoise <- thisNoiseForSignal
  thisSignalPlusNoise <- thisSignal + thisNoise
  noiseOnlyData <- NoiseOnly
  
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
  ciScenario5_allSNRdiffs <- append(ciScenario5_allSNRdiffs, thisFilterSNRdiff - thisMeanSNRdiff)
  ciScenario5_allMSEdiffs <-append(ciScenario5_allMSEdiffs, thisMeanMSE - thisFilteredMSE)
}

alpha <- 0.05
Scenario5_ciMSEmu <- mean(ciScenario5_allMSEdiffs)
Scenario5_ciMSEsd <- sd(ciScenario5_allMSEdiffs)
ciScenario5_allMSEdiffs <- sort(ciScenario5_allMSEdiffs)
Scenario5_MSE_lowerci <- ciScenario5_allMSEdiffs[(alpha / 2) * nsim]
Scenario5_MSE_upperci <- ciScenario5_allMSEdiffs[(1 - (alpha / 2)) * nsim]
Scenario5_MSEci <- c(Scenario5_MSE_lowerci, Scenario5_MSE_upperci)

Scenario5_ciSNRmu <- mean(ciScenario5_allSNRdiffs)
Scenario5_ciSNRsd <- sd(ciScenario5_allSNRdiffs)
ciScenario5_allSNRdiffs <- sort(ciScenario5_allSNRdiffs)
Scenario5_SNR_lowerci <- ciScenario5_allSNRdiffs[(alpha / 2) * nsim]
Scenario5_SNR_upperci <- ciScenario5_allSNRdiffs[(1 - (alpha / 2)) * nsim]
Scenario5_SNRci <- c(Scenario5_SNR_lowerci, Scenario5_SNR_upperci)