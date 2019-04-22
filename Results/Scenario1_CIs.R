# getting confidence intervals
# set seed for consistency of results
set.seed(123)

#initialize some variables to hold output
ciScenario1_allMSEdiffs <- vector()
ciScenario1_allSNRdiffs <- vector()

# set variable lists for loops
nElec <- 64 
nTime <- 600
nTrials <- 100
noiseLevel <- 60
nsim <- 50

# loop to generate results for all trial number and noise power conditions
for (i in 1:nsim) {
    print(i)
  
    #Generate a simulation
    thisSignal <-  simdata(i = 64, j = 600, k = nTrials, p = 0)
    thisWhiteNoiseForSignal <-  array(rnorm(nElec * nTime * nTrials, mean = 0, sd = .01), dim = c(nElec, nTime, nTrials))
    
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
    ciScenario1_allSNRdiffs <- append(ciScenario1_allSNRdiffs, thisFilterSNRdiff - thisMeanSNRdiff)
    ciScenario1_allMSEdiffs <-append(ciScenario1_allMSEdiffs, thisMeanMSE - thisFilteredMSE)
}

alpha <- 0.05
Scenario1_ciMSEmu <- mean(ciScenario1_allMSEdiffs)
Scenario1_ciMSEsd <- sd(ciScenario1_allMSEdiffs)
ciScenario1_allMSEdiffs <- sort(ciScenario1_allMSEdiffs)
Scenario1_MSE_lowerci <- ciScenario1_allMSEdiffs[(alpha / 2) * nsim]
Scenario1_MSE_upperci <- ciScenario1_allMSEdiffs[(1 - (alpha / 2)) * nsim]
Scenario1_MSEci <- c(Scenario1_MSE_lowerci, Scenario1_MSE_upperci)

Scenario1_ciSNRmu <- mean(ciScenario1_allSNRdiffs)
Scenario1_ciSNRsd <- sd(ciScenario1_allSNRdiffs)
ciScenario1_allSNRdiffs <- sort(ciScenario1_allSNRdiffs)
Scenario1_SNR_lowerci <- ciScenario1_allSNRdiffs[(alpha / 2) * nsim]
Scenario1_SNR_upperci <- ciScenario1_allSNRdiffs[(1 - (alpha / 2)) * nsim]
Scenario1_SNRci <- c(Scenario1_SNR_lowerci, Scenario1_SNR_upperci)

