set.seed(123)

#initialize some variables to hold output
Scenario2_allGsvdMse <-vector()
Scenario2_allGsvdDistortion <- vector()
Scenario2_allMeanMse <- vector()
Scenario2_allGsvdResidualNoise <- vector()
Scenario2_allGsvdOriginalNoise <- vector()
Scenario2_allGsvdInSNR <- vector()
Scenario2_allGsvdOutSNR <- vector()
Scenario2_allGsvdSDI <- vector()
Scenario2_allGsvdNRfactor <- vector()

sdiDataFrame <- data.frame()
nElec <- 64 
nTime <- 600
trialList <-c(50,100,500)
noiseList <-c(1,10)
for (nTrials in trialList) {
  for (noiseLevel in noiseList) {
    #for (nTrials in c(25, 50, 100, 200, 400)) {
     # for (noiseLevel in c(.05, .50, 1, 2)) {
        print(c(nTrials, noiseLevel))
    #Generate a simulation
    #thisSignal <-  simdata(i = 64, j = 600, k = nTrials, p = noiseLevel)
    thisSignal <-  simdata(i = 64, j = 600, k = nTrials, p = 0)
    thisPatternNoiseForSignal <- PatternNoise(i = 64, j = 600, k = nTrials, p = noiseLevel)
    thisWhiteNoiseForSignal <-  array(rnorm(nElec * nTime * nTrials, mean = 0, sd = .01), dim = c(nElec, nTime, nTrials))
    
    patternForNoiseOnly <- PatternNoise(i = 64, j = 600, k = nTrials, p = noiseLevel)
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
    Scenario2_allGsvdInSNR <- append(Scenario2_allGsvdInSNR, thisInSNR)
    Scenario2_allGsvdOutSNR <- append(Scenario2_allGsvdOutSNR, thisOutSNR)
    Scenario2_allGsvdSDI <- append(Scenario2_allGsvdSDI, thisSDI)
    Scenario2_allGsvdNRfactor <- append(Scenario2_allGsvdNRfactor, thisNRfactor)
    Scenario2_allGsvdMse <-append(Scenario2_allGsvdMse, thisFilteredMSE)
    Scenario2_allGsvdDistortion <-append(Scenario2_allGsvdDistortion, thisDistortion)
    Scenario2_allGsvdResidualNoise <- append(Scenario2_allGsvdResidualNoise, thisResidual)
    Scenario2_allGsvdOriginalNoise <- append(Scenario2_allGsvdOriginalNoise, thisOriginal)
    
    #Calculate the values if NO filtering is applied
    thisMeanMSE <- mse(thisSignalPlusNoiseMean,thisSignalMean)
    
    #Distortion doesn't make sense to do. Since no filtering is applied by definition
    #no distortion. 
    #thisDistortion <- mse(thisSignalMean,thisSignalMean)
    Scenario2_allMeanMse <-append(Scenario2_allMeanMse,thisMeanMSE)
    
    #Buid data frame on each iteration. 
    sdiDataFrame<-rbind(sdiDataFrame,
                        data.frame("Method"="gsvd",
                                   "Trials"=nTrials,
                                   "NoiseLevel" =noiseLevel,
                                   "SDI"= thisSDI))
  }
  
}

Scenario2_percentDistortion <- (Scenario2_allGsvdDistortion / (Scenario2_allGsvdDistortion + Scenario2_allGsvdResidualNoise)) * 100
Scenario2_percentResidual <- (Scenario2_allGsvdResidualNoise / (Scenario2_allGsvdDistortion + Scenario2_allGsvdResidualNoise)) * 100



# i <- 1
# Scenario2_NoiseMeanPercentDistortion <- vector()
# while (i <= length(Scenario2_percentDistortion)) {
#   Scenario2_NoiseMeanPercentDistortion <- append(Scenario2_NoiseMeanPercentDistortion, mean(Scenario2_percentDistortion[i: (i + 3)]))
#   i <- i + 4
# }
# 
# i <- 1
# Scenario2_NoiseMeanPercentResidual <- vector()
# while (i <= length(Scenario2_percentResidual)) {
#   Scenario2_NoiseMeanPercentResidual <- append(Scenario2_NoiseMeanPercentResidual, mean(Scenario2_percentResidual[i: (i + 3)]))
#   i <- i + 4
# }
# 
# Scenario2_TrialMeanPercentDistortion <- vector()
# i <- 1
# for (i in 1:4) {
#   values <- c(Scenario2_percentDistortion[i], Scenario2_percentDistortion[i + 4], Scenario2_percentDistortion[i + 8], Scenario2_percentDistortion[i + 12], Scenario2_percentDistortion[i + 16])
#   Scenario2_TrialMeanPercentDistortion <- append(Scenario2_TrialMeanPercentDistortion, mean(values))
# }
# 
# Scenario2_TrialMeanPercentResidual <- vector()
# i <- 1
# for (i in 1:4) {
#   values <- c(Scenario2_percentResidual[i], Scenario2_percentResidual[i + 4], Scenario2_percentResidual[i + 8], Scenario2_percentResidual[i + 12], Scenario2_percentResidual[i + 16])
#   Scenario2_TrialMeanPercentResidual <- append(Scenario2_TrialMeanPercentResidual, mean(values))
# }

# Compile tables

methodLabels <- rep(c("Average", "Filter"), each = length(trialList)*length(noiseList))
noiseValues <- rep(noiseList, times = 2*length(trialList))
trialNumbers <- rep(x = trialList, each = length(noiseList), times = 2)
MSE <- c(Scenario2_allMeanMse, Scenario2_allGsvdMse)

Scenario2.table <- data.frame("Method"=methodLabels,
                              "Noise"=noiseValues,
                              "Trials"=trialNumbers,
                              "MSE"=MSE)


# Scenario2.Error <- matrix(nrow = length(trialList)*2, ncol = 3)
# Scenario2.Error[,1] <- rep(c("Distortion", "Residual Noise"), each = length(trialList))
# Scenario2.Error[,2] <- rep(trialList, times = 2)
# Scenario2.Error[,3] <- c(Scenario2_NoiseMeanPercentDistortion, Scenario2_NoiseMeanPercentResidual)
# Scenario2.Error <- as.data.frame(Scenario2.Error)
# colnames(Scenario2.Error) <- c("Partial", "Trials", "Error")
# 
# Scenario2.Error2 <- matrix(nrow = length(noiseList)*2, ncol = 3)
# Scenario2.Error2[,1] <- rep(c("Distortion", "Residual Noise"), each = 4)
# Scenario2.Error2[,2] <- rep(noiseList, times = 2)
# Scenario2.Error2[,3] <- c(Scenario2_TrialMeanPercentDistortion, Scenario2_TrialMeanPercentResidual)
# Scenario2.Error2 <- as.data.frame(Scenario2.Error2)
# colnames(Scenario2.Error2) <- c("Partial", "Noise", "Error")

Scenario2.SDI <- matrix(nrow = length(trialList)*length(noiseList), ncol = 3)
Scenario2.SDI[,1] <- rep(trialList, each = length(noiseList))
Scenario2.SDI[,2] <- rep(noiseList, times = length(trialList))
Scenario2.SDI[,3] <- Scenario2_allGsvdSDI
Scenario2.SDI <- as.data.frame(Scenario2.SDI)
colnames(Scenario2.SDI) <- c("Trials", "Noise", "SDI")

Scenario2.NR <- matrix(nrow = length(trialList)*length(noiseList), ncol = 3)
Scenario2.NR[,1] <- rep(trialList, each = length(noiseList))
Scenario2.NR[,2] <- rep(noiseList, times = length(trialList))
Scenario2.NR[,3] <- Scenario2_allGsvdNRfactor
Scenario2.NR <- as.data.frame(Scenario2.NR)
colnames(Scenario2.NR) <- c("Number of Trials", "Noise Level", "Noise Reduction Factor")

Scenario2.SNR <- matrix(nrow = length(trialList)*length(noiseList), ncol = 4)
Scenario2.SNR[,1] <- rep(trialList, each = length(noiseList))
Scenario2.SNR[,2] <- rep(noiseList, times =  length(trialList))
Scenario2.SNR[,3] <- Scenario2_allGsvdInSNR
Scenario2.SNR[,4] <- Scenario2_allGsvdOutSNR
Scenario2.SNR <- as.data.frame(Scenario2.SNR)
colnames(Scenario2.SNR) <- c("Number of Trials", "Noise Level", "Input SNR", "Output SNR")