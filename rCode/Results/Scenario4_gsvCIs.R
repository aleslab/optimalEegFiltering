# code to obtain confidence intervals for generalized singular values
# set seed for consistency of results
set.seed(123)

#initialize variable to hold output
ciScenario4_gsvalues <- vector()

# set variable lists for loops
nElec <- 64 
nTime <- 600
noiseList <-c(0.05, 0.1, 0.2, 0.4, 0.8)
nTrials <- 100
nsim <- 50

# loop to calculate noise levels
for (noiseLevel in noiseList){
  print(noiseLevel)

  # loop to generate bootstraps  
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
    
    #calculate and extract gsvalues
    theseGSvalues <- Extract_gsvalues(thisSignalPlusNoise, noiseOnlyData)
    
    #eliminate zero elements
    theseGSvalues <- theseGSvalues[which(theseGSvalues != 0)]
    
    #store relevant element
    ciScenario4_gsvalues <- append(ciScenario4_gsvalues, theseGSvalues[1])
  }
}

# divide values by noise level
ciScenario4_gsvalues.n1 <- ciScenario4_gsvalues[1:50]
ciScenario4_gsvalues.n2 <- ciScenario4_gsvalues[51:100]
ciScenario4_gsvalues.n3 <- ciScenario4_gsvalues[101:150]
ciScenario4_gsvalues.n4 <- ciScenario4_gsvalues[151:200]
ciScenario4_gsvalues.n5 <- ciScenario4_gsvalues[201:250]

#calculate confidence intervals
Scenario4_GSVci.n1 <- calc.ci(alpha = 0.05, nsim = nsim, values = ciScenario4_gsvalues.n1)
Scenario4_GSVci.n2 <- calc.ci(alpha = 0.05, nsim = nsim, values = ciScenario4_gsvalues.n2)
Scenario4_GSVci.n3 <- calc.ci(alpha = 0.05, nsim = nsim, values = ciScenario4_gsvalues.n3)
Scenario4_GSVci.n4 <- calc.ci(alpha = 0.05, nsim = nsim, values = ciScenario4_gsvalues.n4)
Scenario4_GSVci.n5 <- calc.ci(alpha = 0.05, nsim = nsim, values = ciScenario4_gsvalues.n5)