# getting confidence intervals
# set seed for consistency of results
set.seed(123)

#initialize variable to hold output
ciScenario5_gsvalues <- vector()

# set variable lists for loops
nElec <- 64 
nTime <- 600
noiseList <-c(0.05, 0.1, 0.2, 0.4, 0.8)
nTrials <- 100
nsim <- 50

# loop to calculate noise levels
for (noiseLevel in noiseList){
  print(noiseLevel)
  
  #loop to generate bootstraps
  for (i in 1:nsim) {
    print(i)
    
    #Generate a simulation
    thisSignal <-  simEEGsignal(nTime = 600, Trials = nTrials)
    thisNoiseForSignal <- simEEGnoise(nTime = 600, Trials = nTrials, p = noiseLevel)
    
    NoiseOnly <-simEEGnoise(nTime = 600, Trials = nTrials, p = noiseLevel)
    
    thisNoise <- thisNoiseForSignal
    thisSignalPlusNoise <- thisSignal + thisNoise
    noiseOnlyData <- NoiseOnly
    
    #calculate and extract gsvalues
    theseGSvalues <- Extract_gsvalues(thisSignalPlusNoise, noiseOnlyData)
    
    #eliminate zero elements
    theseGSvalues <- theseGSvalues[which(theseGSvalues != 0)]
    
    #store relevant element
    ciScenario5_gsvalues <- append(ciScenario5_gsvalues, theseGSvalues[1])
  }
}

#divide elements by noise level
ciScenario5_gsvalues.n1 <- ciScenario5_gsvalues[1:50]
ciScenario5_gsvalues.n2 <- ciScenario5_gsvalues[51:100]
ciScenario5_gsvalues.n3 <- ciScenario5_gsvalues[101:150]
ciScenario5_gsvalues.n4 <- ciScenario5_gsvalues[151:200]
ciScenario5_gsvalues.n5 <- ciScenario5_gsvalues[201:250]

#calculate confidence intervals
Scenario5_GSVci.n1 <- calc.ci(alpha = 0.05, nsim = nsim, values = ciScenario5_gsvalues.n1)
Scenario5_GSVci.n2 <- calc.ci(alpha = 0.05, nsim = nsim, values = ciScenario5_gsvalues.n2)
Scenario5_GSVci.n3 <- calc.ci(alpha = 0.05, nsim = nsim, values = ciScenario5_gsvalues.n3)
Scenario5_GSVci.n4 <- calc.ci(alpha = 0.05, nsim = nsim, values = ciScenario5_gsvalues.n4)
Scenario5_GSVci.n5 <- calc.ci(alpha = 0.05, nsim = nsim, values = ciScenario5_gsvalues.n5)