# code to obtain confidence intervals for generalised singular values
# set seed for consistency of results
set.seed(123)

#initialize variables to hold output
ciScenario1_gsvalues <- vector()

# set variable lists for loops
nElec <- 64 
nTime <- 600
noiseList <-c(15, 30, 60, 120, 240)
nTrials <- 100
noiseLevel <- 240
nsim <- 50

# loop to go through noise levels
for (noiseLevel in noiseList){
  print(noiseLevel)

#loop to calculate bootstraps  
  for (i in 1:nsim) {
    # print statement to keep track of function progress
    print(i)
    
    #Generate a simulation
    thisSignal <-  simdata(i = 64, j = 600, k = nTrials, p = 0)
    thisWhiteNoiseForSignal <-  array(rnorm(nElec * nTime * nTrials, mean = 0, sd = .01), dim = c(nElec, nTime, nTrials))
  
    whiteForNoiseOnly <-  array(rnorm(nElec * nTime * nTrials, mean = 0, sd = .01), dim = c(nElec, nTime, nTrials))
  
    thisNoise <- thisWhiteNoiseForSignal * noiseLevel
    thisSignalPlusNoise <- thisSignal + thisNoise
    noiseOnlyData <- whiteForNoiseOnly * noiseLevel
  
    #calculate and extract gsvalues
    theseGSvalues <- Extract_gsvalues(thisSignalPlusNoise, noiseOnlyData)
        
    #eliminate zero elements
    theseGSvalues <- theseGSvalues[which(theseGSvalues != 0)]
  
    # store first value, i.e. the value corresponding to maximum eigenvalue
    ciScenario1_gsvalues <- append(ciScenario1_gsvalues, theseGSvalues[1])
   }
}

#seperate values by noise level
ciScenario1_gsvalues.n1 <- ciScenario1_gsvalues[1:50]
ciScenario1_gsvalues.n2 <- ciScenario1_gsvalues[51:100]
ciScenario1_gsvalues.n3 <- ciScenario1_gsvalues[101:150]
ciScenario1_gsvalues.n4 <- ciScenario1_gsvalues[151:200]
ciScenario1_gsvalues.n5 <- ciScenario1_gsvalues[201:250]


# function to calculate nonparametric bootstrap confidence intervals using the
# percentile method
calc.ci <- function(alpha, nsim, values) {
  values <- values[which(values != 0)]
  values <- sort(values)
  lowerci <- values[(alpha / 2) * nsim]
  upperci <- values[(1 - (alpha / 2)) * nsim]
  ci <- c(lowerci, upperci)
  return(ci)
}

# create confidence intervals
Scenario1_GSVci.n1 <- calc.ci(alpha = 0.05, nsim = nsim, values = ciScenario1_gsvalues.n1)
Scenario1_GSVci.n2 <- calc.ci(alpha = 0.05, nsim = nsim, values = ciScenario1_gsvalues.n2)
Scenario1_GSVci.n3 <- calc.ci(alpha = 0.05, nsim = nsim, values = ciScenario1_gsvalues.n3)
Scenario1_GSVci.n4 <- calc.ci(alpha = 0.05, nsim = nsim, values = ciScenario1_gsvalues.n4)
Scenario1_GSVci.n5 <- calc.ci(alpha = 0.05, nsim = nsim, values = ciScenario1_gsvalues.n5)



