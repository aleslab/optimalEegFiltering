simEEGnoise <- function(nTime, Trials, p) {
  # The purpose of this function is generate noise similar to background noise
  # in an EEG recording by generating noise from all areas of the  brain.
  # the inputs are the number of time points, nTime, and number of Trials,
  # the resulting array will contain. The output is the simulated noise.

  #input checks  
  if (is.numeric(nTime) == FALSE | is.numeric(Trials) == FALSE | is.numeric(p) == FALSE) {
    stop("Non-numeric arguments")
  }
  
  if (nTime <= 0 | Trials <= 0 | p <= 0) {stop("Non-positive arguments")}
  
  if (nTime != round(nTime) | Trials != round(Trials)) {stop("Non-integer arguments")}  
  
  #Load noise data recorded from all brain regions 
  pathname <- file.path("eegForwardData", "fwdAllBrain.mat")
  dataNoise <- readMat(pathname)
  
  #Load the number of signal sources (brain regions) and number of electrodes recording
  nElec <- (dim(dataNoise$fwdAllBrain)[1]) / 2
  nNoiseSources <- dim(dataNoise$fwdAllBrain)[2]
  
  #Generate the results array of appropraite size
  simBrainNoise <- array(dim = c(nElec, nTime, Trials))
  
  # loop to ensure each trial had different random noise  
  for (i in 1:Trials) {
    
    #generate random noise of the correct size to make the results matrix nElec x nTime    
    randomActivity <- array(rnorm(nNoiseSources * nTime , mean = 0, sd = 1),dim = c(nNoiseSources, nTime))
    #multiply the noise by the random noise and add to results array   
    simBrainNoise[,,i] <- dataNoise$fwdAllBrain[1:length(nElec),] %*% randomActivity
    
  }
  
  #return the simulated noise
  return(simBrainNoise * p)
}