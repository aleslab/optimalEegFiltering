simEEGsignal <- function(nTime, Trials) {
# The purpose of this function is generate a signal similar in structure to one
# obtained from an EEG of evoked potentials in the visual areas of the brain.
# the inputs are the number of time points, nTime, and number of Trials,
# the resulting array will contain. The output is the simulated signal.
  
# input checks 
  if (is.numeric(nTime) == FALSE | is.numeric(Trials) == FALSE) {
    stop("Non-numeric arguments")
  }
  
  if (nTime <= 0 | Trials <= 0) {stop("Non-positive arguments")}
  
  if (nTime != round(nTime) | Trials != round(Trials)) {stop("Non-integer arguments")}  
  
#Load visual evoked potential data  
  pathname <- file.path("eegForwardData", "fwdOnlyVisual.mat")
  dataSignal <- readMat(pathname)

#Load the number of signal sources (brain regions) and number of electrodes recording
  nElec <- (dim(dataSignal$fwdOnlyVisual)[1]) / 2 
  nSources <- dim(dataSignal$fwdOnlyVisual)[2]

#Generate the results array of appropraite size
  simBrainSignal <- array(dim = c(nElec, nTime, Trials))

# loop to ensure each trial had different random noise  
  for (i in 1:Trials) {

#generate random noise of the correct size to make the results matrix nElec x nTime    
    randomActivity <- array(rnorm(nSources * nTime , mean = 0, sd = 1),dim = c(nSources, nTime))
#multiply the signal by the random noise and add signal to results arrat   
     simBrainSignal[,,i] <- dataSignal$fwdOnlyVisual[1:length(nElec),] %*% randomActivity
    
  }

#return the simulated signal
  return(simBrainSignal)
}

