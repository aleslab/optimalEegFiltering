# Code for the second version of simulation

simdata2 <- function(i, j, k) {
  # the purpose of this function is to simulate eeg data with noise. The input i is the
  # number of sensors used to record data, j is the number of time points at which data 
  # is recorded, and k is the number of trials. The function outputs arrays SignalNoise 
  # with dimensions i x j x k.
  
  # create results objects with correct dimensions  
  SignalNoise <- array(dim = c(i, j, k))
  
  # create a signal matrix 
  X <- signal(i = i, j = j, k = k)
  
  N <- PatternNoise(i = i, j = j, k = k)
  
  # Add the signal array to the noise array
  SignalNoise <- N + X
  
  # return the array of simulated data
  return(SignalNoise)
}