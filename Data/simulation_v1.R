# Code for the first version of simulation

simdata <- function(i, j, k, mean = 0, sd = 1, p = 1) {
# the purpose of this function is to simulate eeg data with noise. The input i is the
# number of sensors used to record data, j is the number of time points at which data 
# is recorded, and k is the number of trials. The mean and sd allow you to alter the 
# distribution of the Gaussian noise and p is a factor to alter the power of the noise
# The function outputs arrays SignalNoise with dimensions i x j x k.

# input checks
  if (is.numeric(i) == FALSE | is.numeric(j) == FALSE | is.numeric(k) == FALSE |
      is.numeric(mean) == FALSE | is.numeric(sd) == FALSE | is.numeric(p) == FALSE) {
        stop("Non-numeric arguments")
  }
  
  if (i <= 0 | j <= 0 | k <= 0 | sd <= 0 ) {stop("Non-positive arguments")}
  
  if (p < 0) {stop("Noise cannot be negative")}
  
  if (i != round(i) | j != round(j) | k != round(k)) {stop("Non-integer arguments")}

# create results objects with correct dimensions  
  SignalNoise <- array(dim = c(i, j, k))
  
# create a signal matrix 
  X <- signal(i = i, j = j, k = k)

# create an appropriately sized array of Gaussian random noise    
  N <- array(rnorm(i * j * k, mean = mean, sd = sd), dim = c(i, j, k))

# Add the noise to the array  
  SignalNoise <- N * p

# Add the signal array to the noise array
  SignalNoise <- SignalNoise + X
  
# return the array of simulated data
return(SignalNoise)
}
