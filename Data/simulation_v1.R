# Code for the first version of simulation

simdata <- function(i, j, k, mean = 0, sd = 1) {
# the purpose of this function is to simulate eeg data with noise. The input i is the
# number of sensors used to record data, j is the number of time points at which data 
# is recorded, and k is the number of trials. The function outputs arrays SignalNoise 
# with dimensions i x j x k.

# create results objects with correct dimensions  
  SignalNoise <- array(dim = c(i, j, k))
  
  # create a signal matrix 
  X <- signal(i = i, j = j, k = k)
  
# create a loop to generate the data for each trial, which at this stage are all the same  
  for (m in 1:k) {

# create an i x j matrix of Gaussian random noise    
    N <- matrix(rnorm(i * j, mean = mean, sd = sd), nrow = i, ncol = j)

# Store the noise matrix in the correct trial of the array  
    SignalNoise[,,m] <- N
  }

# Add the signal array to the noise array
  SignalNoise <- SignalNoise + X
  
# return the array of simulated data
return(SignalNoise)
}

scenario.v1 <- simdata(i = 64, j = 600, k = 100)