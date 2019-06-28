# Code for the second version of simulation

simdata2 <- function(i, j, k, mean = 0, sd = 1, p = 1) {
  # the purpose of this function is to simulate eeg data with noise. The input i is the
  # number of sensors used to record data, j is the number of time points at which data 
  # is recorded, and k is the number of trials. The function outputs arrays SignalNoise 
  # with dimensions i x j x k.
  
  # input checks:
  if (is.numeric(i) == FALSE | is.numeric(j) == FALSE | is.numeric(k) == FALSE |
      is.numeric(mean) == FALSE | is.numeric(sd) == FALSE | is.numeric(p) == FALSE) {
    stop("Non-numeric arguments")
  }
  
  if (i <= 0 | j <= 0 | k <= 0 | sd <= 0 | p <= 0) {stop("Non-positive arguments")}
  
  if (i != round(i) | j != round(j) | k != round(k)) {stop("Non-integer arguments")}
  
  # create results objects with correct dimensions  
  SignalNoise <- array(dim = c(i, j, k))
  
  # create a signal matrix 
  X <- signal(i = i, j = j, k = k)
  
  # create a patterned noise matrix
  Np <- PatternNoise(i = i, j = j, k = k)
  
  #create a random noise matrix
  Ng <- array(data = rnorm(n = (i * j * k), mean = mean, sd = sd), dim = c(i, j, k))
  
  # Add the signal array to the noise array
  SignalNoise <- X + ((Np + Ng) * p)
  
  # return the array of simulated data
  return(SignalNoise)
}

scenario.v2 <- simdata2(i = 64, j = 600, k = 100, p = 0.5)