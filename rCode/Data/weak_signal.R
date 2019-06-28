weak_signal <- function(i, j, k) {
  # the purpose of this function is to simulate the signal of eeg data. The input i is
  # the number of sensors used to record data, j is the number of time points at which 
  # data is recorded, and k is the number of trials. The function outputs array S 
  # with dimensions i x j x k.
  
  # Input checks
  if (is.numeric(i) == FALSE | is.numeric(j) == FALSE | is.numeric(k) == FALSE) {
    stop("Non-numeric arguments")
  }
  if (i <= 0 | j <= 0 | k <= 0) {stop("Non-positive arguments")}
  if (i != round(i) | j != round(j) | k != round(k)) {stop("Non-integer arguments")}
  if (j/3 != round(j/3)) {stop("Number of time points not divisible by 3")}
  if (i/4 != round(i/4)) {stop("Number of sensors not divisible by 4")}
  
  # create results objects with correct dimensions  
  S <- array(dim = c(i, j, k))
  
  # create a signal matrix where Xt is 1 at the times the signal is anticipated (after 
  # stimulus presentation) and 0 when there is no signal. And Xs is 1 when the sensor
  # is picking up a signal and 0 when it's not. Multiply these two 1 dimensional matrices
  # together to get the overall signal matrix X
  Xt <- matrix(data = c(rep(0, times = j/3), rep(0.5, times = j/3), rep(0, 
                                                                        times = j/3)), nrow = 1, ncol = j)
  Xs <- matrix(data = c(rep(0, times = i/4), rep(0.5, times = i/2), rep(0, 
                                                                        times = i/4)), nrow = i, ncol = 1)
  X <- Xs %o% Xt
  
  # Add signal to the each trial in the results array 
  S[,,1:k] <- X
  
  # return the array of simulated signal
  return(S)
}

# Code for the first simulation scenario

alt_simdata <- function(i, j, k, mean = 0, sd = 1, p = 1) {
  # the purpose of this function is to simulate eeg data with random noise. The input i is the
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
  X <- weak_signal(i = i, j = j, k = k)
  
  # create an appropriately sized array of Gaussian random noise    
  N <- array(rnorm(i * j * k, mean = mean, sd = sd), dim = c(i, j, k))
  
  # Add the noise multiplied by the noise power factor to the array  
  SignalNoise <- N * p
  
  # Add the signal array to the noise array
  SignalNoise <- SignalNoise + X
  
  # return the array of simulated data
  return(SignalNoise)
}
