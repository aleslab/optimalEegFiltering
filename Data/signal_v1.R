signal <- function(i, j, k) {
  # the purpose of this function is to simulate the signal of egg data. The input i is
  # the number of sensors used to record data, j is the number of time points at which 
  # data is recorded, and k is the number of trials. The function outputs array S 
  # with dimensions i x j x k.
  
  # create results objects with correct dimensions  
  S <- array(dim = c(i, j, k))
  
  # create a loop to generate the data for each trial, which at this stage are all the same  
  for (m in 1:k) {
    
    # create a signal matrix where Xt is 1 at the times the signal is anticipated (after 
    # stimulus presentation) and 0 when there is no signal. And Xs is 1 when the sensor
    # is picking up a signal and 0 when it's not. Multiply these two 1 dimensional matrices
    # together to get the overall signal matrix X
    Xt <- matrix(data = c(rep(0, times = j/3), rep(1, times = j/3), rep(0, times = j/3)),
                 nrow = 1, ncol = j)
    Xs <- matrix(data = c(rep(0, times = i/4), rep(1, times = i/2), rep(0, times = i/4)),
                 nrow = i, ncol = 1)
    X <- Xs %*% Xt
    
    # Add signal to the correct trial in an array 
    S[,,m] <- X
  }
  
  # return the array of simulated data
  return(S)
}