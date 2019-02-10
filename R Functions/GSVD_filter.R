GSVD_filter <- function(signalnoise, noise) {
# Purpose: to create a gsvd based filter to apply to an array containing both signal
#  and noise that leaves behind an estimate of the signal
# Inputs:
# signalnoise: the array containing both the signal and noise information
# noise: an array containing just noise information
# Output:
# xhat: an estimate of the signal

# Input checks
  if(is.array(noise) == FALSE) stop("Invalid arguments")
  if(is.array(signalnoise) == FALSE) stop("Invalid arguments")
  if(ncol(signalnoise) != ncol(noise)) stop("Invalid argument dimensions")
  
  m <- nrow(signalnoise)
  p <- nrow(noise)

# Decompose the two matrices using gsvd  
  decomposition <- gsvd(signalnoise[,,1], noise[,,1])

# Extract the generalized singular values and elimiate any non-real values
  gsvalues <- (decomposition$alpha) ^ 2 / (decomposition$beta) ^ 2
  gsvalues <- gsvalues[which(is.nan(gsvalues) == FALSE & is.infinite(gsvalues) == FALSE)]

# Extract the Q matrix from the decomposition and find the inverse 
  Q <- decomposition$Q
  Qinverse <- solve(Q)

# Create the filter based on formula from research
  Wfilter <- t(Qinverse) * (1 - ((m/p) * gsvalues)) * t(Q)

# Apply filter to data and return signal estimate  
  xhat <- signalnoise[,,1] %*% Wfilter
  return (xhat)
}

scenario.v2.filter <- GSVD_filter(scenario.v2, Noise.v2)
