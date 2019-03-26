PatternNoise <- function(i, j, k, mean = 0, sd = 1, p = 1) {
  # The purpose of this function is to generate noise that has a pattern. It takes
  # i, j, and k as inputs to determine the size of the array produced and mean and sd
  # to alter the Gaussian random noise included in the patterned noise. p is a factor that alters the power of the noise
  # It outputs an i x j x k array of Gaussian noise with a pattern of rank 1
  
  #Input checks
  if (is.numeric(i) == FALSE | is.numeric(j) == FALSE | is.numeric(k) == FALSE |
      is.numeric(mean) == FALSE | is.numeric(sd) == FALSE | is.numeric(p) == FALSE) {
    stop("Non-numeric arguments")
  }
  
  if (i <= 0 | j <= 0 | k <= 0 | sd <= 0 | p <= 0) {stop("Non-positive arguments")}
  
  if (i != round(i) | j != round(j) | k != round(k)) {stop("Non-integer arguments")}
  
  # set up results array
  N <- array(dim = c(i, j, k))
  
  # create pattern vector
  pattern <- matrix(rep(x = c(0,1), times = (i/2)), nrow = i)
  
 
# loop to generate pattern for each trial 
  for (iTrial in 1:k) {
    
  # create matrix of random noise
  GNoise <- matrix(rnorm(n = j, mean = 0, sd = 1), ncol = j)
  
  # Multiple pattern vector by random noise 
  patternN <- pattern %*% GNoise
  
  # Add pattern to trial of the array
  N[,,iTrial] <- patternN * p
  }
 
# Return matrix of patterned noise 
  return(N)
}

