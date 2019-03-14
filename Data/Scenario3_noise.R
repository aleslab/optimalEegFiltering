MultiPatternNoise <- function(i, j, k, mean = 0, sd = 1, p = 1) {
  # The purpose of this function is to generate noise that has a pattern. It takes
  # i, j, and k as inputs to determine the size of the array produced and mean and sd
  # to alter the Gaussian random noise included in the patterned noise. It outputs an
  # i x j x k array of Gaussian noise with a pattern of rank 1
  
  #Input checks
  if (is.numeric(i) == FALSE | is.numeric(j) == FALSE | is.numeric(k) == FALSE |
      is.numeric(mean) == FALSE | is.numeric(sd) == FALSE | is.numeric(p) == FALSE) {
    stop("Non-numeric arguments")
  }
  
  if (i <= 0 | j <= 0 | k <= 0 | sd <= 0 | p <= 0) {stop("Non-positive arguments")}
  
  if (i != round(i) | j != round(j) | k != round(k)) {stop("Non-integer arguments")}
  
  # set up results array
  N <- array(dim = c(i, j, k))
  
  # create pattern vectors
  pattern1 <- rep(x = c(0, 1), times = (i / 2))
  pattern2 <- rep(x = c(1, 0), times = (i / 2))
  pattern3 <- rep(x = c(1, 0, 0, 1), times = (i / 4))
  pattern4 <- rep(x = c(0, 1, 1, 0), times = (i / 4))
  pattern5 <- rep(x = c(1, 1, 0, 1), times = (i / 4))
  pattern6 <- rep(x = c(1, 1, 1, 0), times = (i / 4))
  pattern7 <- rep(x = c(0, 0, 1, 0), times = (i / 4))
  pattern8 <- rep(x = c(0, 0, 0, 1), times = (i / 4))
  pattern <- matrix(c(pattern1, pattern2, pattern3, pattern4, pattern5, pattern6,
                      pattern7, pattern8), nrow = i, ncol = 8)
  
  # create matrix of random noise
  GNoise <- matrix(rnorm(n = (8 * j), mean = 0, sd = 1), nrow = 8, ncol = j)
  
  # Multiply pattern vector by random noise 
  patternN <- pattern %*% GNoise
  
  # Add same pattern to each trial of the array
  N[,, 1:k] <- patternN * p
  
  return(N)
}

