PatternNoise <- function(i, j, k, mean = 0, sd = 1) {
  N <- array(dim = c(i, j, k))
  pattern <- matrix(nrow = i, ncol = j)
  for(m in 1:j){
    pattern[, m] <- sample(x = c(0,1), size = i, replace = TRUE)
  }
  N[, , 1:k] <- pattern
  Gaussian <- array(rnorm(n = (j * j * k), mean = mean, sd = sd), dim = c(j, j, k))
  for (m in 1:k){
    N[, , m] <- N[, , m] %*% Gaussian[, ,m]
  } 
  return(N)
}