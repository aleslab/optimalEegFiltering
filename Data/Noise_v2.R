PatternNoise <- function(i, j, k, mean = 0, sd = 1) {
  N <- array(dim = c(i, j, k))
  pattern <- matrix(rep(x = c(0,1), times = (i/2)), nrow = i)
  GNoise <- matrix(rnorm(n = j, mean = 0, sd = 1), ncol = j)
  patternN <- pattern %*% GNoise
  N[,, 1:k] <- patternN
  return(N)
}

Noise.v2 <- PatternNoise(i = 64, j = 600, k = 100)