GSVD_filter <- function(signalnoise, noise) {
  
  if(is.array(noise) == FALSE) stop("Invalid arguments")
  if(is.array(signalnoise) == FALSE) stop("Invalid arguments")
  if(ncol(signalnoise) != ncol(noise)) stop("Invalid argument dimensions")
  
  m <- nrow(signalnoise)
  p <- nrow(noise)
  
  decomposition <- gsvd(signalnoise[,,1], noise[,,1])
  gsvalues <- (decomposition$alpha) ^ 2 / (decomposition$beta) ^ 2
  gsvalues <- gsvalues[which(is.nan(gsvalues) == FALSE & is.infinite(gsvalues) == FALSE)]
  Q <- decomposition$Q
  Qinverse <- solve(Q)
  Wfilter <- t(Qinverse) * (1 - ((m/p) * gsvalues)) * t(Q)
  
  xhat <- signalnoise[,,1] %*% Wfilter
  return (xhat)
}

scenario.v2.filter <- GSVD_filter(scenario.v2, Noise.v2)
