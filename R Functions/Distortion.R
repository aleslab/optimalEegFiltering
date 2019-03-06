distortion <- function(signal, signalnoise, noise) {
  
  filter <- Extract_GSVD_filter(signalnoise, noise)
  
  origDim <- dim(signal)
  dim(signal) <- c(origDim[1], origDim[2] * origDim[3])
  signal <- t(signal)
  
  filtered_signal <- signal %*% filter
  filtered_signal <- t(filtered_signal)
  dim(filtered_signal) <- origDim
  
  average_filter <- rowMeans(filtered_signal)
  
  d <- mean((average_filter - signal)^2)
  
  return(d)
}
