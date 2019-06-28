Residual_noise <- function(signalnoise, noise) {
  
  filter <- Extract_GSVD_filter(signalnoise, noise)
  
  origDim <- dim(noise)
  dim(noise) <- c(origDim[1], origDim[2] * origDim[3])
  noise <- t(noise)
  
  filtered_noise <- noise %*% filter
  filtered_noise <- t(filtered_noise)
  dim(filtered_noise) <- origDim
  
  average_noise <- rowMeans(filtered_noise)
  
  rnp <- mean(average_noise^2)
  
  return(rnp)
}

Original_noise<- function(noise) {
  
  average_noise <- rowMeans(noise)
  
  np <- mean(average_noise^2)
  
  return(np)
}

