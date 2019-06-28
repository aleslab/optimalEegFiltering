Extract_gsvalues <- function(signalnoise, noise) {
  # Purpose: to create a gsvd based filter to apply to an array containing both signal
  # and noise that leaves behind an estimate of the signal
  # Inputs:
  # signalnoise: the array containing both the signal and noise information
  # noise: an array containing just noise information
  # Output:
  # wfilter: the wiener filter to be applied to simulated data
  
  # Input checks
  if(is.array(noise) == FALSE) stop("Invalid arguments")
  if(is.array(signalnoise) == FALSE) stop("Invalid arguments")
  if(nrow(signalnoise) != nrow(noise)) stop("Invalid argument dimensions")
  
  m <- nrow(signalnoise)
  p <- nrow(noise)
  
  # altering dimensions of inputs to be in correct form for decomposition
  origDim <- dim(signalnoise)
  dim(signalnoise) <- c(origDim[1], origDim[2] * origDim[3])
  signalnoise <- t(signalnoise)
  
  origDim <- dim(noise)
  dim(noise) <- c(origDim[1], origDim[2] * origDim[3])
  noise <- t(noise)
  
  # QR decomposition on noise matrix to get correct form for GSVD
  qrNoise <- qr(noise)
  rNoise <- qr.R(qrNoise)
  
  res <- diagp(qr.Q(qrNoise), rNoise)
  qrNoise <- res$Y
  rNoise <- res$X
  
  
  # Setting infinite or NA values to 0  
  for (i in 1:length(rNoise)) {
    if (is.nan(rNoise[i]) == TRUE | is.infinite(rNoise[i]) == TRUE | 
        is.na(rNoise[i]) == TRUE) {rNoise[i] <- 0}
  }
  
  #QR decomposition of signal matrix to get correct form for GSVD
  qrSignal <- qr(signalnoise)
  rSignal  <- qr.R(qrSignal)
  
  res<-diagp(qr.Q(qrSignal), rSignal)
  qrSignal <- res$Y
  rSignal <- res$X
  
  
  # Setting infinite and NA values to 0  
  for (i in 1:length(rSignal)) {
    if (is.nan(rSignal[i]) == TRUE | is.infinite(rSignal[i]) == TRUE | 
        is.na(rSignal[i]) == TRUE) {rSignal[i] <- 0}
  }
  
  # Perform gsvd on two altered matrices
  decomposition <- gsvd(rSignal, rNoise)
  # The weighting is  1- Noise/Signal ratio
  
  # Extract the generalized singular values and elimiate any non-real values 
  gsvalues <- 1 - ((m / p) * ((decomposition$beta) ^ 2 / (decomposition$alpha) ^ 2))
  
  gsvalues <- gsvalues[which(is.nan(gsvalues) == FALSE & is.infinite(gsvalues) == FALSE)]
  
  #Anything with negative values gets clamped to 0.  
  gsvalues <-pmax(0, gsvalues)
  
  # Return transposed filter because data was transposed going into the function
  return (gsvalues)
}