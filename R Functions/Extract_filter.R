Extract_GSVD_filter <- function(signalnoise, noise) {
  # Purpose: to create a gsvd based filter to apply to an array containing both signal
  # and noise that leaves behind an estimate of the signal
  # Inputs:
  # signalnoise: the array containing both the signal and noise information
  # noise: an array containing just noise information
  # Output:
  # xhat: an estimate of the signal
  
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
  
  # QR decomposition of the two matrices to get correct form for GSVD
  qrNoise <-qr(noise)
  rNoise <-qr.R(qrNoise)
  
  res<-diagp(qr.Q(qrNoise),rNoise)
  qrNoise <- res$Y
  rNoise <- res$X
  
  for (i in 1:length(rNoise)) {
    if (is.nan(rNoise[i]) == TRUE | is.infinite(rNoise[i]) == TRUE | 
        is.na(rNoise[i]) == TRUE) {rNoise[i] <- 0}
  }
  
  # Extracting neccessary results from QR decomposition 
  qrSignal <- qr(signalnoise)
  rSignal  <- qr.R(qrSignal)
  res<-diagp(qr.Q(qrSignal),rSignal)
  qrSignal <- res$Y
  rSignal <- res$X
  
  
  for (i in 1:length(rSignal)) {
    if (is.nan(rSignal[i]) == TRUE | is.infinite(rSignal[i]) == TRUE | 
        is.na(rSignal[i]) == TRUE) {rSignal[i] <- 0}
  }
  
  
  #qrSignalNoise <- qr((rbind(rSignal,rNoise)))
  #Q<-qr.Q(qrSignalNoise)
  
  # Perform gsvd on two matrices
#  decomposition <- gsvd(Q[0:m,],Q[(m+1):(m+m),])
  decomposition <- gsvd(rSignal,rNoise)
  # The weighting is  1- Noise/Signal ratio
  #WARNING THE WEIGHT Does not Incorparate matrix size yet! 
  #I left that out so if more/less noise samples than signal these weights will be wrong.
  
  # Extract the generalized singular values and elimiate any non-real values 
  gsvalues <- 1-((decomposition$beta) ^ 2 / (decomposition$alpha) ^ 2)
  
  gsvalues <- gsvalues[which(is.nan(gsvalues) == FALSE & is.infinite(gsvalues) == FALSE)]
  
  #Anything with negative values gets clamped to 0.  
  gsvalues <-pmax(0,gsvalues)
  
  
  # Extract the Q matrix from the decomposition and find the inverse 
  Q <- t(gsvd.oR(decomposition) %*% t(decomposition$Q))
  Qinverse <- solve(t(Q))
  
  # Create the filter based on formula from research
  
  #  Wfilter <- t(Qinverse) * (1 - ((m/p) * gsvalues)) * t(Q)
  #The filter can be thought of in 3 steps:
  #1) Project signal onto the different filters found from gsvd (Q)
  #2) Weight these signals by the signal to noise ratio  (gsvalues)
  #3) Reconstruct the original signal incorporating the SNR weights (QInverse)
  #
  #!Incorrect!   Wfilter <-  t(Q) %*% diag(gsvalues) %*% t(Qinverse) 
  Wfilter <-  Qinverse %*% diag(gsvalues) %*% t(Q)
  
  #Need to transpose the output filter because we transpose the inputs coming into this function
  #For this filter to apply to the stuff before we transpose inside this function
  return (t(Wfilter))
}

# function [Y,X] = diagp(Y,X,k)
# % DIAGP  Diagonal positive.
# % [Y,X] = diagp(Y,X,k) scales the columns of Y and the rows of X by
# % unimodular factors to make the k-th diagonal of X real and positive.
# D = diagk(X,k);
# j = find(real(D) < 0 | imag(D) ~= 0);
# D = diag(conj(D(j))./abs(D(j)));
# Y(:,j) = Y(:,j)*D';
# X(j,:) = D*X(j,:);

diagp <- function(Y,X) {
# DIAGP  Diagonal positive.
# [Y,X] = diagp(Y,X) scales the columns of Y and the rows of X by
# unimodular factors to make the k-th diagonal of X real and positive.
D <- diag(X)

j = which( D< 0)
j = which( (Re(D) < 0) | Im(D) != 0)
D = diag(Conj(D[j])/Mod(D[j]))
Y[,j] = Y[,j] %*% t(D)
X[j,] = D %*% X[j,]
return(list(Y=Y,X=
              X))
}