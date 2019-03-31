library(geigen)

signal <- function(i, j, k) {
  # the purpose of this function is to simulate the signal of eeg data. The input i is
  # the number of sensors used to record data, j is the number of time points at which 
  # data is recorded, and k is the number of trials. The function outputs array S 
  # with dimensions i x j x k.
  
  # Input checks
  if (is.numeric(i) == FALSE | is.numeric(j) == FALSE | is.numeric(k) == FALSE) {
    stop("Non-numeric arguments")
  }
  if (i <= 0 | j <= 0 | k <= 0) {stop("Non-positive arguments")}
  if (i != round(i) | j != round(j) | k != round(k)) {stop("Non-integer arguments")}
  if (j/3 != round(j/3)) {stop("Number of time points not divisible by 3")}
  if (i/4 != round(i/4)) {stop("Number of sensors not divisible by 4")}
  
  # create results objects with correct dimensions  
  S <- array(dim = c(i, j, k))
  
  # create a signal matrix where Xt is 1 at the times the signal is anticipated (after 
  # stimulus presentation) and 0 when there is no signal. And Xs is 1 when the sensor
  # is picking up a signal and 0 when it's not. Multiply these two 1 dimensional matrices
  # together to get the overall signal matrix X
  Xt <- matrix(data = c(rep(0, times = j/3), rep(1, times = j/3), rep(0, 
                                                                      times = j/3)), nrow = 1, ncol = j)
  Xs <- matrix(data = c(rep(0, times = i/4), rep(1, times = i/2), rep(0, 
                                                                      times = i/4)), nrow = i, ncol = 1)
  X <- Xs %o% Xt
  
  # Add signal to the each trial in the results array 
  S[,,1:k] <- X
  
  # return the array of simulated signal
  return(S)
}

# Simulation function

simdata <- function(i, j, k, mean = 0, sd = 1, p = 1) {
  # the purpose of this function is to simulate eeg data with random noise. The input i is the
  # number of sensors used to record data, j is the number of time points at which data 
  # is recorded, and k is the number of trials. The mean and sd allow you to alter the 
  # distribution of the Gaussian noise and p is a factor to alter the power of the noise
  # The function outputs arrays SignalNoise with dimensions i x j x k.
  
  # input checks
  if (is.numeric(i) == FALSE | is.numeric(j) == FALSE | is.numeric(k) == FALSE |
      is.numeric(mean) == FALSE | is.numeric(sd) == FALSE | is.numeric(p) == FALSE) {
    stop("Non-numeric arguments")
  }
  
  if (i <= 0 | j <= 0 | k <= 0 | sd <= 0 ) {stop("Non-positive arguments")}
  
  if (p < 0) {stop("Noise cannot be negative")}
  
  if (i != round(i) | j != round(j) | k != round(k)) {stop("Non-integer arguments")}
  
  # create results objects with correct dimensions  
  SignalNoise <- array(dim = c(i, j, k))
  
  # create a signal matrix 
  X <- signal(i = i, j = j, k = k)
  
  # create an appropriately sized array of Gaussian random noise    
  N <- array(rnorm(i * j * k, mean = mean, sd = sd), dim = c(i, j, k))
  
  # Add the noise multiplied by the noise power factor to the array  
  SignalNoise <- N * p
  
  # Add the signal array to the noise array
  SignalNoise <- SignalNoise + X
  
  # return the array of simulated data
  return(SignalNoise)
}

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
  
  if (i <= 0 | j <= 0 | k <= 0 | sd <= 0) {stop("Non-positive arguments")}
  
  if (p < 0) {stop("Noise cannot be non-negative")}
  
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
  
  # loop to generate pattern for each trial 
  for (iTrial in 1:k) {
    
    # create matrix of random noise
    GNoise <- matrix(rnorm(n = j * 8, mean = 0, sd = 1), nrow = 8, ncol = j)
    
    # Multiple pattern vector by random noise 
    patternN <- pattern %*% GNoise
    
    # Add pattern to trial of the array
    N[,,iTrial] <- patternN * p
    
  }
  
  # Return Rank 8 patterend noise
  return(N)
}

SpatialTemporalNoise <- function(i, j, k, mean = 0, sd = 1, p = 1) {
  # The purpose of this function is to generate noise that has a pattern. It takes
  # i, j, and k as inputs to determine the size of the array produced and mean and sd
  # to alter the Gaussian random noise included in the patterned noise. It outputs an
  # i x j x k array of Gaussian noise with a pattern of rank 1
  
  #Input checks
  if (is.numeric(i) == FALSE | is.numeric(j) == FALSE | is.numeric(k) == FALSE |
      is.numeric(mean) == FALSE | is.numeric(sd) == FALSE | is.numeric(p) == FALSE) {
    stop("Non-numeric arguments")
  }
  
  if (i <= 0 | j <= 0 | k <= 0 | sd <= 0) {stop("Non-positive arguments")}
  
  if (p < 0) {stop("Noise cannot be non-negative")}
  
  if (i != round(i) | j != round(j) | k != round(k)) {stop("Non-integer arguments")}
  
  # set up results array
  N <- array(dim = c(i, j, k))
  
  # create pattern vectors
  s1 <- rep(x = c(0, 1), times = (i / 2))
  s2 <- rep(x = c(1, 0), times = (i / 2))
  s3 <- rep(x = c(1, 0, 0, 1), times = (i / 4))
  s4 <- rep(x = c(0, 1, 1, 0), times = (i / 4))
  s5 <- rep(x = c(1, 1, 0, 1), times = (i / 4))
  s6 <- rep(x = c(1, 1, 1, 0), times = (i / 4))
  s7 <- rep(x = c(0, 0, 1, 0), times = (i / 4))
  s8 <- rep(x = c(0, 0, 0, 1), times = (i / 4))
  spattern <- matrix(c(s1, s2, s3, s4, s5, s6, s7, s8), nrow = i, ncol = 8)
  
  t1 <- rep(x = c(0, 1), times = (j / 2))
  t2 <- rep(x = c(1, 0), times = (j / 2))
  t3 <- rep(x = c(1, 0, 0, 1), times = (j / 4))
  t4 <- rep(x = c(0, 1, 1, 0), times = (j / 4))
  t5 <- rep(x = c(1, 1, 0, 1), times = (j / 4))
  t6 <- rep(x = c(1, 1, 1, 0), times = (j / 4))
  t7 <- rep(x = c(0, 0, 1, 0), times = (j / 4))
  t8 <- rep(x = c(0, 0, 0, 1), times = (j / 4))
  tpattern <- matrix(c(t1, t2, t3, t4, t5, t6, t7, t8), nrow = 8, ncol = j, byrow = TRUE)
  
  # loop to generate pattern for each trial 
  for (iTrial in 1:k) {
    
    # create matrix of random noise
    GNoise <- matrix(rnorm(n = (i * j), mean = 0, sd = 1), nrow = i, ncol = j)
    
    # Multiple pattern vector by random noise 
    patternN <- (spattern %*% tpattern) + GNoise
    
    # Add pattern to trial of the array
    N[,,iTrial] <- patternN * p
    
  }  
  
  # Return matrix of spatial-temporal pattern noise    
  return(N)
}

Extract_GSVD_filter <- function(signalnoise, noise) {
  # Purpose: to create a gsvd based filter to apply to an array containing both signal
  # and noise that leaves behind an estimate of the signal
  # Inputs:
  # signalnoise: the array containing both the signal and noise information
  # noise: an array containing just noise information
  # Output:
  # wfilter: the weiner filter to be applied to simulated data
  
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
  qrNoise <-qr(noise)
  rNoise <-qr.R(qrNoise)
  
  res<-diagp(qr.Q(qrNoise),rNoise)
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
  res<-diagp(qr.Q(qrSignal),rSignal)
  qrSignal <- res$Y
  rSignal <- res$X
  
  # Setting infinite and NA values to 0  
  for (i in 1:length(rSignal)) {
    if (is.nan(rSignal[i]) == TRUE | is.infinite(rSignal[i]) == TRUE | 
        is.na(rSignal[i]) == TRUE) {rSignal[i] <- 0}
  }
  
  # Perform gsvd on two altered matrices
  decomposition <- gsvd(rSignal,rNoise)
  # The weighting is  1- Noise/Signal ratio
  #WARNING THE WEIGHT Does not Incorparate matrix size yet! 
  #I left that out so if more/less noise samples than signal these weights will be wrong.
  
  # Extract the generalized singular values and elimiate any non-real values 
  gsvalues <- 1-((decomposition$beta) ^ 2 / (decomposition$alpha) ^ 2)
  
  gsvalues <- gsvalues[which(is.nan(gsvalues) == FALSE & is.infinite(gsvalues) == FALSE)]
  
  #Anything with negative values gets clamped to 0.  
  gsvalues <-pmax(0,gsvalues)
  
  
  # Extract the R matrix from the decomposition and find the inverse 
  Q <- t(gsvd.oR(decomposition) %*% t(decomposition$Q))
  Qinverse <- solve(t(Q))
  
  # Create the filter based on formula from research
  Wfilter <-  Qinverse %*% diag(gsvalues) %*% t(Q)
  
  # Return transposed filter because data was transposed going into the function
  return (t(Wfilter))
}

diagp <- function(Y,X) {
  # DIAGP  Diagonal positive.
  # The purpose of this function is to scale the columns of Y and the rows of X by
  # unimodular factors to make the k-th diagonal of X real and positive.
  D <- diag(X)
  
  j = which(D < 0)
  j = which((Re(D) < 0) | Im(D) != 0)
  D = diag(Conj(D[j]) / Mod(D[j]))
  
  Y[,j] = Y[,j] %*% t(D)
  X[j,] = D %*% X[j,]
  
  return(list(Y = Y, X = X))
}

applyFilterTo3dData <- function(filter, signal) {
  #Purpose of this function is to apply the filter to a 3-D array of data to save
  # time when calculating results. Inputs are the filter and the object the filter 
  # will be apploed to. Outputs the filtered data.
  
  #Data is assumed ordered sensor x time x trial
  
  #Alter dimensions of data for correct shape  
  origDim <- dim(signal)
  dim(signal) <- c(origDim[1], origDim[2] * origDim[3])
  
  # Apply filter  
  filtered_signal <- filter %*% signal 
  
  # Return data to original 3d structure
  dim(filtered_signal) <-c(origDim[1], origDim[2], origDim[3])
  
  # Return filtered data  
  return(filtered_signal)
  
}

mse <- function(y_est, y_true) {
  # The purpose of this function is to calculate the mean squared error of two objects
  # The inputs are two objects for the MSE to be calculated for, one being the "true" 
  # value and the other being an estimate. The output is a value that quantifies the 
  # mean squared error
  
  # average the squared difference of the estimate and the true value
  error <- mean((y_est - y_true)^2)
  
  # return MSE value
  return(error)
}

# set seed for consistency of results
set.seed(123)

#initialize some variables to hold output
Scenario1_allGsvdMse <-data.frame()
Scenario1_allMeanMse <- data.frame()
Scenario1_allGsvdError <- data.frame()
Scenario1_allGsvdSNR <- data.frame()
Scenario1_allGsvdSDI <- data.frame()
Scenario1_allGsvdNRfactor <- data.frame()
Scenario1_allGsvdSNRDiff <- data.frame()

# set variable lists for loops
nElec <- 64 
nTime <- 600
trialList <-c(25, 50, 100, 200, 400)
noiseList <-c(5, 10, 20)

# loop to generate results for all trial number and noise power conditions
for (nTrials in trialList) {
  for (noiseLevel in noiseList) {
    #print statement to keep track of function running
    print(c(nTrials, noiseLevel))
    
    #Generate a simulation
    thisSignal <-  simdata(i = 64, j = 600, k = nTrials, p = 0)
    thisWhiteNoiseForSignal <-  array(rnorm(nElec * nTime * nTrials, mean = 0, sd = .01), dim = c(nElec, nTime, nTrials))
    
    whiteForNoiseOnly <-  array(rnorm(nElec * nTime * nTrials, mean = 0, sd = .01), dim = c(nElec, nTime, nTrials))
    
    thisNoise <- thisWhiteNoiseForSignal
    thisSignalPlusNoise <- thisSignal + thisNoise
    noiseOnlyData <- whiteForNoiseOnly
    
    #Filter data
    filter <- Extract_GSVD_filter(thisSignalPlusNoise, noiseOnlyData)
    filteredSignalPlusNoise <- applyFilterTo3dData(filter, thisSignalPlusNoise)
    filteredSignal <- applyFilterTo3dData(filter, thisSignal)
    filteredNoise <-  applyFilterTo3dData(filter, thisNoise)
    
    #Take mean over trials.  
    thisSignalMean <- rowMeans(thisSignal, dims = 2)
    thisSignalPlusNoiseMean <- rowMeans(thisSignalPlusNoise, dims = 2)
    thisNoiseMean <- rowMeans(thisNoise, dims = 2)
    filteredSignalPlusNoise<- rowMeans(filteredSignalPlusNoise, dims = 2) 
    filteredSignal <- rowMeans(filteredSignal, dims = 2) 
    filteredSignalNoise <- rowMeans(filteredNoise, dims = 2) 
    
    #Calculate measures about the GSVD filtered data
    thisFilteredMSE <- mse(filteredSignalPlusNoise, thisSignalMean)
    thisDistortion <- mse(filteredSignal, thisSignalMean)
    thisResidual <- mean(filteredSignalNoise^2)
    thisOriginal <- mean(thisNoiseMean^2)
    thisInSNR <- mean((thisSignal^2) / (thisNoise^2))
    thisOutSNR <- mean((filteredSignal^2) / thisResidual)
    thisSDI <- thisDistortion / mean(thisSignal^2)
    thisNRfactor <- thisOriginal / thisResidual
    
    #Store measures in data frames for later graphing
    Scenario1_allGsvdSNR <- rbind(Scenario1_allGsvdSNR, data.frame("Condition" =
                                                                     "Input", "Trials" = nTrials, "NoiseLevel" = noiseLevel,
                                                                   "SNR" = thisInSNR))
    Scenario1_allGsvdSNR <- rbind(Scenario1_allGsvdSNR, data.frame("Condition" =
                                                                     "Output", "Trials" = nTrials, "NoiseLevel" = noiseLevel,
                                                                   "SNR" = thisOutSNR))
    Scenario1_allGsvdSNRDiff <- rbind(Scenario1_allGsvdSNRDiff, data.frame("Trials" = nTrials,
                                                                           "NoiseLevel" = noiseLevel, "Difference" = thisInSNR - thisOutSNR))
    Scenario1_allGsvdSDI <- rbind(Scenario1_allGsvdSDI, data.frame("Trials" = nTrials, 
                                                                   "NoiseLevel" = noiseLevel, "SDI"= thisSDI))
    Scenario1_allGsvdNRfactor <- rbind(Scenario1_allGsvdNRfactor, data.frame("Trials" = nTrials, 
                                                                             "NoiseLevel" = noiseLevel, "NoiseReduction" = thisNRfactor))
    Scenario1_allGsvdMse <-rbind(Scenario1_allGsvdMse, data.frame("Method" = "gsvd", 
                                                                  "Trials" = nTrials, "NoiseLevel" = noiseLevel, "MSE" = thisFilteredMSE))
    Scenario1_allGsvdError <-rbind(Scenario1_allGsvdError, data.frame("Partial" = "Distortion", "Trials" = nTrials, 
                                                                      "NoiseLevel" = noiseLevel, "PercentError" = (thisDistortion / (thisDistortion + thisResidual)) * 100))
    Scenario1_allGsvdError<- rbind(Scenario1_allGsvdError, data.frame("Partial" = "ResidualNoise", "Trials" = nTrials,
                                                                      "NoiseLevel" = noiseLevel, "PercentError" = (thisResidual / (thisDistortion + thisResidual)) * 100))
    
    
    #Calculate the values if NO filtering is applied
    thisMeanMSE <- mse(thisSignalPlusNoiseMean,thisSignalMean)
    Scenario1_allMeanMse <-rbind(Scenario1_allMeanMse, data.frame("Method" = "Averaging",
                                                                  "Trials" = nTrials, "NoiseLevel" = noiseLevel, "MSE" = thisMeanMSE))
  }
  
}

#Combine MSE's into one data.frame for graphing purposes
Scenario1_MSE <- data.frame()
Scenario1_MSE <- rbind(Scenario1_allGsvdMse, Scenario1_allMeanMse)

# load grpahing packages
library(ggplot2)
library(scales)

# create plot for MSE measures
ggplot(data = Scenario1_MSE, aes(x = as.factor(Trials), y = MSE, color = Method)) +
  geom_point() +
  facet_wrap(~ as.factor(NoiseLevel), nrow = 1, scales = "free_y") +
  theme_minimal() +
  labs(title = "MSE Comparison Between Methods as Function of Number of Trials 
       Faceted by Noise Level for Scenario 1", x = "Number of Trials", y = "MSE", color = "Method") +
  theme(plot.title = element_text(hjust = 0.5))

#create plot for MSE breakdown
ggplot(data = Scenario1_allGsvdError, aes(x = as.factor(Trials), y = PercentError, fill = Partial)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ as.factor(NoiseLevel), nrow = 1, scales = "free_y") +
  theme_minimal() +
  labs(title = "Breakdown of MSE by Trial Size Faceted by Noise Level for Scenario 1", x = "Number of Trials",
       y = " Percent of MSE", fill = "Part of MSE") +
  theme(plot.title = element_text(hjust = 0.2))

# create SDI plot
ggplot(data = Scenario1_allGsvdSDI, aes(x = as.factor(Trials), y = SDI, color = as.factor(NoiseLevel))) +
  geom_point() +
  theme_minimal() +
  labs(title = "Signal Distortion Index in Scenario 1", x = "Number of Trials", 
       y = "Signal Distortion Index", color = "Noise Level") +
  theme(plot.title = element_text(hjust = 0.5))

# Create Noise Reduction Factor plot
ggplot(data = Scenario1_allGsvdNRfactor, aes(x = as.factor(Trials), y = NoiseReduction, color = as.factor(NoiseLevel))) +
  geom_point() +
  theme_minimal() +
  labs(title = "Noise Reduction in Scenario 1", x = "Number of Trials", 
       y = "Noise Reduction Factor", color = "Noise Level") +
  theme(plot.title = element_text(hjust = 0.5))

# view table of SNR values
view(Scenario1_allGsvdSNR)

#initialize some variables to hold output
Scenario2_allGsvdMse <-data.frame()
Scenario2_allMeanMse <- data.frame()
Scenario2_allGsvdError <- data.frame()
Scenario2_allGsvdSNR <- data.frame()
Scenario2_allGsvdSDI <- data.frame()
Scenario2_allGsvdNRfactor <- data.frame()
Scenario2_allGsvdSNRDiff <- data.frame()

# set variable lists for loops
nElec <- 64 
nTime <- 600
trialList <-c(25, 50, 100, 200, 400)
noiseList <-c(5, 10, 20)

# loop to create results for all conditions
for (nTrials in trialList) {
  for (noiseLevel in noiseList) {
    
    #print statement to keep track of loop progress
    print(c(nTrials, noiseLevel))
    
    #Generate a simulation
    thisSignal <-  simdata(i = 64, j = 600, k = nTrials, p = 0)
    thisPatternNoiseForSignal <- PatternNoise(i = 64, j = 600, k = nTrials, p = noiseLevel)
    thisWhiteNoiseForSignal <-  array(rnorm(nElec * nTime * nTrials, mean = 0, sd = .01), dim = c(nElec, nTime, nTrials))
    
    patternForNoiseOnly <- PatternNoise(i = 64, j = 600, k = nTrials, p = noiseLevel)
    whiteForNoiseOnly <-  array(rnorm(nElec * nTime * nTrials, mean = 0, sd = .01), dim = c(nElec, nTime, nTrials))
    
    thisNoise <- thisPatternNoiseForSignal+thisWhiteNoiseForSignal
    thisSignalPlusNoise <- thisSignal + thisNoise
    noiseOnlyData <- patternForNoiseOnly+whiteForNoiseOnly
    
    #Apply filter to data
    filter <- Extract_GSVD_filter(thisSignalPlusNoise, noiseOnlyData)
    filteredSignalPlusNoise <- applyFilterTo3dData(filter, thisSignalPlusNoise)
    filteredSignal <- applyFilterTo3dData(filter, thisSignal)
    filteredNoise <-  applyFilterTo3dData(filter, thisNoise)
    
    #Take mean over trials.  
    thisSignalMean <- rowMeans(thisSignal, dims = 2)
    thisSignalPlusNoiseMean <- rowMeans(thisSignalPlusNoise, dims = 2)
    thisNoiseMean <- rowMeans(thisNoise, dims = 2)
    filteredSignalPlusNoise<- rowMeans(filteredSignalPlusNoise, dims = 2) 
    filteredSignal <- rowMeans(filteredSignal, dims = 2) 
    filteredSignalNoise <- rowMeans(filteredNoise, dims = 2) 
    
    #Calculate measures for the GSVD filtered data
    thisFilteredMSE <- mse(filteredSignalPlusNoise, thisSignalMean)
    thisDistortion <- mse(filteredSignal, thisSignalMean)
    thisResidual <- mean(filteredSignalNoise^2)
    thisOriginal <- mean(thisNoiseMean^2)
    thisInSNR <- mean((thisSignal^2) / (thisNoise^2))
    thisOutSNR <- mean((filteredSignal^2) / thisResidual)
    thisSDI <- thisDistortion / mean(thisSignal^2)
    thisNRfactor <- thisOriginal / thisResidual
    
    #Store measures in data frames for later graphing
    Scenario2_allGsvdSNR <- rbind(Scenario2_allGsvdSNR, data.frame("Condition" =
                                                                     "Input", "Trials" = nTrials, "NoiseLevel" = noiseLevel,
                                                                   "SNR" = thisInSNR))
    Scenario2_allGsvdSNR <- rbind(Scenario2_allGsvdSNR, data.frame("Condition" =
                                                                     "Output", "Trials" = nTrials, "NoiseLevel" = noiseLevel,
                                                                   "SNR" = thisOutSNR))
    Scenario2_allGsvdSNRDiff <- rbind(Scenario2_allGsvdSNRDiff, data.frame("Trials" = nTrials,
                                                                           "NoiseLevel" = noiseLevel, "Difference" = thisInSNR - thisOutSNR))
    Scenario2_allGsvdSDI <- rbind(Scenario2_allGsvdSDI, data.frame("Trials" = nTrials, 
                                                                   "NoiseLevel" = noiseLevel, "SDI"= thisSDI))
    Scenario2_allGsvdNRfactor <- rbind(Scenario2_allGsvdNRfactor, data.frame("Trials" = nTrials, 
                                                                             "NoiseLevel" = noiseLevel, "NoiseReduction" = thisNRfactor))
    Scenario2_allGsvdMse <-rbind(Scenario2_allGsvdMse, data.frame("Method" = "gsvd", 
                                                                  "Trials" = nTrials, "NoiseLevel" = noiseLevel, "MSE" = thisFilteredMSE))
    Scenario2_allGsvdError <-rbind(Scenario2_allGsvdError, data.frame("Partial" = "Distortion", "Trials" = nTrials, 
                                                                      "NoiseLevel" = noiseLevel, "PercentError" = (thisDistortion / (thisDistortion + thisResidual)) * 100))
    Scenario2_allGsvdError<- rbind(Scenario2_allGsvdError, data.frame("Partial" = "ResidualNoise", "Trials" = nTrials,
                                                                      "NoiseLevel" = noiseLevel, "PercentError" = (thisResidual / (thisDistortion + thisResidual)) * 100))
    
    
    #Calculate the values if NO filtering is applied
    thisMeanMSE <- mse(thisSignalPlusNoiseMean,thisSignalMean)
    Scenario2_allMeanMse <-rbind(Scenario2_allMeanMse, data.frame("Method" = "Averaging",
                                                                  "Trials" = nTrials, "NoiseLevel" = noiseLevel, "MSE" = thisMeanMSE))
  }
  
}

# Combine MSE's into one table for graphing purposes
Scenario2_MSE <- data.frame()
Scenario2_MSE <- rbind(Scenario2_allGsvdMse, Scenario2_allMeanMse)


#create MSE plot
ggplot(data = Scenario2_MSE, aes(x = as.factor(Trials), y = MSE, color = Method)) +
  geom_point() +
  facet_wrap(~ as.factor(NoiseLevel), nrow = 1, scales = "free_y") +
  theme_minimal() +
  labs(title = "MSE Comparison Between Methods as Function of Number of Trials 
       Faceted by Noise Level for Scenario 2", x = "Number of Trials", y = "MSE", color = "Method") +
  theme(plot.title = element_text(hjust = 0.3))

#Create MSE breakdown plot
ggplot(data = Scenario2_allGsvdError, aes(x = as.factor(Trials), y = PercentError, fill = Partial)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ as.factor(NoiseLevel), nrow = 1, scales = "free_y") +
  theme_minimal() +
  labs(title = "Breakdown of MSE by Trial Size Faceted by Noise Level for Scenario 2", x = "Number of Trials",
       y = " Percent of MSE", fill = "Part of MSE") +
  theme(plot.title = element_text(hjust = 0.2))

# Create SDI plot
ggplot(data = Scenario2_allGsvdSDI, aes(x = as.factor(Trials), y = SDI, color = as.factor(NoiseLevel))) +
  geom_point() +
  theme_minimal() +
  labs(title = "Signal Distortion Index in Scenario 2", x = "Number of Trials", 
       y = "Signal Distortion Index", color = "Noise Level") +
  theme(plot.title = element_text(hjust = 0.5))

#Create Noise Reduction Factor plot
ggplot(data = Scenario2_allGsvdNRfactor, aes(x = as.factor(Trials), y = NoiseReduction, color = as.factor(NoiseLevel))) +
  geom_point() +
  theme_minimal() +
  scale_y_continuous(limits = c(0, 25000)) +
  labs(title = "Noise Reduction in Scenario 2", x = "Number of Trials", 
       y = "Noise Reduction Factor", color = "Noise Level") +
  theme(plot.title = element_text(hjust = 0.5))

#view table of SNR values
view(Scenario2_allGsvdSNR)

#initialize some variables to hold output
Scenario3_allGsvdMse <-data.frame()
Scenario3_allMeanMse <- data.frame()
Scenario3_allGsvdError <- data.frame()
Scenario3_allGsvdSNR <- data.frame()
Scenario3_allGsvdSDI <- data.frame()
Scenario3_allGsvdNRfactor <- data.frame()
Scenario3_allGsvdSNRDiff <- data.frame()

#set variables for simulations
nElec <- 64 
nTime <- 600
trialList <-c(25, 50, 100, 200, 400)
noiseList <-c(5, 10, 20)

# loop to generate all results
for (nTrials in trialList) {
  for (noiseLevel in noiseList) {
    # print to track progress of loop
    print(c(nTrials, noiseLevel))
    
    #Generate a simulation
    thisSignal <-  simdata(i = 64, j = 600, k = nTrials, p = 0)
    thisPatternNoiseForSignal <- MultiPatternNoise(i = 64, j = 600, k = nTrials, p = noiseLevel)
    thisWhiteNoiseForSignal <-  array(rnorm(nElec * nTime * nTrials, mean = 0, sd = .01), dim = c(nElec, nTime, nTrials))
    
    patternForNoiseOnly <- MultiPatternNoise(i = 64, j = 600, k = nTrials, p = noiseLevel)
    whiteForNoiseOnly <-  array(rnorm(nElec * nTime * nTrials, mean = 0, sd = .01), dim = c(nElec, nTime, nTrials))
    
    thisNoise <- thisPatternNoiseForSignal+thisWhiteNoiseForSignal
    thisSignalPlusNoise <- thisSignal + thisNoise
    noiseOnlyData <- patternForNoiseOnly+whiteForNoiseOnly
    
    #Apply filter to data
    filter <- Extract_GSVD_filter(thisSignalPlusNoise, noiseOnlyData)
    filteredSignalPlusNoise <- applyFilterTo3dData(filter, thisSignalPlusNoise)
    filteredSignal <- applyFilterTo3dData(filter, thisSignal)
    filteredNoise <-  applyFilterTo3dData(filter, thisNoise)
    
    #Take mean over trials.  
    thisSignalMean <- rowMeans(thisSignal, dims = 2)
    thisSignalPlusNoiseMean <- rowMeans(thisSignalPlusNoise, dims = 2)
    thisNoiseMean <- rowMeans(thisNoise, dims = 2)
    filteredSignalPlusNoise<- rowMeans(filteredSignalPlusNoise, dims = 2) 
    filteredSignal <- rowMeans(filteredSignal, dims = 2) 
    filteredSignalNoise <- rowMeans(filteredNoise, dims = 2) 
    
    #Calculate measures for the GSVD filtered data
    thisFilteredMSE <- mse(filteredSignalPlusNoise, thisSignalMean)
    thisDistortion <- mse(filteredSignal, thisSignalMean)
    thisResidual <- mean(filteredSignalNoise^2)
    thisOriginal <- mean(thisNoiseMean^2)
    thisInSNR <- mean((thisSignal^2) / (thisNoise^2))
    thisOutSNR <- mean((filteredSignal^2) / thisResidual)
    thisSDI <- thisDistortion / mean(thisSignal^2)
    thisNRfactor <- thisOriginal / thisResidual
    
    #store measures in data frames for graphing 
    Scenario3_allGsvdSNR <- rbind(Scenario3_allGsvdSNR, data.frame("Condition" =
                                                                     "Input", "Trials" = nTrials, "NoiseLevel" = noiseLevel,
                                                                   "SNR" = thisInSNR))
    Scenario3_allGsvdSNR <- rbind(Scenario3_allGsvdSNR, data.frame("Condition" =
                                                                     "Output", "Trials" = nTrials, "NoiseLevel" = noiseLevel,
                                                                   "SNR" = thisOutSNR))
    Scenario3_allGsvdSNRDiff <- rbind(Scenario3_allGsvdSNRDiff, data.frame("Trials" = nTrials,
                                                                           "NoiseLevel" = noiseLevel, "Difference" = thisInSNR - thisOutSNR))
    Scenario3_allGsvdSDI <- rbind(Scenario3_allGsvdSDI, data.frame("Trials" = nTrials, 
                                                                   "NoiseLevel" = noiseLevel, "SDI"= thisSDI))
    Scenario3_allGsvdNRfactor <- rbind(Scenario3_allGsvdNRfactor, data.frame("Trials" = nTrials, 
                                                                             "NoiseLevel" = noiseLevel, "NoiseReduction" = thisNRfactor))
    Scenario3_allGsvdMse <-rbind(Scenario3_allGsvdMse, data.frame("Method" = "gsvd", 
                                                                  "Trials" = nTrials, "NoiseLevel" = noiseLevel, "MSE" = thisFilteredMSE))
    Scenario3_allGsvdError <-rbind(Scenario3_allGsvdError, data.frame("Partial" = "Distortion", "Trials" = nTrials, 
                                                                      "NoiseLevel" = noiseLevel, "PercentError" = (thisDistortion / (thisDistortion + thisResidual)) * 100))
    Scenario3_allGsvdError<- rbind(Scenario3_allGsvdError, data.frame("Partial" = "ResidualNoise", "Trials" = nTrials,
                                                                      "NoiseLevel" = noiseLevel, "PercentError" = (thisResidual / (thisDistortion + thisResidual)) * 100))
    
    
    #Calculate the values if NO filtering is applied
    thisMeanMSE <- mse(thisSignalPlusNoiseMean,thisSignalMean)
    Scenario3_allMeanMse <-rbind(Scenario3_allMeanMse, data.frame("Method" = "Averaging",
                                                                  "Trials" = nTrials, "NoiseLevel" = noiseLevel, "MSE" = thisMeanMSE))
  }
  
}

# Combine MSE into one data frame for graphing
Scenario3_MSE <- data.frame()
Scenario3_MSE <- rbind(Scenario3_allGsvdMse, Scenario3_allMeanMse)


#create MSE plot
ggplot(data = Scenario3_MSE, aes(x = as.factor(Trials), y = MSE, color = Method)) +
  geom_point() +
  facet_wrap(~ as.factor(NoiseLevel), nrow = 1, scales = "free_y") +
  theme_minimal() +
  labs(title = "MSE Comparison Between Methods as Function of Number of Trials 
       Faceted by Noise Level for Scenario 3", x = "Number of Trials", y = "MSE", color = "Method") +
  theme(plot.title = element_text(hjust = 0.3))

# create MSE breakdown plot
ggplot(data = Scenario3_allGsvdError, aes(x = as.factor(Trials), y = PercentError, fill = Partial)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ as.factor(NoiseLevel), nrow = 1, scales = "free_y") +
  theme_minimal() +
  labs(title = "Breakdown of MSE by Trial Size Faceted by Noise Level for Scenario 3", x = "Number of Trials",
       y = " Percent of MSE", fill = "Part of MSE") +
  theme(plot.title = element_text(hjust = 0.2))

# create SDI plot
ggplot(data = Scenario3_allGsvdSDI, aes(x = as.factor(Trials), y = SDI, color = as.factor(NoiseLevel))) +
  geom_point() +
  theme_minimal() +
  labs(title = "Signal Distortion Index in Scenario 3", x = "Number of Trials", 
       y = "Signal Distortion Index", color = "Noise Level") +
  theme(plot.title = element_text(hjust = 0.5))

#create noise reduction factor plot
ggplot(data = Scenario3_allGsvdNRfactor, aes(x = as.factor(Trials), y = NoiseReduction, color = as.factor(NoiseLevel))) +
  geom_point() +
  theme_minimal() +
  labs(title = "Noise Reduction in Scenario 3", x = "Number of Trials", 
       y = "Noise Reduction Factor", color = "Noise Level") +
  theme(plot.title = element_text(hjust = 0.5))

#view table of SNR values
view(Scenario3_allGsvdSNR)

#initialize some variables to hold output
Scenario4_allGsvdMse <-data.frame()
Scenario4_allMeanMse <- data.frame()
Scenario4_allGsvdError <- data.frame()
Scenario4_allGsvdSNR <- data.frame()
Scenario4_allGsvdSDI <- data.frame()
Scenario4_allGsvdNRfactor <- data.frame()
Scenario4_allGsvdSNRDiff <- data.frame()

#set variables for simulations
nElec <- 64 
nTime <- 600
trialList <-c(25, 50, 100, 200, 400)
noiseList <-c(5, 10, 20)

#loop to generate all results
for (nTrials in trialList) {
  for (noiseLevel in noiseList) {
    #print to track loop progress
    print(c(nTrials, noiseLevel))
    
    #Generate a simulation
    thisSignal <-  simdata(i = 64, j = 600, k = nTrials, p = 0)
    thisPatternNoiseForSignal <- SpatialTemporalNoise(i = 64, j = 600, k = nTrials, p = noiseLevel)
    thisWhiteNoiseForSignal <-  array(rnorm(nElec * nTime * nTrials, mean = 0, sd = .01), dim = c(nElec, nTime, nTrials))
    
    patternForNoiseOnly <- SpatialTemporalNoise(i = 64, j = 600, k = nTrials, p = noiseLevel)
    whiteForNoiseOnly <-  array(rnorm(nElec * nTime * nTrials, mean = 0, sd = .01), dim = c(nElec, nTime, nTrials))
    
    thisNoise <- thisPatternNoiseForSignal+thisWhiteNoiseForSignal
    thisSignalPlusNoise <- thisSignal + thisNoise
    noiseOnlyData <- patternForNoiseOnly+whiteForNoiseOnly
    
    #Apply filter to data
    filter <- Extract_GSVD_filter(thisSignalPlusNoise, noiseOnlyData)
    filteredSignalPlusNoise <- applyFilterTo3dData(filter, thisSignalPlusNoise)
    filteredSignal <- applyFilterTo3dData(filter, thisSignal)
    filteredNoise <-  applyFilterTo3dData(filter, thisNoise)
    
    #Take mean over trials.  
    thisSignalMean <- rowMeans(thisSignal, dims = 2)
    thisSignalPlusNoiseMean <- rowMeans(thisSignalPlusNoise, dims = 2)
    thisNoiseMean <- rowMeans(thisNoise, dims = 2)
    filteredSignalPlusNoise<- rowMeans(filteredSignalPlusNoise, dims = 2) 
    filteredSignal <- rowMeans(filteredSignal, dims = 2) 
    filteredSignalNoise <- rowMeans(filteredNoise, dims = 2) 
    
    #Calculate measures for the GSVD filtered data
    thisFilteredMSE <- mse(filteredSignalPlusNoise, thisSignalMean)
    thisDistortion <- mse(filteredSignal, thisSignalMean)
    thisResidual <- mean(filteredSignalNoise^2)
    thisOriginal <- mean(thisNoiseMean^2)
    thisInSNR <- mean((thisSignal^2) / (thisNoise^2))
    thisOutSNR <- mean((filteredSignal^2) / thisResidual)
    thisSDI <- thisDistortion / mean(thisSignal^2)
    thisNRfactor <- thisOriginal / thisResidual
    
    #Store measures in data frames for chemistry 
    Scenario4_allGsvdSNR <- rbind(Scenario4_allGsvdSNR, data.frame("Condition" =
                                                                     "Input", "Trials" = nTrials, "NoiseLevel" = noiseLevel,
                                                                   "SNR" = thisInSNR))
    Scenario4_allGsvdSNR <- rbind(Scenario4_allGsvdSNR, data.frame("Condition" =
                                                                     "Output", "Trials" = nTrials, "NoiseLevel" = noiseLevel,
                                                                   "SNR" = thisOutSNR))
    Scenario4_allGsvdSNRDiff <- rbind(Scenario4_allGsvdSNRDiff, data.frame("Trials" = nTrials,
                                                                           "NoiseLevel" = noiseLevel, "Difference" = thisInSNR - thisOutSNR))
    Scenario4_allGsvdSDI <- rbind(Scenario4_allGsvdSDI, data.frame("Trials" = nTrials, 
                                                                   "NoiseLevel" = noiseLevel, "SDI"= thisSDI))
    Scenario4_allGsvdNRfactor <- rbind(Scenario4_allGsvdNRfactor, data.frame("Trials" = nTrials, 
                                                                             "NoiseLevel" = noiseLevel, "NoiseReduction" = thisNRfactor))
    Scenario4_allGsvdMse <-rbind(Scenario4_allGsvdMse, data.frame("Method" = "gsvd", 
                                                                  "Trials" = nTrials, "NoiseLevel" = noiseLevel, "MSE" = thisFilteredMSE))
    Scenario4_allGsvdError <-rbind(Scenario4_allGsvdError, data.frame("Partial" = "Distortion", "Trials" = nTrials, 
                                                                      "NoiseLevel" = noiseLevel, "PercentError" = (thisDistortion / (thisDistortion + thisResidual)) * 100))
    Scenario4_allGsvdError<- rbind(Scenario4_allGsvdError, data.frame("Partial" = "ResidualNoise", "Trials" = nTrials,
                                                                      "NoiseLevel" = noiseLevel, "PercentError" = (thisResidual / (thisDistortion + thisResidual)) * 100))
    
    
    #Calculate the values if NO filtering is applied
    thisMeanMSE <- mse(thisSignalPlusNoiseMean,thisSignalMean)
    Scenario4_allMeanMse <-rbind(Scenario4_allMeanMse, data.frame("Method" = "Averaging",
                                                                  "Trials" = nTrials, "NoiseLevel" = noiseLevel, "MSE" = thisMeanMSE))
  }
  
}

#Combine MSE for graphing purposes
Scenario4_MSE <- data.frame()
Scenario4_MSE <- rbind(Scenario4_allGsvdMse, Scenario4_allMeanMse)

#create MSE plot
ggplot(data = Scenario4_MSE, aes(x = as.factor(Trials), y = MSE, color = Method)) +
  geom_point() +
  facet_wrap(~ as.factor(NoiseLevel), nrow = 1, scales = "free_y") +
  theme_minimal() +
  labs(title = "MSE Comparison Between Methods as Function of Number of Trials 
       Faceted by Noise Level for Scenario 4", x = "Number of Trials", y = "MSE", color = "Method") +
  theme(plot.title = element_text(hjust = 0.4))

# create MSE breakdown plot
ggplot(data = Scenario4_allGsvdError, aes(x = as.factor(Trials), y = PercentError, fill = Partial)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ as.factor(NoiseLevel), nrow = 1, scales = "free_y") +
  theme_minimal() +
  labs(title = "Breakdown of MSE by Trial Size Faceted by Noise Level for Scenario 4", x = "Number of Trials",
       y = " Percent of MSE", fill = "Part of MSE") +
  theme(plot.title = element_text(hjust = 0.2))

# create SDI plot
ggplot(data = Scenario4_allGsvdSDI, aes(x = as.factor(Trials), y = SDI, color = as.factor(NoiseLevel))) +
  geom_point() +
  theme_minimal() +
  labs(title = "Signal Distortion Index in Scenario 4", x = "Number of Trials", 
       y = "Signal Distortion Index", color = "Noise Level") +
  theme(plot.title = element_text(hjust = 0.5))

# Create Noise Reduction plot
ggplot(data = Scenario4_allGsvdNRfactor, aes(x = as.factor(Trials), y = NoiseReduction, color = as.factor(NoiseLevel))) +
  geom_point() +
  theme_minimal() +
  labs(title = "Noise Reduction in Scenario 4", x = "Number of Trials", 
       y = "Noise Reduction Factor", color = "Noise Level") +
  theme(plot.title = element_text(hjust = 0.5))

#view table of SNR values
view(Scenario4_allGsvdSNR)

simEEGsignal <- function(nTime, Trials) {
  # The purpose of this function is generate a signal similar in structure to one
  # obtained from an EEG of evoked potentials in the visual areas of the brain.
  # the inputs are the number of time points, nTime, and number of Trials,
  # the resulting array will contain. The output is the simulated signal.
  
  # input checks 
  if (is.numeric(nTime) == FALSE | is.numeric(Trials) == FALSE) {
    stop("Non-numeric arguments")
  }
  
  if (nTime <= 0 | Trials <= 0) {stop("Non-positive arguments")}
  
  if (nTime != round(nTime) | Trials != round(Trials)) {stop("Non-integer arguments")}  
  
  #Load visual evoked potential data  
  pathname <- file.path("eegForwardData", "fwdOnlyVisual.mat")
  dataSignal <- readMat(pathname)
  
  #Load the number of signal sources (brain regions) and number of electrodes recording
  nElec <- 128
  nSources <- dim(dataSignal$fwdOnlyVisual)[2]
  
  #Generate the results array of appropraite size
  simBrainSignal <- array(dim = c(nElec, nTime, Trials))
  
  # loop to ensure each trial had different random noise  
  for (i in 1:Trials) {
    
    #generate random noise of the correct size to make the results matrix nElec x nTime    
    randomActivity <- array(rnorm(nSources * nTime , mean = 0, sd = 1),dim = c(nSources, nTime))
    #multiply the signal by the random noise and add signal to results arrat   
    simBrainSignal[,,i] <- dataSignal$fwdOnlyVisual[1:length(nElec),] %*% randomActivity
    
  }
  
  #return the simulated signal
  return(simBrainSignal)
}

simEEGsignal <- function(nTime, Trials) {
  # The purpose of this function is generate a signal similar in structure to one
  # obtained from an EEG of evoked potentials in the visual areas of the brain.
  # the inputs are the number of time points, nTime, and number of Trials,
  # the resulting array will contain. The output is the simulated signal.
  
  # input checks 
  if (is.numeric(nTime) == FALSE | is.numeric(Trials) == FALSE) {
    stop("Non-numeric arguments")
  }
  
  if (nTime <= 0 | Trials <= 0) {stop("Non-positive arguments")}
  
  if (nTime != round(nTime) | Trials != round(Trials)) {stop("Non-integer arguments")}  
  
  #Load visual evoked potential data  
  pathname <- file.path("eegForwardData", "fwdOnlyVisual.mat")
  dataSignal <- readMat(pathname)
  
  #Load the number of signal sources (brain regions) and number of electrodes recording
  nElec <- 128
  nSources <- dim(dataSignal$fwdOnlyVisual)[2]
  
  #Generate the results array of appropraite size
  simBrainSignal <- array(dim = c(nElec, nTime, Trials))
  
  # loop to ensure each trial had different random noise  
  for (i in 1:Trials) {
    
    #generate random noise of the correct size to make the results matrix nElec x nTime    
    randomActivity <- array(rnorm(nSources * nTime , mean = 0, sd = 1),dim = c(nSources, nTime))
    #multiply the signal by the random noise and add signal to results arrat   
    simBrainSignal[,,i] <- dataSignal$fwdOnlyVisual[1:length(nElec),] %*% randomActivity
    
  }
  
  #return the simulated signal
  return(simBrainSignal)
}


#initialize some variables to hold output
Scenario5_allGsvdMse <-data.frame()
Scenario5_allMeanMse <- data.frame()
Scenario5_allGsvdError <- data.frame()
Scenario5_allGsvdSNR <- data.frame()
Scenario5_allGsvdSDI <- data.frame()
Scenario5_allGsvdNRfactor <- data.frame()
Scenario5_allGsvdSNRDiff <- data.frame()

# set variable lists for loops
nElec <- 128 
nTime <- 600
trialList <-c(25, 50, 100, 200, 400)
noiseList <-c(5, 10, 20)

# loop to create results for all conditions
for (nTrials in trialList) {
  for (noiseLevel in noiseList) {
    
    #print statement to keep track of loop progress
    print(c(nTrials, noiseLevel))
    
    #Generate a simulation
    thisSignal <-  simEEGsignal(nTime = 600, Trials = nTrials)
    thisNoiseForSignal <- simEEGnoise(nTime = 600, Trials = nTrials, p = 0.05)
    
    NoiseOnly <-simEEGnoise(nTime = 600, Trials = nTrials, p = 0.05)
    
    thisNoise <- thisNoiseForSignal * noiseLevel
    thisSignalPlusNoise <- thisSignal + thisNoise
    noiseOnlyData <- NoiseOnly * noiseLevel
    
    #Apply filter to data
    filter <- Extract_GSVD_filter(thisSignalPlusNoise, noiseOnlyData)
    filteredSignalPlusNoise <- applyFilterTo3dData(filter, thisSignalPlusNoise)
    filteredSignal <- applyFilterTo3dData(filter, thisSignal)
    filteredNoise <-  applyFilterTo3dData(filter, thisNoise)
    
    #Take mean over trials.  
    thisSignalMean <- rowMeans(thisSignal, dims = 2)
    thisSignalPlusNoiseMean <- rowMeans(thisSignalPlusNoise, dims = 2)
    thisNoiseMean <- rowMeans(thisNoise, dims = 2)
    filteredSignalPlusNoise<- rowMeans(filteredSignalPlusNoise, dims = 2) 
    filteredSignal <- rowMeans(filteredSignal, dims = 2) 
    filteredSignalNoise <- rowMeans(filteredNoise, dims = 2) 
    
    #Calculate measures for the GSVD filtered data
    thisFilteredMSE <- mse(filteredSignalPlusNoise, thisSignalMean)
    thisDistortion <- mse(filteredSignal, thisSignalMean)
    thisResidual <- mean(filteredSignalNoise^2)
    thisOriginal <- mean(thisNoiseMean^2)
    thisInSNR <- mean((thisSignal^2) / (thisNoise^2))
    thisOutSNR <- mean((filteredSignal^2) / thisResidual)
    thisSDI <- thisDistortion / mean(thisSignal^2)
    thisNRfactor <- thisOriginal / thisResidual
    
    #Store measures in data frames for later graphing
    Scenario5_allGsvdSNR <- rbind(Scenario5_allGsvdSNR, data.frame("Condition" =
                                                                     "Input", "Trials" = nTrials, "NoiseLevel" = noiseLevel,
                                                                   "SNR" = thisInSNR))
    Scenario5_allGsvdSNR <- rbind(Scenario5_allGsvdSNR, data.frame("Condition" =
                                                                     "Output", "Trials" = nTrials, "NoiseLevel" = noiseLevel,
                                                                   "SNR" = thisOutSNR))
    Scenario5_allGsvdSNRDiff <- rbind(Scenario5_allGsvdSNRDiff, data.frame("Trials" = nTrials,
                                                                           "NoiseLevel" = noiseLevel, "Difference" = thisInSNR - thisOutSNR))
    Scenario5_allGsvdSDI <- rbind(Scenario5_allGsvdSDI, data.frame("Trials" = nTrials, 
                                                                   "NoiseLevel" = noiseLevel, "SDI"= thisSDI))
    Scenario5_allGsvdNRfactor <- rbind(Scenario5_allGsvdNRfactor, data.frame("Trials" = nTrials, 
                                                                             "NoiseLevel" = noiseLevel, "NoiseReduction" = thisNRfactor))
    Scenario5_allGsvdMse <-rbind(Scenario5_allGsvdMse, data.frame("Method" = "gsvd", 
                                                                  "Trials" = nTrials, "NoiseLevel" = noiseLevel, "MSE" = thisFilteredMSE))
    Scenario5_allGsvdError <-rbind(Scenario5_allGsvdError, data.frame("Partial" = "Distortion", "Trials" = nTrials, 
                                                                      "NoiseLevel" = noiseLevel, "PercentError" = (thisDistortion / (thisDistortion + thisResidual)) * 100))
    Scenario5_allGsvdError<- rbind(Scenario5_allGsvdError, data.frame("Partial" = "ResidualNoise", "Trials" = nTrials,
                                                                      "NoiseLevel" = noiseLevel, "PercentError" = (thisResidual / (thisDistortion + thisResidual)) * 100))
    
    
    #Calculate the values if NO filtering is applied
    thisMeanMSE <- mse(thisSignalPlusNoiseMean,thisSignalMean)
    Scenario5_allMeanMse <-rbind(Scenario5_allMeanMse, data.frame("Method" = "Averaging",
                                                                  "Trials" = nTrials, "NoiseLevel" = noiseLevel, "MSE" = thisMeanMSE))
  }
  
}

# Combine MSE's into one table for graphing purposes
Scenario5_MSE <- data.frame()
Scenario5_MSE <- rbind(Scenario5_allGsvdMse, Scenario5_allMeanMse)

#create MSE plot
ggplot(data = Scenario5_MSE, aes(x = as.factor(Trials), y = MSE, color = Method)) +
  geom_point() +
  facet_wrap(~ as.factor(NoiseLevel), nrow = 1, scales = "free_y") +
  theme_minimal() +
  labs(title = "MSE Comparison Between Methods as Function of Number of Trials 
       Faceted by Noise Level for Scenario 5", x = "Number of Trials", y = "MSE", color = "Method") +
  theme(plot.title = element_text(hjust = 0.4))

# create MSE breakdown plot
ggplot(data = Scenario5_allGsvdError, aes(x = as.factor(Trials), y = PercentError, fill = Partial)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ as.factor(NoiseLevel), nrow = 1, scales = "free_y") +
  theme_minimal() +
  labs(title = "Breakdown of MSE by Trial Size Faceted by Noise Level for Scenario 5", x = "Number of Trials",
       y = " Percent of MSE", fill = "Part of MSE") +
  theme(plot.title = element_text(hjust = 0.2))

# create SDI plot
ggplot(data = Scenario5_allGsvdSDI, aes(x = as.factor(Trials), y = SDI, color = as.factor(NoiseLevel))) +
  geom_point() +
  theme_minimal() +
  labs(title = "Signal Distortion Index in Scenario 5", x = "Number of Trials", 
       y = "Signal Distortion Index", color = "Noise Level") +
  theme(plot.title = element_text(hjust = 0.5))

# Create Noise Reduction plot
ggplot(data = Scenario5_allGsvdNRfactor, aes(x = as.factor(Trials), y = NoiseReduction, color = as.factor(NoiseLevel))) +
  geom_point() +
  theme_minimal() +
  labs(title = "Noise Reduction in Scenario 5", x = "Number of Trials", 
       y = "Noise Reduction Factor", color = "Noise Level") +
  theme(plot.title = element_text(hjust = 0.5))

view(Scenario5_allGsvdSNR)

# calculate descriptive statistics for result measures
descriptives <- function(v) {
  # the purpose of this function is to calculate basic descriptive statistics
  # The input is a vector of values for which descriptives values are to be
  #calculated and the output is a vector of length 4 of the descriptives
  
  #set up results vector
  results <- vector()
  
  #add descriptives
  results <- append(results, mean(v))
  results <- append(results, sd(v))
  results <- append(results, min(v))
  results <- append(results, max(v))
  
  #return descriptives
  return(results)
}

#create table of descriptive statistics for Scenario 1
Scenario1_descriptives <- matrix(nrow = 4, ncol = 9)
colnames(Scenario1_descriptives) <- c("GSVDMSE", "AveragingMSE", "Distortion", "ResidualNoise", "SDI", "NoiseReduction", "InputSNR", "OutputSNR", "SNRDifference")
rownames(Scenario1_descriptives) <- c("Mean", "SD", "Min", "Max")
Scenario1_descriptives[, "GSVDMSE"] <- descriptives(Scenario1_MSE$MSE[which(Scenario1_MSE$Method == "gsvd")])
Scenario1_descriptives[, "AveragingMSE"] <- descriptives(Scenario1_MSE$MSE[which(Scenario1_MSE$Method == "Averaging")])
Scenario1_descriptives[, "Distortion"] <- descriptives(Scenario1_allGsvdError$PercentError[which(Scenario1_allGsvdError$Partial == "Distortion")])
Scenario1_descriptives[, "ResidualNoise"] <- descriptives(Scenario1_allGsvdError$PercentError[which(Scenario1_allGsvdError$Partial == "ResidualNoise")])
Scenario1_descriptives[, "SDI"] <- descriptives(Scenario1_allGsvdSDI$SDI)
Scenario1_descriptives[, "NoiseReduction"] <- descriptives(Scenario1_allGsvdNRfactor$NoiseReduction)
Scenario1_descriptives[, "InputSNR"] <- descriptives(Scenario1_allGsvdSNR$SNR[which(Scenario1_allGsvdSNR$Condition == "Input")])
Scenario1_descriptives[, "OutputSNR"] <- descriptives(Scenario1_allGsvdSNR$SNR[which(Scenario1_allGsvdSNR$Condition == "Output")])
Scenario1_descriptives[, "SNRDifference"] <- descriptives(Scenario1_allGsvdSNRDiff$Difference)
Scenario1_descriptives <- t(Scenario1_descriptives)
view(Scenario1_descriptives)

#Create table of descriptive statistics for Scenario 2
Scenario2_descriptives <- matrix(nrow = 4, ncol = 9)
colnames(Scenario2_descriptives) <- c("GSVDMSE", "AveragingMSE", "Distortion", "ResidualNoise", "SDI", "NoiseReduction", "InputSNR", "OutputSNR", "SNRDifference")
rownames(Scenario2_descriptives) <- c("Mean", "SD", "Min", "Max")
Scenario2_descriptives[, "GSVDMSE"] <- descriptives(Scenario2_MSE$MSE[which(Scenario2_MSE$Method == "gsvd")])
Scenario2_descriptives[, "AveragingMSE"] <- descriptives(Scenario2_MSE$MSE[which(Scenario2_MSE$Method == "Averaging")])
Scenario2_descriptives[, "Distortion"] <- descriptives(Scenario2_allGsvdError$PercentError[which(Scenario2_allGsvdError$Partial == "Distortion")])
Scenario2_descriptives[, "ResidualNoise"] <- descriptives(Scenario2_allGsvdError$PercentError[which(Scenario2_allGsvdError$Partial == "ResidualNoise")])
Scenario2_descriptives[, "SDI"] <- descriptives(Scenario2_allGsvdSDI$SDI)
Scenario2_descriptives[, "NoiseReduction"] <- descriptives(Scenario2_allGsvdNRfactor$NoiseReduction)
Scenario2_descriptives[, "InputSNR"] <- descriptives(Scenario2_allGsvdSNR$SNR[which(Scenario2_allGsvdSNR$Condition == "Input")])
Scenario2_descriptives[, "OutputSNR"] <- descriptives(Scenario2_allGsvdSNR$SNR[which(Scenario2_allGsvdSNR$Condition == "Output")])
Scenario2_descriptives[, "SNRDifference"] <- descriptives(Scenario2_allGsvdSNRDiff$Difference)
Scenario2_descriptives <- t(Scenario2_descriptives)
view(Scenario2_descriptives)

#create table of descriptives for Scenario 3
Scenario3_descriptives <- matrix(nrow = 4, ncol = 9)
colnames(Scenario3_descriptives) <- c("GSVDMSE", "AveragingMSE", "Distortion", "ResidualNoise", "SDI", "NoiseReduction", "InputSNR", "OutputSNR", "SNRDifference")
rownames(Scenario3_descriptives) <- c("Mean", "SD", "Min", "Max")
Scenario3_descriptives[, "GSVDMSE"] <- descriptives(Scenario3_MSE$MSE[which(Scenario3_MSE$Method == "gsvd")])
Scenario3_descriptives[, "AveragingMSE"] <- descriptives(Scenario3_MSE$MSE[which(Scenario3_MSE$Method == "Averaging")])
Scenario3_descriptives[, "Distortion"] <- descriptives(Scenario3_allGsvdError$PercentError[which(Scenario3_allGsvdError$Partial == "Distortion")])
Scenario3_descriptives[, "ResidualNoise"] <- descriptives(Scenario3_allGsvdError$PercentError[which(Scenario3_allGsvdError$Partial == "ResidualNoise")])
Scenario3_descriptives[, "SDI"] <- descriptives(Scenario3_allGsvdSDI$SDI)
Scenario3_descriptives[, "NoiseReduction"] <- descriptives(Scenario3_allGsvdNRfactor$NoiseReduction)
Scenario3_descriptives[, "InputSNR"] <- descriptives(Scenario3_allGsvdSNR$SNR[which(Scenario3_allGsvdSNR$Condition == "Input")])
Scenario3_descriptives[, "OutputSNR"] <- descriptives(Scenario3_allGsvdSNR$SNR[which(Scenario3_allGsvdSNR$Condition == "Output")])
Scenario3_descriptives[, "SNRDifference"] <- descriptives(Scenario3_allGsvdSNRDiff$Difference)
Scenario3_descriptives <- t(Scenario3_descriptives)
view(Scenario3_descriptives)

#create table of descriptives for Scenario 4
Scenario4_descriptives <- matrix(nrow = 4, ncol = 9)
colnames(Scenario4_descriptives) <- c("GSVDMSE", "AveragingMSE", "Distortion", "ResidualNoise", "SDI", "NoiseReduction", "InputSNR", "OutputSNR", "SNRDifference")
rownames(Scenario4_descriptives) <- c("Mean", "SD", "Min", "Max")
Scenario4_descriptives[, "GSVDMSE"] <- descriptives(Scenario4_MSE$MSE[which(Scenario4_MSE$Method == "gsvd")])
Scenario4_descriptives[, "AveragingMSE"] <- descriptives(Scenario4_MSE$MSE[which(Scenario4_MSE$Method == "Averaging")])
Scenario4_descriptives[, "Distortion"] <- descriptives(Scenario4_allGsvdError$PercentError[which(Scenario4_allGsvdError$Partial == "Distortion")])
Scenario4_descriptives[, "ResidualNoise"] <- descriptives(Scenario4_allGsvdError$PercentError[which(Scenario4_allGsvdError$Partial == "ResidualNoise")])
Scenario4_descriptives[, "SDI"] <- descriptives(Scenario4_allGsvdSDI$SDI)
Scenario4_descriptives[, "NoiseReduction"] <- descriptives(Scenario4_allGsvdNRfactor$NoiseReduction)
Scenario4_descriptives[, "InputSNR"] <- descriptives(Scenario4_allGsvdSNR$SNR[which(Scenario4_allGsvdSNR$Condition == "Input")])
Scenario4_descriptives[, "OutputSNR"] <- descriptives(Scenario4_allGsvdSNR$SNR[which(Scenario4_allGsvdSNR$Condition == "Output")])
Scenario4_descriptives[, "SNRDifference"] <- descriptives(Scenario4_allGsvdSNRDiff$Difference)
Scenario4_descriptives <- t(Scenario4_descriptives)
view(Scenario4_descriptives)

#create table of descriptives for Scenario 5
Scenario5_descriptives <- matrix(nrow = 4, ncol = 9)
colnames(Scenario5_descriptives) <- c("GSVDMSE", "AveragingMSE", "Distortion", "ResidualNoise", "SDI", "NoiseReduction", "InputSNR", "OutputSNR", "SNRDifference")
rownames(Scenario5_descriptives) <- c("Mean", "SD", "Min", "Max")
Scenario5_descriptives[, "GSVDMSE"] <- descriptives(Scenario5_MSE$MSE[which(Scenario5_MSE$Method == "gsvd")])
Scenario5_descriptives[, "AveragingMSE"] <- descriptives(Scenario5_MSE$MSE[which(Scenario5_MSE$Method == "Averaging")])
Scenario5_descriptives[, "Distortion"] <- descriptives(Scenario5_allGsvdError$PercentError[which(Scenario5_allGsvdError$Partial == "Distortion")])
Scenario5_descriptives[, "ResidualNoise"] <- descriptives(Scenario5_allGsvdError$PercentError[which(Scenario5_allGsvdError$Partial == "ResidualNoise")])
Scenario5_descriptives[, "SDI"] <- descriptives(Scenario5_allGsvdSDI$SDI)
Scenario5_descriptives[, "NoiseReduction"] <- descriptives(Scenario5_allGsvdNRfactor$NoiseReduction)
Scenario5_descriptives[, "InputSNR"] <- descriptives(Scenario5_allGsvdSNR$SNR[which(Scenario5_allGsvdSNR$Condition == "Input")])
Scenario5_descriptives[, "OutputSNR"] <- descriptives(Scenario5_allGsvdSNR$SNR[which(Scenario5_allGsvdSNR$Condition == "Output")])
Scenario5_descriptives[, "SNRDifference"] <- descriptives(Scenario5_allGsvdSNRDiff$Difference)
Scenario5_descriptives <- t(Scenario5_descriptives)