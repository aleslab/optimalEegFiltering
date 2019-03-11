set.seed(123)

noiseLevel <- 0.5

#initialize some variables to hold output
allGsvdMse <-vector()
allGsvdDistortion <- vector()
allMeanMse <- vector()


for (nTrials in c(25,50,75)) {
 
  #Generate a simulation
  thisSignal <-  simdata(i = 64, j = 600, k = nTrials, p = 0.25)
  thisNoise <- array(rnorm(64 * 600 * nTrials, mean = 0, sd = 1), dim = c(64, 600, nTrials))
  thisNoise <- thisNoise * noiseLevel
  thisSignalPlusNoise <- thisSignal + thisNoise
  
  #Analyze output
  filter <- Extract_GSVD_filter(thisSignalPlusNoise,thisNoise)
  filteredSignalPlusNoise <- applyFilterTo3dData(filter,thisSignalPlusNoise)
  filteredSignal <- applyFilterTo3dData(filter,thisSignal)
  filteredNoise <-  applyFilterTo3dData(filter,thisNoise)
  
  #Take mean over trials.  
  thisSignalMean <- rowMeans(thisSignal,dims=2)
  thisSignalPlusNoiseMean <- rowMeans(thisSignalPlusNoise,dims=2)
  filteredSignalPlusNoise<- rowMeans(filteredSignalPlusNoise,dims=2) 
  filteredSignal<- rowMeans(filteredSignal,dims=2) 
  filteredSignalNoise<- rowMeans(filteredNoise,dims=2) 
  
  #Calculate some values about the GSVD filtered data
  thisMSE <- mse(filteredSignalPlusNoise,thisSignalMean)
  thisDistortion <- mse(filteredSignal,thisSignalMean)
  allGsvdMse <-append(allGsvdMse,thisMSE)
  allGsvdDistortion <-append(allGsvdDistortion,thisDistortion)
  
  #Calculate the values if NO filtering is applied
  thisMSE <- mse(thisSignalPlusNoiseMean,thisSignalMean)
  
  #Distortion doesn't make sense to do. Since no filtering is applied by definition
  #no distortion. 
  #thisDistortion <- mse(thisSignalMean,thisSignalMean)
  allMeanMse <-append(allMeanMse,thisMSE)
  
  
}

########

# scenario1.t1.n1 <- simdata(i = 64, j = 600, k = 25, p = 0.25)
# scenario1.t2.n1 <- simdata(i = 64, j = 600, k = 50, p = 0.25)
# scenario1.t3.n1 <- simdata(i = 64, j = 600, k = 75, p = 0.25)
# scenario1.t4.n1 <- simdata(i = 64, j = 600, k = 100, p = 0.25)
# scenario1.t5.n1 <- simdata(i = 64, j = 600, k = 150, p = 0.25)
# av.scenario1.t1.n1 <- rowMeans(scenario1.t1.n1, dims = 2)
# av.scenario1.t2.n1 <- rowMeans(scenario1.t2.n1, dims = 2)
# av.scenario1.t3.n1 <- rowMeans(scenario1.t3.n1, dims = 2)
# av.scenario1.t4.n1 <- rowMeans(scenario1.t4.n1, dims = 2)
# av.scenario1.t5.n1 <- rowMeans(scenario1.t5.n1, dims = 2)
# MSEav.scenario1.t1.n1 <- mse(av.scenario1.t1.n1, drop(signal(i = 64, j = 600, k = 1)))
# MSEav.scenario1.t2.n1 <- mse(av.scenario1.t2.n1, drop(signal(i = 64, j = 600, k = 1)))
# MSEav.scenario1.t3.n1 <- mse(av.scenario1.t3.n1, drop(signal(i = 64, j = 600, k = 1)))
# MSEav.scenario1.t4.n1 <- mse(av.scenario1.t4.n1, drop(signal(i = 64, j = 600, k = 1)))
# MSEav.scenario1.t5.n1 <- mse(av.scenario1.t5.n1, drop(signal(i = 64, j = 600, k = 1)))
# 
# scenario1.t1.n2 <- simdata(i = 64, j = 600, k = 25, p = 0.5)
# scenario1.t2.n2 <- simdata(i = 64, j = 600, k = 50, p = 0.5)
# scenario1.t3.n2 <- simdata(i = 64, j = 600, k = 75, p = 0.5)
# scenario1.t4.n2 <- simdata(i = 64, j = 600, k = 100, p = 0.5)
# scenario1.t5.n2 <- simdata(i = 64, j = 600, k = 150, p = 0.5)
# av.scenario1.t1.n2 <- rowMeans(scenario1.t1.n2, dims = 2)
# av.scenario1.t2.n2 <- rowMeans(scenario1.t2.n2, dims = 2)
# av.scenario1.t3.n2 <- rowMeans(scenario1.t3.n2, dims = 2)
# av.scenario1.t4.n2 <- rowMeans(scenario1.t4.n2, dims = 2)
# av.scenario1.t5.n2 <- rowMeans(scenario1.t5.n2, dims = 2)
# MSEav.scenario1.t1.n2 <- mse(av.scenario1.t1.n2, drop(signal(i = 64, j = 600, k = 1)))
# MSEav.scenario1.t2.n2 <- mse(av.scenario1.t2.n2, drop(signal(i = 64, j = 600, k = 1)))
# MSEav.scenario1.t3.n2 <- mse(av.scenario1.t3.n2, drop(signal(i = 64, j = 600, k = 1)))
# MSEav.scenario1.t4.n2 <- mse(av.scenario1.t4.n2, drop(signal(i = 64, j = 600, k = 1)))
# MSEav.scenario1.t5.n2 <- mse(av.scenario1.t5.n2, drop(signal(i = 64, j = 600, k = 1)))
# 
# scenario1.t1.n3 <- simdata(i = 64, j = 600, k = 25, p = 0.75)
# scenario1.t2.n3 <- simdata(i = 64, j = 600, k = 50, p = 0.75)
# scenario1.t3.n3 <- simdata(i = 64, j = 600, k = 75, p = 0.75)
# scenario1.t4.n3 <- simdata(i = 64, j = 600, k = 100, p = 0.75)
# scenario1.t5.n3 <- simdata(i = 64, j = 600, k = 150, p = 0.75)
# av.scenario1.t1.n3 <- rowMeans(scenario1.t1.n3, dims = 2)
# av.scenario1.t2.n3 <- rowMeans(scenario1.t2.n3, dims = 2)
# av.scenario1.t3.n3 <- rowMeans(scenario1.t3.n3, dims = 2)
# av.scenario1.t4.n3 <- rowMeans(scenario1.t4.n3, dims = 2)
# av.scenario1.t5.n3 <- rowMeans(scenario1.t5.n3, dims = 2)
# MSEav.scenario1.t1.n3 <- mse(av.scenario1.t1.n3, drop(signal(i = 64, j = 600, k = 1)))
# MSEav.scenario1.t2.n3 <- mse(av.scenario1.t2.n3, drop(signal(i = 64, j = 600, k = 1)))
# MSEav.scenario1.t3.n3 <- mse(av.scenario1.t3.n3, drop(signal(i = 64, j = 600, k = 1)))
# MSEav.scenario1.t4.n3 <- mse(av.scenario1.t4.n3, drop(signal(i = 64, j = 600, k = 1)))
# MSEav.scenario1.t5.n3 <- mse(av.scenario1.t5.n3, drop(signal(i = 64, j = 600, k = 1)))
# 
# scenario1.t1.n4 <- simdata(i = 64, j = 600, k = 25, p = 1)
# scenario1.t2.n4 <- simdata(i = 64, j = 600, k = 50, p = 1)
# scenario1.t3.n4 <- simdata(i = 64, j = 600, k = 75, p = 1)
# scenario1.t4.n4 <- simdata(i = 64, j = 600, k = 100, p = 1)
# scenario1.t5.n4 <- simdata(i = 64, j = 600, k = 150, p = 1)
# av.scenario1.t1.n4 <- rowMeans(scenario1.t1.n4, dims = 2)
# av.scenario1.t2.n4 <- rowMeans(scenario1.t2.n4, dims = 2)
# av.scenario1.t3.n4 <- rowMeans(scenario1.t3.n4, dims = 2)
# av.scenario1.t4.n4 <- rowMeans(scenario1.t4.n4, dims = 2)
# av.scenario1.t5.n4 <- rowMeans(scenario1.t5.n4, dims = 2)
# MSEav.scenario1.t1.n4 <- mse(av.scenario1.t1.n4, drop(signal(i = 64, j = 600, k = 1)))
# MSEav.scenario1.t2.n4 <- mse(av.scenario1.t2.n4, drop(signal(i = 64, j = 600, k = 1)))
# MSEav.scenario1.t3.n4 <- mse(av.scenario1.t3.n4, drop(signal(i = 64, j = 600, k = 1)))
# MSEav.scenario1.t4.n4 <- mse(av.scenario1.t4.n4, drop(signal(i = 64, j = 600, k = 1)))
# MSEav.scenario1.t5.n4 <- mse(av.scenario1.t5.n4, drop(signal(i = 64, j = 600, k = 1)))
# 
# MSE.average.n1 <- c(MSEav.scenario1.t1.n1, MSEav.scenario1.t2.n1, MSEav.scenario1.t2.n1, 
#                     MSEav.scenario1.t4.n1, MSEav.scenario1.t5.n1)
# MSE.average.n2 <- c(MSEav.scenario1.t1.n2, MSEav.scenario1.t2.n2, MSEav.scenario1.t2.n2, 
#                     MSEav.scenario1.t4.n2, MSEav.scenario1.t5.n2)
# MSE.average.n3 <- c(MSEav.scenario1.t1.n3, MSEav.scenario1.t2.n3, MSEav.scenario1.t2.n3, 
#                     MSEav.scenario1.t4.n3, MSEav.scenario1.t5.n3)
# MSE.average.n4 <- c(MSEav.scenario1.t1.n4, MSEav.scenario1.t2.n4, MSEav.scenario1.t2.n4, 
#                     MSEav.scenario1.t4.n4, MSEav.scenario1.t5.n4)
# 
# # Results for filters
# RNoise.t1 <- array(rnorm(64 * 600 * 25, mean = 0, sd = 1), dim = c(64, 600, 25))
# RNoise.t2 <- array(rnorm(64 * 600 * 50, mean = 0, sd = 1), dim = c(64, 600, 50))
# RNoise.t3 <- array(rnorm(64 * 600 * 75, mean = 0, sd = 1), dim = c(64, 600, 75))
# RNoise.t4 <- array(rnorm(64 * 600 * 100, mean = 0, sd = 1), dim = c(64, 600, 100))
# RNoise.t5 <- array(rnorm(64 * 600 * 150, mean = 0, sd = 1), dim = c(64, 600, 150))
# 
# f.scenario1.t1.n1 <- GSVD_filter(scenario1.t1.n1, (RNoise.t1 *0.25))
# f.scenario1.t2.n1 <- GSVD_filter(scenario1.t2.n1, (RNoise.t2 *0.25))
# f.scenario1.t3.n1 <- GSVD_filter(scenario1.t3.n1, (RNoise.t3 *0.25))
# f.scenario1.t4.n1 <- GSVD_filter(scenario1.t4.n1, (RNoise.t4 *0.25))
# f.scenario1.t5.n1 <- GSVD_filter(scenario1.t5.n1, (RNoise.t5 *0.25))
# avf.scenario1.t1.n1 <-rowMeans(f.scenario1.t1.n1, dims=2)
# avf.scenario1.t2.n1 <-rowMeans(f.scenario1.t2.n1, dims=2)
# avf.scenario1.t3.n1 <-rowMeans(f.scenario1.t3.n1, dims=2)
# avf.scenario1.t4.n1 <-rowMeans(f.scenario1.t4.n1, dims=2)
# avf.scenario1.t5.n1 <-rowMeans(f.scenario1.t5.n1, dims=2)
# MSEf.scenario1.t1.n1 <- mse(avf.scenario1.t1.n1, drop(signal(i = 64, j = 600, k = 1)))
# MSEf.scenario1.t2.n1 <- mse(avf.scenario1.t2.n1, drop(signal(i = 64, j = 600, k = 1)))
# MSEf.scenario1.t3.n1 <- mse(avf.scenario1.t3.n1, drop(signal(i = 64, j = 600, k = 1)))
# MSEf.scenario1.t4.n1 <- mse(avf.scenario1.t4.n1, drop(signal(i = 64, j = 600, k = 1)))
# MSEf.scenario1.t5.n1 <- mse(avf.scenario1.t5.n1, drop(signal(i = 64, j = 600, k = 1)))
# 
# f.scenario1.t1.n2 <- GSVD_filter(scenario1.t1.n2, (RNoise.t1 *0.5))
# f.scenario1.t2.n2 <- GSVD_filter(scenario1.t2.n2, (RNoise.t2 *0.5))
# f.scenario1.t3.n2 <- GSVD_filter(scenario1.t3.n2, (RNoise.t3 *0.5))
# f.scenario1.t4.n2 <- GSVD_filter(scenario1.t4.n2, (RNoise.t4 *0.5))
# f.scenario1.t5.n2 <- GSVD_filter(scenario1.t5.n2, (RNoise.t5 *0.5))
# avf.scenario1.t1.n2 <-rowMeans(f.scenario1.t1.n2, dims=2)
# avf.scenario1.t2.n2 <-rowMeans(f.scenario1.t2.n2, dims=2)
# avf.scenario1.t3.n2 <-rowMeans(f.scenario1.t3.n2, dims=2)
# avf.scenario1.t4.n2 <-rowMeans(f.scenario1.t4.n2, dims=2)
# avf.scenario1.t5.n2 <-rowMeans(f.scenario1.t5.n2, dims=2)
# MSEf.scenario1.t1.n2 <- mse(avf.scenario1.t1.n2, drop(signal(i = 64, j = 600, k = 1)))
# MSEf.scenario1.t2.n2 <- mse(avf.scenario1.t2.n2, drop(signal(i = 64, j = 600, k = 1)))
# MSEf.scenario1.t3.n2 <- mse(avf.scenario1.t3.n2, drop(signal(i = 64, j = 600, k = 1)))
# MSEf.scenario1.t4.n2 <- mse(avf.scenario1.t4.n2, drop(signal(i = 64, j = 600, k = 1)))
# MSEf.scenario1.t5.n2 <- mse(avf.scenario1.t5.n2, drop(signal(i = 64, j = 600, k = 1)))
# 
# f.scenario1.t1.n3 <- GSVD_filter(scenario1.t1.n3, (RNoise.t1 *0.75))
# f.scenario1.t2.n3 <- GSVD_filter(scenario1.t2.n3, (RNoise.t2 *0.75))
# f.scenario1.t3.n3 <- GSVD_filter(scenario1.t3.n3, (RNoise.t3 *0.75))
# f.scenario1.t4.n3 <- GSVD_filter(scenario1.t4.n3, (RNoise.t4 *0.75))
# f.scenario1.t5.n3 <- GSVD_filter(scenario1.t5.n3, (RNoise.t5 *0.75))
# avf.scenario1.t1.n3 <-rowMeans(f.scenario1.t1.n3, dims=2)
# avf.scenario1.t2.n3 <-rowMeans(f.scenario1.t2.n3, dims=2)
# avf.scenario1.t3.n3 <-rowMeans(f.scenario1.t3.n3, dims=2)
# avf.scenario1.t4.n3 <-rowMeans(f.scenario1.t4.n3, dims=2)
# avf.scenario1.t5.n3 <-rowMeans(f.scenario1.t5.n3, dims=2)
# MSEf.scenario1.t1.n3 <- mse(avf.scenario1.t1.n3, drop(signal(i = 64, j = 600, k = 1)))
# MSEf.scenario1.t2.n3 <- mse(avf.scenario1.t2.n3, drop(signal(i = 64, j = 600, k = 1)))
# MSEf.scenario1.t3.n3 <- mse(avf.scenario1.t3.n3, drop(signal(i = 64, j = 600, k = 1)))
# MSEf.scenario1.t4.n3 <- mse(avf.scenario1.t4.n3, drop(signal(i = 64, j = 600, k = 1)))
# MSEf.scenario1.t5.n3 <- mse(avf.scenario1.t5.n3, drop(signal(i = 64, j = 600, k = 1)))
# 
# f.scenario1.t1.n4 <- GSVD_filter(scenario1.t1.n4, RNoise.t1)
# f.scenario1.t2.n4 <- GSVD_filter(scenario1.t2.n4, RNoise.t2)
# f.scenario1.t3.n4 <- GSVD_filter(scenario1.t3.n4, RNoise.t3)
# f.scenario1.t4.n4 <- GSVD_filter(scenario1.t4.n4, RNoise.t4)
# f.scenario1.t5.n4 <- GSVD_filter(scenario1.t5.n4, RNoise.t5)
# avf.scenario1.t1.n4 <-rowMeans(f.scenario1.t1.n4, dims=2)
# avf.scenario1.t2.n4 <-rowMeans(f.scenario1.t2.n4, dims=2)
# avf.scenario1.t3.n4 <-rowMeans(f.scenario1.t3.n4, dims=2)
# avf.scenario1.t4.n4 <-rowMeans(f.scenario1.t4.n4, dims=2)
# avf.scenario1.t5.n4 <-rowMeans(f.scenario1.t5.n4, dims=2)
# MSEf.scenario1.t1.n4 <- mse(avf.scenario1.t1.n4, drop(signal(i = 64, j = 600, k = 1)))
# MSEf.scenario1.t2.n4 <- mse(avf.scenario1.t2.n4, drop(signal(i = 64, j = 600, k = 1)))
# MSEf.scenario1.t3.n4 <- mse(avf.scenario1.t3.n4, drop(signal(i = 64, j = 600, k = 1)))
# MSEf.scenario1.t4.n4 <- mse(avf.scenario1.t4.n4, drop(signal(i = 64, j = 600, k = 1)))
# MSEf.scenario1.t5.n4 <- mse(avf.scenario1.t5.n4, drop(signal(i = 64, j = 600, k = 1)))
# 
# MSE.filter.n1 <- c(MSEf.scenario1.t1.n1, MSEf.scenario1.t2.n1, MSEf.scenario1.t2.n1, 
#                     MSEf.scenario1.t4.n1, MSEf.scenario1.t5.n1)
# MSE.filter.n2 <- c(MSEf.scenario1.t1.n2, MSEf.scenario1.t2.n2, MSEf.scenario1.t2.n2, 
#                     MSEf.scenario1.t4.n2, MSEf.scenario1.t5.n2)
# MSE.filter.n3 <- c(MSEf.scenario1.t1.n3, MSEf.scenario1.t2.n3, MSEf.scenario1.t2.n3, 
#                     MSEf.scenario1.t4.n3, MSEf.scenario1.t5.n3)
# MSE.filter.n4 <- c(MSEf.scenario1.t1.n4, MSEf.scenario1.t2.n4, MSEf.scenario1.t2.n4, 
#                     MSEf.scenario1.t4.n4, MSEf.scenario1.t5.n4)
# 
# # Compile tables
# Scenario1.table.n1 <- matrix(nrow = 10, ncol = 3)
# Scenario1.table.n1[,3] <- c(MSE.average.n1, MSE.filter.n1)
# Scenario1.table.n1[,1] <- rep(c("Average", "Filter"), each = 5)
# Scenario1.table.n1[,2] <- rep(x = c(25, 50, 75, 100, 150), times = 2)
# Scenario1.table.n1 <- as.data.frame(Scenario1.table.n1)
# colnames(Scenario1.table.n1) <- c("Method", "Trials", "MSE")
# 
# Scenario1.table.n2 <- matrix(nrow = 10, ncol = 3)
# Scenario1.table.n2[,3] <- c(MSE.average.n2, MSE.filter.n2)
# Scenario1.table.n2[,1] <- rep(c("Average", "Filter"), each = 5)
# Scenario1.table.n2[,2] <- rep(x = c(25, 50, 75, 100, 150), times = 2)
# Scenario1.table.n2 <- as.data.frame(Scenario1.table.n2)
# colnames(Scenario1.table.n2) <- c("Method", "Trials", "MSE")
# 
# Scenario1.table.n3 <- matrix(nrow = 10, ncol = 3)
# Scenario1.table.n3[,3] <- c(MSE.average.n3, MSE.filter.n3)
# Scenario1.table.n3[,1] <- rep(c("Average", "Filter"), each = 5)
# Scenario1.table.n3[,2] <- rep(x = c(25, 50, 75, 100, 150), times = 2)
# Scenario1.table.n3 <- as.data.frame(Scenario1.table.n3)
# colnames(Scenario1.table.n3) <- c("Method", "Trials", "MSE")
# 
# Scenario1.table.n4 <- matrix(nrow = 10, ncol = 3)
# Scenario1.table.n4[,3] <- c(MSE.average.n4, MSE.filter.n4)
# Scenario1.table.n4[,1] <- rep(c("Average", "Filter"), each = 5)
# Scenario1.table.n4[,2] <- rep(x = c(25, 50, 75, 100, 150), times = 2)
# Scenario1.table.n4 <- as.data.frame(Scenario1.table.n4)
# colnames(Scenario1.table.n4) <- c("Method", "Trials", "MSE")
# 
# Scenario1.table <- matrix(nrow = 40, ncol = 4)
# Scenario1.table[,2] <- rep(c("Average", "Filter"), each = 5, times = 4)
# Scenario1.table[,1] <- rep(c(1:4), each = 10)
# Scenario1.table[,3] <- rep(x = c(25, 50, 75, 100, 150), times = 8)
# Scenario1.table[,4] <- c(MSE.average.n1, MSE.filter.n1, MSE.average.n2, MSE.filter.n2,
#                          MSE.average.n3, MSE.filter.n3, MSE.average.n4, MSE.filter.n4)
# Scenario1.table <- as.data.frame(Scenario1.table)
# colnames(Scenario1.table) <- c("Noise", "Method", "Trials", "MSE")