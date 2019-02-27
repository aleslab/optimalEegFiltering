set.seed(123)


#scenario.v1.average <- apply(X = scenario.v1, MARGIN = c(1,2), FUN = mean)
scenario.v1.average <-rowMeans(scenario.v1,dims=2)
MSE.v1.average <- mse(scenario.v1.average, drop(signal(i = 64, j = 600, k = 1)))

RNoise <- array(rnorm(64 * 600*100, mean = 0, sd = 1), dim = c(64, 600, 100))
RNoise <- RNoise *.5

scenario.v1.filter <- GSVD_filter(scenario.v1, RNoise)
scenario.v1.avefilter <-rowMeans(scenario.v1.filter,dims=2)
MSE.v1.filter <- mse(scenario.v1.avefilter, drop(signal(i = 64, j = 600, k = 1)))



#Scenario 2
scenario.v2.average <- apply(X = scenario.v2, MARGIN = c(1,2), FUN = mean)
MSE.v2.average <- mse(scenario.v2.average, drop(signal(i = 64, j = 600, k = 1)))


scenario.v2.filter <- GSVD_filter(scenario.v2, Noise.v2)
MSE.v2.filter <- mse(scenario.v2.filter, drop(signal(i = 64, j = 600, k = 100)))