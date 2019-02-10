scenario.v1.average <- apply(X = scenario.v1, MARGIN = c(1,2), FUN = mean)
MSE.v1.average <- mse(scenario.v1.average, signal(i = 64, j = 600, k = 1))

scenario.v2.average <- apply(X = scenario.v2, MARGIN = c(1,2), FUN = mean)
MSE.v2.average <- mse(scenario.v2.average, signal(i = 64, j = 600, k = 1))

scenario.v2.filter <- GSVD_filter(scenario.v2, Noise.v2)
MSE.v2.filter <- mse(scenario.v2.filter, signal(i = 64, j = 600, k = 1))