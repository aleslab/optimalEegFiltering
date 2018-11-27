set.seed(123)
signal1 <- signal(i = 64, j = 600, k = 200)
scenario.v1 <- simdata(i = 64, j = 600, k = 200)
mse1 <- mse(scenario.v1, signal1)
