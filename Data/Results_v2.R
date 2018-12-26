set.seed(123)
signal2 <- signal(i = 64, j = 600, k = 200)
scenario.v2 <- simdata2(i = 64, j = 600, k = 200)
mse2 <- mse(scenario.v2, signal2)