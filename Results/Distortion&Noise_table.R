basesignal.t1 <- signal(i = 64, j = 600, k = 25)
basesignal.t2 <- signal(i = 64, j = 600, k = 50)
basesignal.t3 <- signal(i = 64, j = 600, k = 75)
basesignal.t4 <- signal(i = 64, j = 600, k = 100)
basesignal.t5 <- signal(i = 64, j = 600, k = 150)

distortion.t1.n1 <- distortion(basesignal.t1, scenario1.t1.n1, (RNoise.t1 * 0.25))
distortion.t2.n1 <- distortion(basesignal.t2, scenario1.t2.n1, (RNoise.t2 * 0.25))
distortion.t3.n1 <- distortion(basesignal.t3, scenario1.t3.n1, (RNoise.t3 * 0.25))
distortion.t4.n1 <- distortion(basesignal.t4, scenario1.t4.n1, (RNoise.t4 * 0.25))
distortion.t5.n1 <- distortion(basesignal.t5, scenario1.t5.n1, (RNoise.t5 * 0.25))
distortion.t1.n2 <- distortion(basesignal.t1, scenario1.t1.n2, (RNoise.t1 * 0.5))
distortion.t2.n2 <- distortion(basesignal.t2, scenario1.t2.n2, (RNoise.t2 * 0.5))
distortion.t3.n2 <- distortion(basesignal.t3, scenario1.t3.n2, (RNoise.t3 * 0.5))
distortion.t4.n2 <- distortion(basesignal.t4, scenario1.t4.n2, (RNoise.t4 * 0.5))
distortion.t5.n2 <- distortion(basesignal.t5, scenario1.t5.n2, (RNoise.t5 * 0.5))
distortion.t1.n3 <- distortion(basesignal.t1, scenario1.t1.n3, (RNoise.t1 * 0.75))
distortion.t2.n3 <- distortion(basesignal.t2, scenario1.t2.n3, (RNoise.t2 * 0.75))
distortion.t3.n3 <- distortion(basesignal.t3, scenario1.t3.n3, (RNoise.t3 * 0.75))
distortion.t4.n3 <- distortion(basesignal.t4, scenario1.t4.n3, (RNoise.t4 * 0.75))
distortion.t5.n3 <- distortion(basesignal.t5, scenario1.t5.n3, (RNoise.t5 * 0.75))
distortion.t1.n4 <- distortion(basesignal.t1, scenario1.t1.n4, (RNoise.t1))
distortion.t2.n4 <- distortion(basesignal.t2, scenario1.t2.n4, (RNoise.t2))
distortion.t3.n4 <- distortion(basesignal.t3, scenario1.t3.n4, (RNoise.t3))
distortion.t4.n4 <- distortion(basesignal.t4, scenario1.t4.n4, (RNoise.t4))
distortion.t5.n4 <- distortion(basesignal.t5, scenario1.t5.n4, (RNoise.t5))

Scenario1.distortion.table <- matrix(nrow = 20, ncol = 3)
Scenario1.distortion.table[,1] <- rep(1:4, each = 5)
Scenario1.distortion.table[,2] <- rep(c(25, 50, 75, 100, 150), times = 4)
Scenario1.distortion.table[,3] <- c(distortion.t1.n1, distortion.t2.n1, distortion.t3.n1,
                                    distortion.t4.n1, distortion.t5.n1, distortion.t1.n2,
                                    distortion.t2.n2, distortion.t3.n2, distortion.t4.n2,
                                    distortion.t5.n2, distortion.t1.n3, distortion.t2.n3, 
                                    distortion.t3.n3, distortion.t4.n3, distortion.t5.n3,
                                    distortion.t1.n4, distortion.t2.n4, distortion.t3.n4,
                                    distortion.t4.n4, distortion.t5.n4)
Scenario1.distortion.table <- as.data.frame(Scenario1.distortion.table)
colnames(Scenario1.distortion.table) <- c("Noise Level", "Trials", "Distortion")


Resid_noise1.t1.n1 <- Residual_noise(scenario1.t1.n1, (RNoise.t1 * 0.25))
Resid_noise1.t2.n1 <- Residual_noise(scenario1.t2.n1, (RNoise.t2 * 0.25))
Resid_noise1.t3.n1 <- Residual_noise(scenario1.t3.n1, (RNoise.t3 * 0.25))
Resid_noise1.t4.n1 <- Residual_noise(scenario1.t4.n1, (RNoise.t4 * 0.25))
Resid_noise1.t5.n1 <- Residual_noise(scenario1.t5.n1, (RNoise.t5 * 0.25))
Resid_noise1.t1.n2 <- Residual_noise(scenario1.t1.n2, (RNoise.t1 * 0.5))
Resid_noise1.t2.n2 <- Residual_noise(scenario1.t2.n2, (RNoise.t2 * 0.5))
Resid_noise1.t3.n2 <- Residual_noise(scenario1.t3.n2, (RNoise.t3 * 0.5))
Resid_noise1.t4.n2 <- Residual_noise(scenario1.t4.n2, (RNoise.t4 * 0.5))
Resid_noise1.t5.n2 <- Residual_noise(scenario1.t5.n2, (RNoise.t5 * 0.5))
Resid_noise1.t1.n3 <- Residual_noise(scenario1.t1.n3, (RNoise.t1 * 0.75))
Resid_noise1.t2.n3 <- Residual_noise(scenario1.t2.n3, (RNoise.t2 * 0.75))
Resid_noise1.t3.n3 <- Residual_noise(scenario1.t3.n3, (RNoise.t3 * 0.75))
Resid_noise1.t4.n3 <- Residual_noise(scenario1.t4.n3, (RNoise.t4 * 0.75))
Resid_noise1.t5.n3 <- Residual_noise(scenario1.t5.n3, (RNoise.t5 * 0.75))
Resid_noise1.t1.n4 <- Residual_noise(scenario1.t1.n4, RNoise.t1)
Resid_noise1.t2.n4 <- Residual_noise(scenario1.t2.n4, RNoise.t2)
Resid_noise1.t3.n4 <- Residual_noise(scenario1.t3.n4, RNoise.t3)
Resid_noise1.t4.n4 <- Residual_noise(scenario1.t4.n4, RNoise.t4)
Resid_noise1.t5.n4 <- Residual_noise(scenario1.t5.n4, RNoise.t5)

Orig_noise.t1.n1 <- Original_noise((RNoise.t1 * 0.25))
Orig_noise.t2.n1 <- Original_noise((RNoise.t2 * 0.25))
Orig_noise.t3.n1 <- Original_noise((RNoise.t3 * 0.25))
Orig_noise.t4.n1 <- Original_noise((RNoise.t4 * 0.25))
Orig_noise.t5.n1 <- Original_noise((RNoise.t5 * 0.25))
Orig_noise.t1.n2 <- Original_noise((RNoise.t1 * 0.5))
Orig_noise.t2.n2 <- Original_noise((RNoise.t2 * 0.5))
Orig_noise.t3.n2 <- Original_noise((RNoise.t3 * 0.5))
Orig_noise.t4.n2 <- Original_noise((RNoise.t4 * 0.5))
Orig_noise.t5.n2 <- Original_noise((RNoise.t5 * 0.5))
Orig_noise.t1.n3 <- Original_noise((RNoise.t1 * 0.75))
Orig_noise.t2.n3 <- Original_noise((RNoise.t2 * 0.75))
Orig_noise.t3.n3 <- Original_noise((RNoise.t3 * 0.75))
Orig_noise.t4.n3 <- Original_noise((RNoise.t4 * 0.75))
Orig_noise.t5.n3 <- Original_noise((RNoise.t5 * 0.75))
Orig_noise.t1.n4 <- Original_noise(RNoise.t1)
Orig_noise.t2.n4 <- Original_noise(RNoise.t2)
Orig_noise.t3.n4 <- Original_noise(RNoise.t3)
Orig_noise.t4.n4 <- Original_noise(RNoise.t4)
Orig_noise.t5.n4 <- Original_noise(RNoise.t5)

Scenario1.dnp.table <- matrix(nrow = 20, ncol = 5)
Scenario1.dnp.table[,1] <- rep(1:4, each = 5)
Scenario1.dnp.table[,2] <- rep(c(25, 50, 75, 100, 150), times = 4)
Scenario1.dnp.table[,4] <- c(Resid_noise1.t1.n1, Resid_noise1.t2.n1, Resid_noise1.t3.n1,
                                     Resid_noise1.t4.n1, Resid_noise1.t5.n1, Resid_noise1.t1.n2, 
                                     Resid_noise1.t2.n2, Resid_noise1.t3.n2, Resid_noise1.t4.n2,
                                     Resid_noise1.t5.n2, Resid_noise1.t1.n3, Resid_noise1.t2.n3, 
                                     Resid_noise1.t3.n3, Resid_noise1.t4.n3, Resid_noise1.t5.n3,
                                     Resid_noise1.t1.n4, Resid_noise1.t2.n4, Resid_noise1.t3.n4,
                                     Resid_noise1.t4.n4, Resid_noise1.t5.n4)
Scenario1.dnp.table[,3] <- c(Orig_noise.t1.n1, Orig_noise.t2.n1, Orig_noise.t3.n1,
                                     Orig_noise.t4.n1, Orig_noise.t5.n1, Orig_noise.t1.n2, 
                                     Orig_noise.t2.n2, Orig_noise.t3.n2, Orig_noise.t4.n2,
                                     Orig_noise.t5.n2, Orig_noise.t1.n3, Orig_noise.t2.n3, 
                                     Orig_noise.t3.n3, Orig_noise.t4.n3, Orig_noise.t5.n3,
                                     Orig_noise.t1.n4, Orig_noise.t2.n4, Orig_noise.t3.n4,
                                     Orig_noise.t4.n4, Orig_noise.t5.n4)
Scenario1.dnp.table[,5] <- c(distortion.t1.n1, distortion.t2.n1, distortion.t3.n1,
                             distortion.t4.n1, distortion.t5.n1, distortion.t1.n2,
                             distortion.t2.n2, distortion.t3.n2, distortion.t4.n2,
                             distortion.t5.n2, distortion.t1.n3, distortion.t2.n3, 
                             distortion.t3.n3, distortion.t4.n3, distortion.t5.n3,
                             distortion.t1.n4, distortion.t2.n4, distortion.t3.n4,
                             distortion.t4.n4, distortion.t5.n4)
Scenario1.dnp.table <- as.data.frame(Scenario1.dnp.table)
colnames(Scenario1.dnp.table) <- c("Noise Level", "Trials", "Original Noise Power", "Residual Noise Power", "Distortion")
