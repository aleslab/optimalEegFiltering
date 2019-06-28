library(R.matlab) 

#Fist load up a matrix that represents sources from all over the brain
#To be exact 20484 sources spread evenly over the whole cortex.
pathname <- file.path("eegForwardData", "fwdAllBrain.mat")
dataNoise <- readMat(pathname)

#128 x 20484
nElec <- dim(dataNoise$fwdAllBrain)[1]
nNoiseSources <- dim(dataNoise$fwdAllBrain)[2]
nTime <-600


randomActivity <- array(rnorm(nNoiseSources * nTime , mean = 0, sd = 1),dim = c(nNoiseSources, nTime))

simBrainNoise <- dataNoise$fwdAllBrain %*% randomActivity

#Now load the set of sources that are only taken from a subset of visual areas in the brain
#this simulates sources typical of "visal evoked potentials" generated/concentrated in visual areas
pathname <- file.path("eegForwardData", "fwdOnlyVisual.mat")
dataSignal <- readMat(pathname)

nSources <- dim(dataSignal$fwdOnlyVisual)[2]
randomActivity <- array(rnorm(nSources * nTime , mean = 0, sd = 1),dim = c(nSources, nTime))

simBrainSignal <- dataSignal$fwdOnlyVisual %*% randomActivity

