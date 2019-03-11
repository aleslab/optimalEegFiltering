applyFilterTo3dData <- function(filter,signal) {
  #Data is assumed ordered sensor x time x trial

  origDim <- dim(signal)
  dim(signal) <- c(origDim[1], origDim[2] * origDim[3])

filtered_signal <- filter %*% signal 
#Now that signal is filtered, return to original 3d structure
dim(filtered_signal) <-c(origDim[1], origDim[2], origDim[3])

return(filtered_signal)
}