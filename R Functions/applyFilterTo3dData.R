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

# REturn filtered data  
  return(filtered_signal)
  
}