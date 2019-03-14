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