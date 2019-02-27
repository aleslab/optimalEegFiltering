mse <- function(y_est, y_true) {
  mu <- mean(y_true)
  error <- mean((y_est - y_true)^2)
  return(error)
}