create_training_set <- function(set_size, start_value, end_value)
{
  return(runif(set_size, start_value, end_value))
}

evaluate_training_set <- function(training_set)
{
  return((0.6 * training_set ^ 4) + (2 * training_set ^ 3) - (8 * training_set ^ 2))
}

x <- create_training_set(200, -1, 3)
y <- evaluate_training_set(x)
noise <- rnorm(200, mean = 0, sd = sqrt(0.09))
y_with_noise <- y + noise

plot(x,y_with_noise,col='blue',xlab='x',main='TODO')
points(x,y,col='red')

######################################################
training_set <- create_training_set(4000, -1, 3)
# training_set_results <- evaluate_training_set(training_set)
x0_correct_value <- evaluate_training_set(1.8)
  

error <- vector()
for (n in seq(1, 7))
{
  error_n <- 0
  for (counter in seq(1, 200))
  {
    startIndex <- (20 * (counter - 1)) + 1
    endIndex <- startIndex + 19
    current_training_set <- training_set[startIndex: endIndex]
    current_training_set_results <- evaluate_training_set(current_training_set)
    model <- lm(current_training_set_results ~ poly(current_training_set,n))
    predicted_value <- predict(model, newdata = data.frame(current_training_set=1.8))
    error_n <- error_n + ((x0_correct_value - predicted_value) ^ 2)
  }
  error[n] <- error_n/400
}

plot(seq(1, 7),error,col='blue',xlab='x',main='TODO', type = "l")#EPE
lines(seq(1, 7),error/1.5,col='red',xlab='x',main='TODO', type = "l")
lines(seq(1, 7),error/3,col='green',xlab='x',main='TODO', type = "l")


