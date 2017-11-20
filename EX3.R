# Name: Omar Amr
# Matrikel-Nr: k11776960

set.seed(4000)

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

# plot(x,y_with_noise,col='blue',xlab='x',main='TODO')
# points(x,y,col='red')
######################################################

training_set <- create_training_set(4000, -1, 3)
x0_correct_value <- evaluate_training_set(1.8)
  
bias <- vector()
variance <- vector()
estimated_value_variance <- vector()
EPE <- vector()
for (n in seq(1, 7))
{
  bias_per_n <- vector()
  variance_per_n <- vector()
  variance_per_set <- vector()
  for (counter in seq(1, 200))
  {
    startIndex <- (20 * (counter - 1)) + 1
    endIndex <- startIndex + 19
    current_training_set <- training_set[startIndex: endIndex]
    current_training_set_results <- evaluate_training_set(current_training_set)
    model <- lm(current_training_set_results ~ poly(current_training_set,n))
    predicted_value <- predict(model, newdata = data.frame(current_training_set=1.8))
    bias_per_n[counter] <- predicted_value
    variance_per_n[counter] <- (predicted_value - mean(predicted_value)) ^ 2
    variance_per_set[counter] <- predicted_value
  }
  bias[n] <- (mean(bias_per_n) - x0_correct_value) ^ 2
  variance[n] <- mean(variance_per_n)
  estimated_value_variance[n] <- var(variance_per_set)
  EPE[n] <- bias[n] + variance[n] + estimated_value_variance[n]
}

plot(seq(1, 7), EPE, col='red', xlab="n (Model's Polynomial Degree)", ylab = "EPE,   Bias^2,  Variance, etc", main='EX3 - Bias-Variance Decomposition', type = "l")
lines(seq(1, 7), bias, col='blue')
lines(seq(1, 7), estimated_value_variance, col='green')
lines(seq(1, 7), variance, col='orange')
legend("topright", legend=c("EPE", "Bias ^ 2", "Variance in Estimated Values", "Variance(y|x0)"), col=c("red", "blue", "green", "orange"), lty=1:1, cex=0.8)
