plot_gauss <- function(x) {
  avg <- mean(x)
  std <- sd(x)
  x_axis <- seq(min(x), max(x), length = length(x))
  y_axis <- dnorm(x_axis, mean = avg, sd = std)
  hist(x)
  lines(x_axis, y_axis, type = "l", col = "red")
}

sample_size <- 10000

iter <- 1000
lambda <- 5
poisson_vector <- vector(length = iter)

for (i in 1:iter) {
  new_vector <- rpois(n = sample_size, lambda = lambda)
  poisson_vector[i] <- mean(new_vector)
}

plot_gauss(poisson_vector)


iter <- 1000
avg <- 1.5
std <- 0.75
norm_vector <- vector(length = iter)

for (i in 1:iter) {
  new_vector <- rnorm(n = sample_size, mean = avg, sd = std)
  norm_vector[i] <- mean(new_vector)
}

plot_gauss(norm_vector)


iter <- 5000
shape <- 2
scale <- 1.5
gamma_vector <- vector(length = iter)

for (i in 1:iter) {
  new_vector <- rgamma(n = sample_size, shape = shape, scale = scale)
  gamma_vector[i] <- mean(new_vector)
}

plot_gauss(gamma_vector)
