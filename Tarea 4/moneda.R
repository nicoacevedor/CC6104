library(ggplot2)
library(rethinking)

dataMoneda <- read.csv("moneda.csv", header = TRUE)

grid_approx <- function(data, size_exp) {
  n_heads <- sum(data$Resultado[1:size_exp])
  p_grid <- seq(from = 0, to = 1, length.out = size_exp)
  prior <- rep(1, size_exp)
  likelihood <- dbinom(x = n_heads, size = size_exp, prob = p_grid)
  posterior <- likelihood * prior
  posterior_std <- posterior / sum(posterior)
  return(list(p_grid = p_grid, posterior = posterior_std))
}

# plot(p_grid, posterior_std, type = "b",
#   xlab = "Probability of heads",
#   ylab = "Posterior probability",
# )

laplace_approx <- function(data, size_exp) {
  n_heads <- sum(data$Resultado[1:size_exp])
  qa <- quap(
    alist(
      heads ~ dbinom(heads + tails, p),
      p ~ dunif(0, 1)
    ),
    data = list(heads = n_heads, tails = size_exp - n_heads),
    control = list(ndeps = 1e-6)
  )
  return(qa)
}

experiment <- grid_approx(dataMoneda, nrow(dataMoneda))
p_grid <- experiment$p_grid
posterior <- experiment$posterior

samples <- sample(p_grid, size = 1e4, replace = TRUE, prob = posterior)
print(HPDI(samples, prob = 0.5))