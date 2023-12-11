gamma_func <- function(x, shape, rate) {
  if (x > 0)
    return(x^(shape - 1) * exp(-rate * x))
  else
    return(0)
}


metropolis <- function(theta_0, n_iter, alpha, beta) {
  theta <- vector(length = n_iter + 1)
  current <- theta_0
  for (k in 1:n_iter + 1) {
    theta[k] <- current
    proposal <- rnorm(1, mean = theta[k], sd = 1)
    prob_prop <- gamma_func(proposal, shape = alpha, rate = beta)
    prob_current <- gamma_func(current, shape = alpha, rate = beta)
    ratio <- prob_prop / prob_current
    prob_decision <- min(1, ratio)
    decision <- rbinom(1, 1, prob_decision)
    current <- ifelse(decision == 1, proposal, current)
    current <- ifelse(current > 0, current, 0)
  }
  return(theta[-1])
}

outcomes <- metropolis(1, 1e5, 5, 0.2)

hist(outcomes)
