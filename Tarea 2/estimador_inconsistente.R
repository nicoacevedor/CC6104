library(plotly)
n_max <- 50000

estimator <- vector(length = n_max)
bernoulli <- rbinom(n_max, size = 1, prob = 0.5)
noise <- rnorm(n_max)
T <- vector(length = n_max)

for (k in 1:n_max) {
  estimator[k] <- sum(bernoulli[1:k]) / k
  T[k] = estimator[k] + noise[k]
}

plot <- plot_ly(
  x = 1:n_max,
  y = T,
  mode = "lines",
  type = "scatter",
  name = "Experimentos",
  line = list(color = "blue")
) %>%
add_trace(
  x = 1:n_max,
  y = 0.5,
  name = "p = 0.5",
  line = list(color = "red")
) %>%
layout(
  title = "Estimador para X ~ Bernoulli(p) con p = 0.5",
  xaxis = list(title = sprintf("Pasos (%d)", n_max)),
  yaxis = list(title = "Probabilidad")
)

print(plot)
