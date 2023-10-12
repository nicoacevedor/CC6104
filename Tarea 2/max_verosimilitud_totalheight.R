library(ggplot2)
library(plotly)
data_raw <- read.csv("Body Measurements _ original.csv")

# totalheight_plot <- ggplot(
#   data = data_raw,
#   aes(x = TotalHeight, color = "density")
# )

# totalheight_dens <- geom_density(color = "#67B7D1")

# totalheight_hist <- geom_histogram(
#   aes(y = after_stat(density)),
#   bins = 10,
#   fill = "#67B7D1",
#   alpha = 0.5
# )

# final_plot <- totalheight_plot + totalheight_dens + totalheight_hist +
#   ylab("") + xlab("") + scale_color_manual(values = c("density" = "#67B7D1"))


# ggplotly(final_plot) %>%
#   layout(
#     xaxis = list(title = "TotalHeight"),
#     yaxis = list(title = "Frequency")
#   )

# likelihood <- function(mu, sigma) {
#   x <- data_raw$TotalHeight
#   n <- length(x)
#   x_mean <- mean(x)
#   x_var <- var(x)
#   first_sum <- (n * x_var) / (2 * sigma^2)
#   second_sum <- (n * (x_mean - mu)^2) / (2 * sigma^2)

#   -n * log(sigma) - first_sum - second_sum
# }

# mu <- seq(from = 20, to = 80, by = 0.5)
# sigma <- seq(from = 5, to = 23, by = 0.5)
# ll_plot <- Vectorize(likelihood)
# ll_plot <- outer(X = mu, Y = sigma, ll_plot)

# filled.contour(x = mu, y = sigma, z = ll_plot,
#                xlab = expression(mu), ylab = expression(sigma))

# likelihood <- function(param) {
#   mu <- param[1]
#   sigma <- param[2]
#   x <- data_raw$TotalHeight
#   n <- length(x)
#   x_mean <- mean(x)
#   x_var <- var(x)
#   first_sum <- (n * x_var) / (2 * sigma^2)
#   second_sum <- (n * (x_mean - mu)^2) / (2 * sigma^2)

#   n * log(sigma) + first_sum + second_sum
# }

# MLE <- nlminb(objective = likelihood, start = c(10, 10), lower = c(0.001, 0.001), upper = c(500, 500))



likelihood <- function(mu, sigma, sub = NULL) {
  x_org <- data_raw$TotalHeight
  if (is.null(sub))
    x <- x_org
  else
    x <- sample(x_org, sub)
  n <- length(x)
  x_mean <- mean(x)
  x_var <- var(x)
  first_sum <- (n * x_var) / (2 * sigma^2)
  second_sum <- (n * (x_mean - mu)^2) / (2 * sigma^2)

  -n * log(sigma) - first_sum - second_sum
}

mu <- seq(from = 20, to = 80, by = 0.5)
sigma <- 12

p <- plot_ly(x = mu, y = likelihood(mu, sigma),
             type = "scatter", name = "Todos los datos")
p <- add_trace(p, y = likelihood(mu, sigma, 100), name = "100 datos")
p <- add_trace(p, y = likelihood(mu, sigma, 300), name = "300 datos")
p <- add_trace(p, x = 48)
p <- p %>% layout(title = "Likelihood para valores de mu y valores de datos",
  xaxis = list(title = "mu"),
  yaxis = list(title = "valor de likelihood")
)
print(p)
