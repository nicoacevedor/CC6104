library(ggplot2)
library(plotly)
data_raw <- read.csv("Body Measurements _ original.csv")

# data_plot <- ggplot(
#   data = data_raw,
#   aes(x = ShoulderToWaist, color = "density")
# )

# data_dens <- geom_density(color = "#67B7D1")

# data_hist <- geom_histogram(
#   aes(y = after_stat(density)),
#   bins = 10,
#   fill = "#67B7D1",
#   alpha = 0.5
# )

# final_plot <- data_plot + data_dens + data_hist +
#   ylab("") + xlab("") + scale_color_manual(values = c("density" = "#67B7D1"))


# plot_plot <- ggplotly(final_plot) %>%
#   layout(
#     xaxis = list(title = "ShoulderToWaist"),
#     yaxis = list(title = "Frequency")
#   )

# likelihood <- function(mu, sigma) {
#   x <- data_raw$ShoulderToWaist
#   # n <- length(x)
#   # x_mean <- mean(x)
#   # x_var <- var(x)
#   sum(log(dnorm(x, mean = mu, sd = sigma)))
# }

# mu <- seq(from = 10, to = 30, by = 0.5)
# sigma <- seq(from = 3, to = 10, by = 0.5)
# ll_plot <- Vectorize(likelihood)
# ll_plot <- outer(X = mu, Y = sigma, ll_plot)

# filled.contour(x = mu, y = sigma, z = ll_plot,
#                xlab = expression(mu), ylab = expression(sigma))

likelihood <- function(param) {
  mu <- param[1]
  sigma <- param[2]
  x <- data_raw$ShoulderToWaist
  -sum(log(dnorm(x, mean = mu, sd = sigma)))
}

MLE <- nlminb(objective = likelihood, start = c(10, 10), lower = c(0.001, 0.001), upper = c(500, 500))
cat("mu = ", MLE$par[1], ", sigma = ", MLE$par[2])