df <- read.csv("no+mordidas.csv")

bites_1 <- df$bites_month_1
bites_2 <- df$bites_month_2
data <- c(bites_1, bites_2)


n_data <- length(data)
n_bites <- sum(data)
p_grid <- seq(from = 0, to = 1, length.out = n_data)
prior <- rep(1, n_data)
likelihood <- dbinom(x = n_bites, size = n_data, prob = p_grid)
posterior <- likelihood * prior
posterior_std <- posterior / sum(posterior)
p <- p_grid[which.max(posterior_std)]
cat(
  "max posterior:", max(posterior_std),
  "\np:", p, "\n"
)

# simulation <- rbinom(n = 1e4, size = 500, prob = p)
# hist(simulation, breaks = seq(from = min(simulation), to = max(simulation)))
# abline(v = 248, col = "red")

bit_again <- bites_2[bites_1]
n_bites <- sum(bit_again)
posterior <- dbeta(p_grid, n_bites + 1, length(bites_2) - n_bites + 1)
p <- p_grid[which.max(posterior)]
print(p)
cat("length(bites_1) = ", length(bites_1), "\n", sep = "")

simulation <- rbinom(n = 1e4, size = 250, prob = p)
hist(simulation, breaks = seq(from = min(simulation), to = max(simulation)))
abline(v = n_bites, col = "red")
