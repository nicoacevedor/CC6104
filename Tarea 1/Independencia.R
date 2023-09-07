n_lan <- 1000000
dice1 <- sample(1:6, n_lan, replace = TRUE)
dice2 <- sample(1:6, n_lan, replace = TRUE)

favorable_a <- c(1, 2, 6)
favorable_b <- c(1, 2, 3, 4)
favorable_ayb <- c(1, 2)

prob_axb <- vector(length = n_lan)
prob_ayb <- vector(length = n_lan)

cum_a <- 0
cum_b <- 0
cum_ayb <- 0

for (i in 1:n_lan) {
  a <- FALSE
  b <- FALSE
  if (dice1[i] %in% favorable_a) {
    cum_a <- cum_a + 1
    a <- TRUE
  }
  if (dice2[i] %in% favorable_b) {
    cum_b <- cum_b + 1
    b <- TRUE
  }
  cum_ayb <- if (a && b) (cum_ayb + 1) else cum_ayb

  prob_axb[i] <- (cum_a / i) * (cum_b / i)
  prob_ayb[i] <- cum_ayb / i
}

plot(1, ann = FALSE, type = "n", xlim = c(0, n_lan+1), ylim = c(0, 1))
lines(seq(1, n_lan), prob_axb, col = "blue")
lines(seq(1, n_lan), prob_ayb, col = "red")
legend(8e+05, 1, legend = c("P(A)*P(B)", "P(A∩B)"), fill = c("blue", "red"))

# n_lan <- 1000000
# dice <- sample(1:6, n_lan, replace = TRUE)

# favorable_a <- c(1, 2, 6)
# favorable_b <- c(1, 2, 3)
# favorable_ayb <- c(1, 2)

# prob_axb <- vector(length = n_lan)
# prob_ayb <- vector(length = n_lan)

# cum_a <- 0
# cum_b <- 0
# cum_ayb <- 0

# for (i in 1:n_lan) {
#   a <- FALSE
#   b <- FALSE
#   if (dice[i] %in% favorable_a) {
#     cum_a <- cum_a + 1
#     a <- TRUE
#   }
#   if (dice[i] %in% favorable_b) {
#     cum_b <- cum_b + 1
#     b <- TRUE
#   }
#   cum_ayb <- if (a && b) (cum_ayb + 1) else cum_ayb

#   prob_axb[i] <- (cum_a / i) * (cum_b / i)
#   prob_ayb[i] <- cum_ayb / i
# }

# plot(1, ann = FALSE, type = "n", xlim = c(0, n_lan + 1), ylim = c(0, 1))
# lines(seq(1, n_lan), prob_axb, col = "blue")
# lines(seq(1, n_lan), prob_ayb, col = "red")
# legend(8e+05, 1, legend = c("P(A)*P(B)", "P(A∩B)"), fill = c("blue", "red"))