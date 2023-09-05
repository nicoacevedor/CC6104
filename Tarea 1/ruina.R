ruina <- function(money = 100, bet = 5) {
  vec_money <- c(money)
  while (0 < money && money < 200) {
    game <- as.integer(runif(1, 1, 19))
    # print(sprintf("game: %s", game))
    sign <- if (game <= 8) 1 else -1
    money <- money + sign * bet
    vec_money <- append(vec_money, money)
  }
  return(vec_money)
}

# plot(ruina(), type="l", col="blue", xlab="N° de juegos", ylab="Fondos", main="Evolución de los fondos (apuesta = 5)")
# # plot(ruina(bet = 25), type="l", col="blue", xlab="N° de juegos", ylab="Fondos", main="Evolución de los fondos (apuesta = 25)")
# plot(ruina(bet = 50), type="l", col="blue", xlab="N° de juegos", ylab="Fondos", main="Evolución de los fondos (apuesta = 50)")

n_bet <- 5000
bet_val <- 50
bet5_vector <- vector(length = n_bet)
bet20_vector <- vector(length = n_bet)
bet50_vector <- vector(length = n_bet)
for (i in 1:n_bet) {
  final5_money <- tail(ruina(bet = 5), 1)
  final20_money <- tail(ruina(bet = 20), 1)
  final50_money <- tail(ruina(bet = 50), 1)
  win5 <- if (final5_money <= 0) 0 else 1
  win20 <- if (final20_money <= 0) 0 else 1
  win50 <- if (final50_money <= 0) 0 else 1
  bet5_vector[i] <- win5
  bet20_vector[i] <- win20
  bet50_vector[i] <- win50
}
t5 <- table(bet5_vector)
t20 <- table(bet20_vector)
t50 <- table(bet50_vector)
print(sprintf("Apuesta = 5 -> E = %d, F = %d, Prob = %f", t5["1"], t5["0"], t5["1"]/(t5["1"] + t5["0"])))
hist(bet5_vector, main = sprintf("Éxitos y fracasos para apuesta = 5"))
print(sprintf("Apuesta = 20 -> E = %d, F = %d, Prob = %f", t20["1"], t20["0"], t20["1"]/(t20["1"] + t20["0"])))
hist(bet20_vector, main = sprintf("Éxitos y fracasos para apuesta = 20"))
print(sprintf("Apuesta = 50 -> E = %d, F = %d, Prob = %f", t50["1"], t50["0"], t50["1"]/(t50["1"] + t50["0"])))
hist(bet50_vector, main = sprintf("Éxitos y fracasos para apuesta = 50"))
