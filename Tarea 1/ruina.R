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
bet_vector <- vector(length = n_bet)
for (i in 1:n_bet) {
  final_money <- tail(ruina(bet = bet_val), 1)
  if (final_money <= 0) {
    win <- 0
  } else if (final_money >= 200) {
    win <- 1
  } else {
    print("Error: Funcion 'ruina' no llega a 0 o 200")
    break
  }
  bet_vector[i] <- win
}
hist(bet_vector, main = sprintf(""))
