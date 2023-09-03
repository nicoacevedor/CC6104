n_lan <- 6000000
# dice1 <- sample(1:6, n_lan, replace = TRUE)
# dice2 <- sample(1:6, n_lan, replace = TRUE)
# dice_count1 <- table(dice1)
# dice_count2 <- table(dice2)

# favorable_a <- c(1, 2, 6)
# favorable_b <- c(1, 2, 3, 4)
# favorable_ayb <- c(1, 2)

# print(dice_count1[favorable_a])
# print(dice_count2[favorable_b])

# count_a <- sum(dice_count1[favorable_a])
# count_b <- sum(dice_count2[favorable_b])
# count_ayb <- 0
for (i in 1:n_lan) {
  if (dice1[i] %in% favorable_a && dice2[i] %in% favorable_b) {
    count_ayb <- count_ayb + 1
  }
}

# prob_a <- count_a / n_lan
# prob_b <- count_b / n_lan
# prob_ayb <- count_ayb / n_lan

# par(mfrow = c(1, 2))

# hist(dice1, breaks = 0.5:6.5)
# hist(dice2, breaks = 0.5:6.5)

dice <- sample(1:6, n_lan, replace = TRUE)
dice_count <- table(dice)

favorable_a <- c(1, 2, 6)
favorable_b <- c(1, 2, 3)
favorable_ayb <- c(1, 2)

count_a <- sum(dice_count[favorable_a])
count_b <- sum(dice_count[favorable_b])
count_ayb <- sum(dice_count[favorable_ayb])

prob_a <- count_a / n_lan
prob_b <- count_b / n_lan
prob_ayb <- count_ayb / n_lan

par(mfrow = c(1, 1))
hist(dice, breaks = 0.5:6.5)


text_1 <- sprintf("P(A) =  %s", prob_a)
text_2 <- sprintf("P(B) =  %s", prob_b)
text_3 <- sprintf("P(A) * P(B) =  %s", prob_a * prob_b)
text_4 <- sprintf("P(A y B) = %s", prob_ayb)
text_5 <- sprintf("Diferencia = %s", abs((prob_a * prob_b) - prob_ayb))

print(text_1)
print(text_2)
print(text_3)
print(text_4)
print(text_5)
