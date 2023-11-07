data <- read.csv("insurance.csv")

# One Hot Encoding
data$female <- ifelse(data$sex == "female", 1, 0)
data$male <- ifelse(data$sex == "male", 1, 0)
data$southwest <- ifelse(data$region == "southwest", 1, 0)
data$southeast <- ifelse(data$region == "southeast", 1, 0)
data$northwest <- ifelse(data$region == "northwest", 1, 0)
data$northeast <- ifelse(data$region == "northeast", 1, 0)
data <- data[, !names(data) %in% c("sex", "region")]


get_max_correlation_column <- function(df, column) {
  cor_df <- as.data.frame(as.table(cor(df)))
  cor_df <- cor_df[cor_df$Var1 == column, ]
  cols <- c("Var1", "Var2")
  cor_df[cols] <- lapply(cor_df[cols], as.character)
  max_cor <- max(abs(cor_df[cor_df$Freq != 1, ]$Freq))
  variable <- cor_df[abs(cor_df$Freq) == max_cor, ]$Var2
  variable
}


# Separation of smokers and non smokers
smoker <- data[data$smoker == "yes", !names(data) == "smoker"]
smoker_variable <- get_max_correlation_column(smoker, "charges")
x_smoker <- smoker[, smoker_variable]
y_smoker <- smoker$charges

non_smoker <- data[data$smoker == "no", !names(data) == "smoker"]
non_smoker_variable <- get_max_correlation_column(non_smoker, "charges")
x_non_smoker <- non_smoker[, non_smoker_variable]
y_non_smoker <- non_smoker$charges



linear_regression <- function(data, target) {
  x_mean <- mean(data)
  y_mean <- mean(target)
  numerator <- sum((data - x_mean) * (target - y_mean))
  denominator <- sum((data - x_mean)^2)

  beta_1 <- numerator / denominator
  beta_0 <- y_mean - beta_1 * x_mean
  list("intercept" = beta_0, "slope" = beta_1)
}

reg_smoker <- linear_regression(x_smoker, y_smoker)
slope <- reg_smoker$slope
intercept <- reg_smoker$intercept
y_hat_smoker <- intercept + slope * x_smoker

plot(x_smoker, y_smoker, ylab = "charges", xlab = smoker_variable)
abline(a = intercept, b = slope, col = "red")
title("Linear Regression for smokers")



SST_smoker <- sum((y_smoker - mean(y_smoker))^2)
SSE_smoker <- sum((y_smoker - y_hat_smoker)^2)
R2_smoker <- 1 - (SSE_smoker / SST_smoker)
print("R2")
print(R2_smoker)