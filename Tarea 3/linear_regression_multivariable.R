data <- read.csv("insurance.csv")

# One Hot Encoding
data$female <- ifelse(data$sex == "female", 1, 0)
data$male <- ifelse(data$sex == "male", 1, 0)
data$southwest <- ifelse(data$region == "southwest", 1, 0)
data$southeast <- ifelse(data$region == "southeast", 1, 0)
data$northwest <- ifelse(data$region == "northwest", 1, 0)
data$northeast <- ifelse(data$region == "northeast", 1, 0)
data <- data[, !names(data) %in% c("sex", "region")]

smoker <- data[data$smoker == "yes", !names(data) == "smoker"]
non_smoker <- data[data$smoker == "no", !names(data) == "smoker"]

get_max_correlation_column <- function(df, column) {
  cor_df <- as.data.frame(as.table(cor(df)))
  cor_df <- cor_df[cor_df$Var1 == column, ]
  cols <- c("Var1", "Var2")
  cor_df[cols] <- lapply(cor_df[cols], as.character)
  max_cor_1 <- max(abs(cor_df[cor_df$Freq != 1, ]$Freq))
  next_max <- abs(cor_df$Freq) < abs(max_cor_1)
  max_cor_2 <- max(abs(cor_df[next_max, ]$Freq))
  variable_1 <- cor_df[abs(cor_df$Freq) == max_cor_1, ]$Var2
  variable_2 <- cor_df[abs(cor_df$Freq) == max_cor_2, ]$Var2
  list("var1" = variable_1, "var2" = variable_2)
}

linear_regression_multivar <- function(data, target) {
  matrix_data <- data.matrix(data)
  colnames(matrix_data) <- NULL
  beta <- solve(t(matrix_data) %*% matrix_data) %*% t(matrix_data) %*% target
  list("beta_0" = beta[1], "beta_1" = beta[2], "beta_2" = beta[3])
}

smoker <- data[data$smoker == "yes", !names(data) == "smoker"]
non_smoker <- data[data$smoker == "no", !names(data) == "smoker"]

smoker_variables <- get_max_correlation_column(smoker, "charges")
non_smoker_variables <- get_max_correlation_column(non_smoker, "charges")

x_smoker <- data.matrix(smoker[, names(smoker) %in% smoker_variables])
x_smoker <- cbind(rep(1, nrow(x_smoker)), x_smoker)
y_smoker <- smoker[, "charges"]
reg <- linear_regression_multivar(x_smoker, y_smoker)
beta <- unlist(reg)
y_hat_smoker <- x_smoker %*% beta

SST_smoker <- sum((y_smoker - mean(y_smoker))^2)
SSE_smoker <- sum((y_smoker - y_hat_smoker)^2)
R2_smoker <- 1 - (SSE_smoker / SST_smoker)

print(R2_smoker)