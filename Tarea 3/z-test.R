data <- read.csv("marketing_campaign.csv", sep = "\t")

graduation <- data[data$Education == "Graduation", "Income"]
master <- data[data$Education == "Master", "Income"]
phd <- data[data$Education == "PhD", "Income"]

std_grad <- 28180
std_master <- 20160
std_phd <- 20615


z_test <- function(data1 = NULL, data1_name = NULL, sigma1 = 0.5,
                   data2 = NULL, data2_name = NULL, sigma2 = 0.5,
                   mu = 0, test_type = c("one-sided", "two-sided"),
                   verbose = TRUE) {

  # Condiciones para evitar errores en la función
  length_test_type_condition <- length(test_type) == 1
  name_test_type_condition <- (test_type %in% c("menor", "mayor", "two-sided"))
  if (!(length_test_type_condition && name_test_type_condition)) {
    print("Por favor escoge un tipo de Test: ´mayor´, ´menor´ o ´two-sided´ ")
    return()
  } else if (is.null(data1)) {
    print("Por favor ingresa algún valor en data1")
    return()
  }

  # Nombre data1
  if (is.null(data1_name))
    data_deparse <- deparse(substitute(data1))
  else
    data_deparse <- data1_name

  # Z-Score
  if (is.null(data2)) {
    z_score <- (mean(data1, na.rm = TRUE) - mu) / (sigma1 * sqrt(length(data1)))
    output <- "One"
  } else {
    mu_1 <- mean(data1, na.rm = TRUE)
    mu_2 <- mean(data2, na.rm = TRUE)
    n_1 <- length(data1)
    n_2 <- length(data2)
    z_score <- (mu_1 - mu_2) / sqrt(sigma1^2 / n_1 + sigma2^2 / n_2)

    # Nombre data2
    output <- "Two"
    if (is.null(data2_name))
      data_deparse <- paste(data_deparse, "y", deparse(substitute(data2)))
    else
      data_deparse <- paste(data_deparse, "y", data2_name)
  }

  # P-Value
  if (test_type == "menor")
    p_value <- pnorm(z_score)

  else if (test_type == "mayor")
    p_value <- 1 - pnorm(z_score)

  else if (test_type == "two-sided")
    p_value <- 2 * pnorm(-abs(z_score))

  # Texto de Salida
  if (verbose) {
    cat("\n\t", output, "-sample Z-Test:\n\nData analizada: ",
      data_deparse, "\nZ = ", z_score,
      " P-value = ", p_value,
      "\n\n", sep = ""
    )
  }

  return(p_value)
}


z_test_multiple_testing <- function(data = NULL, sigma = NULL,
                                    test_type = c("one-sided", "two-sided"),
                                    verbose = TRUE) {

  # Condiciones para evitar errores en la función
  length_test_type_condition <- length(test_type) == 1
  name_test_type_condition <- (test_type %in% c("menor", "mayor", "two-sided"))
  nullity_condition <- is.null(data) || is.null(sigma)
  dimension_condition <- !nullity_condition && (length(data) == length(sigma))
  if (!(length_test_type_condition && name_test_type_condition)) {
    print("Por favor escoge un tipo de Test: ´mayor´, ´menor´ o ´two-sided´ ")
    return()
  } else if (!dimension_condition) {
    cat(
      "Cantidad incompatible de datos y sigmas\n",
      "length(data) = ", length(data), "\n",
      "length(sigma) = ", length(sigma), "\n\n", sep = ""
    )
  }

  # Nombres de los datos estudiados
  data_names <- sapply(substitute(data), deparse)[-1]

  # Cálculo del p-value para cada par de datos
  indexes <- seq_along(data)
  p_value_list <- list()
  for (i in indexes) {
    for (j in indexes) {
      if (j <= i)
        next
      var1 <- data_names[i]
      var2 <- data_names[j]
      p_value <- z_test(
        data1 = data[[i]], data2 = data[[j]],
        data1_name = var1, data2_name = var2,
        sigma1 = sigma[i], sigma2 = sigma[j],
        test_type = test_type, verbose = verbose
      )
      key <- paste(var1, "_", var2, sep = "")
      p_value_list[key] <- p_value
    }
  }

  return(p_value_list)
}

results <- z_test_multiple_testing(
  data = list(graduation, master, phd),
  sigma = c(std_grad, std_master, std_phd),
  test_type = "two-sided",
  verbose = TRUE
)

print(results)