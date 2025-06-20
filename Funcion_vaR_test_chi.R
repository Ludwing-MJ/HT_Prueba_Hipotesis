# Función personalizada para prueba de hipótesis de una varianza
var_test_chi <- function(x = NULL, n = NULL, s2 = NULL, sigma0_2, 
                         alternative = "two.sided", alpha = 0.05) {
  
  # Validación de argumentos
  if (is.null(x) && (is.null(n) || is.null(s2))) {
    stop("Debe proporcionar 'x' (vector de datos) o 'n' y 's2' (estadísticos muestrales)")
  }
  
  if (!is.null(x) && (!is.null(n) || !is.null(s2))) {
    warning("Se proporcionaron datos y estadísticos. Se usarán los datos 'x'")
  }
  
  # Calcular estadísticos si se proporcionan los datos
  if (!is.null(x)) {
    n <- length(x)
    s2 <- var(x)
  }
  
  # Validar alternative
  alternative <- match.arg(alternative, c("two.sided", "less", "greater"))
  
  # Calcular estadístico chi-cuadrado
  chi_sq <- (n - 1) * s2 / sigma0_2
  df <- n - 1
  
  # Calcular valor-p según el tipo de prueba
  if (alternative == "two.sided") {
    # Para prueba bilateral, usar el mínimo de las dos colas multiplicado por 2
    p_value <- 2 * min(pchisq(chi_sq, df), pchisq(chi_sq, df, lower.tail = FALSE))
  } else if (alternative == "greater") {
    p_value <- pchisq(chi_sq, df, lower.tail = FALSE)
  } else { # alternative == "less"
    p_value <- pchisq(chi_sq, df, lower.tail = TRUE)
  }
  
  # Decisión
  decision <- ifelse(p_value < alpha, "Rechazar H0", "No rechazar H0")
  
  # Valor crítico
  if (alternative == "two.sided") {
    crit_lower <- qchisq(alpha/2, df)
    crit_upper <- qchisq(1 - alpha/2, df)
    critical_value <- c(crit_lower, crit_upper)
  } else if (alternative == "greater") {
    critical_value <- qchisq(1 - alpha, df)
  } else { # alternative == "less"
    critical_value <- qchisq(alpha, df)
  }
  
  # Crear objeto de resultado
  result <- list(
    statistic = chi_sq,
    parameter = df,
    p.value = p_value,
    critical.value = critical_value,
    alternative = alternative,
    method = "Prueba de hipótesis para una varianza (Chi-cuadrado)",
    data.name = deparse(substitute(x)),
    sample.size = n,
    sample.variance = s2,
    null.variance = sigma0_2,
    alpha = alpha,
    decision = decision
  )
  
  class(result) <- "var_test_custom"
  return(result)
}

# Método print personalizado para mostrar resultados de forma clara
print.var_test_custom <- function(x, ...) {
  cat("\n")
  cat(x$method, "\n")
  cat("Datos:", x$data.name, "\n")
  cat("\n")
  cat("Hipótesis:\n")
  if (x$alternative == "two.sided") {
    cat("H0: sigma^2 =", x$null.variance, "\n")
    cat("Ha: sigma^2 ≠", x$null.variance, "\n")
  } else if (x$alternative == "greater") {
    cat("H0: sigma^2 ≤", x$null.variance, "\n")
    cat("Ha: sigma^2 >", x$null.variance, "\n")
  } else {
    cat("H0: sigma^2 ≥", x$null.variance, "\n")
    cat("Ha: sigma^2 <", x$null.variance, "\n")
  }
  cat("\n")
  cat("Estadísticos de la muestra:\n")
  cat("n =", x$sample.size, "\n")
  cat("s^2 =", round(x$sample.variance, 4), "\n")
  cat("\n")
  cat("Estadístico de prueba:\n")
  cat("Chi-cuadrado =", round(x$statistic, 4), "\n")
  cat("Grados de libertad =", x$parameter, "\n")
  cat("\n")
  cat("Valor crítico(s):\n")
  if (length(x$critical.value) == 2) {
    cat("Chi^2(", x$alpha/2, ",", x$parameter, ") =", round(x$critical.value[1], 4), "\n")
    cat("Chi^2(", 1-x$alpha/2, ",", x$parameter, ") =", round(x$critical.value[2], 4), "\n")
  } else {
    cat("Chi^2 crítico =", round(x$critical.value, 4), "\n")
  }
  cat("\n")
  cat("Valor-p =", round(x$p.value, 6), "\n")
  cat("Nivel de significancia =", x$alpha, "\n")
  cat("\n")
  cat("Decisión:", x$decision, "\n")
  cat("\n")
}