library(dplyr)
library(tibble)
library(ggplot2)

dispersion_ratio <- function(model) {
  sum(residuals(model, type = "pearson")^2) / model$df.residual
}

check_overdispersion <- function(model) {
  ratio <- dispersion_ratio(model)

  tibble(
    check = "Overdispersion",
    value = round(ratio, 3),
    interpretation = case_when(
      ratio < 1.2 ~ "No strong evidence of overdispersion.",
      ratio < 2 ~ "Possible mild overdispersion. Consider Quasi-Poisson.",
      TRUE ~ "Strong overdispersion. Consider Negative Binomial or Quasi-Poisson."
    )
  )
}

check_zero_inflation <- function(data, response, model) {
  observed_zeros <- sum(data[[response]] == 0)
  expected_zeros <- sum(dpois(0, fitted(model)))

  ratio <- observed_zeros / expected_zeros

  tibble(
    check = "Zero inflation",
    observed_zeros = observed_zeros,
    expected_zeros = round(expected_zeros, 2),
    zero_ratio = round(ratio, 3),
    interpretation = case_when(
      ratio < 1.5 ~ "No strong evidence of excess zeros.",
      ratio < 2 ~ "Possible excess zeros. Consider checking a zero-inflated model.",
      TRUE ~ "Strong evidence of excess zeros. Consider a zero-inflated model."
    )
  )
}

make_poisson_diagnostic_plot <- function(model) {
  data.frame(
    fitted = fitted(model),
    pearson_residual = residuals(model, type = "pearson")
  ) %>%
    ggplot(aes(x = fitted, y = pearson_residual)) +
    geom_point(alpha = 0.7) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(
      x = "Fitted values",
      y = "Pearson residuals",
      title = "Poisson Diagnostic Plot"
    ) +
    theme_minimal()
}