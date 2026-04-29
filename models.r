library(dplyr)
library(tibble)

#fits the model
fit_poisson_model <- function(formula_text, data) {
  glm(
    formula = as.formula(formula_text),
    data = data,
    family = poisson(link = "log")
  )
}

#function that displays the summary of the model (including confidence interval)
tidy_poisson_model <- function(model, alpha = 0.05) {
  coefs <- summary(model)$coefficients
  ci <- confint(model, level = 1 - alpha)

  data.frame(
    term = rownames(coefs),
    estimate = coefs[, "Estimate"],
    std.error = coefs[, "Std. Error"],
    statistic = coefs[, "z value"],
    p.value = coefs[, "Pr(>|z|)"],
    incidence_rate_ratio = exp(coefs[, "Estimate"]),
    conf.low.irr = exp(ci[, 1]),
    conf.high.irr = exp(ci[, 2]),
    row.names = NULL
  )
}

#function that displays how good the model fit is (deviance and AIC)
poisson_gof_table <- function(model) {
  tibble(
    statistic = c(
      "Null deviance",
      "Residual deviance",
      "AIC",
      "Residual df"
    ),
    value = c(
      model$null.deviance,
      model$deviance,
      AIC(model),
      model$df.residual
    )
  )
}