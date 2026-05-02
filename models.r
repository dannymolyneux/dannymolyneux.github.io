library(dplyr)
library(tibble)
library(MASS)
library(pscl)

#fits the model
fit_count_model <- function(formula_text, data, model_type) {
  form = as.formula(formula_text)
  if (model_type == "Poisson") {
    glm(form, data = data, family = poisson(link = "log"))

  } else if (model_type == "Quasi-Poisson") {
    glm(form, data = data, family = quasipoisson(link = "log"))

  } else if (model_type == "Negative Binomial") {
    MASS::glm.nb(form, data = data)

  } else if (model_type == "Zero-Inflated Poisson") {
    pscl::zeroinfl(form, data = data, dist = "poisson")

  } else if (model_type == "Zero-Inflated Negative Binomial") {
    pscl::zeroinfl(form, data = data, dist = "negbin")

  } else {
    stop("Unsupported model type.")
  }
}


#function that displays the summary of the model (including confidence interval)
tidy_count_model <- function(model, model_type, alpha = 0.05) {
  #anything but zero-inflated (all have same summary format)
  if (model_type %in% c("Poisson", "Quasi-Poisson", "Negative Binomial")) {
    coefs <- summary(model)$coefficients
    ci <- confint(model, level = 1 - alpha)

    data.frame(
      term = rownames(coefs),
      estimate = coefs[, "Estimate"],
      std.error = coefs[, "Std. Error"],
      statistic = coefs[, 3],
      p.value = coefs[, 4],
      incidence_rate_ratio = exp(coefs[, "Estimate"]),
      conf.low.irr = exp(ci[, 1]),
      conf.high.irr = exp(ci[, 2]),
      row.names = NULL
    )
  }
  #zero-inflated
  else {
    coefs <- summary(model)$coefficients

    #split up into count part and zero part for all summary terms
    count_part <- as.data.frame(coefs$count)
    zero_part <- as.data.frame(coefs$zero)
    names(count_part)[1:4] <- c("estimate", "std.error", "statistic", "p.value")
    names(zero_part)[1:4] <- c("estimate", "std.error", "statistic", "p.value")
    count_part$component <- "count"
    zero_part$component <- "zero"
    count_part$term <- rownames(count_part)
    zero_part$term <- rownames(zero_part)

    #combine
    bind_rows(count_part, zero_part) %>%
      mutate(incidence_rate_ratio = exp(estimate)) %>%
      select(component, term, estimate, std.error, statistic, p.value, incidence_rate_ratio)
  }
}
#function that displays how good the model fit is (deviance and AIC)
count_gof_table <- function(model, model_type) {
  #no AIC, BIC, or likelihood
  if (model_type == "Quasi-Poisson") {
    tibble(
      statistic = c(
        "Residual deviance",
        "Residual df",
        "Dispersion Ratio"
      ),
      value = c(
        model$deviance,
        model$df.residual,
        sum(residuals(model, type = "pearson")^2) / model$df.residual
      )
    )
  }
  #has likelihood and AIC/BIC
  else {
    tibble(
      statistic = c(
        "Log-likelihood",
        "AIC",
        "BIC"
      ),
      value = c(
        as.numeric(logLik(model)),
        AIC(model),
        BIC(model)
      )
    )
  }
}