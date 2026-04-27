library(dplyr)
library(tibble)

fit_poisson_model <- function(formula_text, data) {
  glm(
    formula = as.formula(formula_text),
    data = data,
    family = poisson(link = "log")
  )
}