#Function for fitting model based on selection
fit_count_model <- function(formula, data, model_type) {
  if (model_type == "Poisson") {
    glm(formula, data = data, family = poisson(link = "log"))
  } else if (model_type == "Quasi-Poisson") {
    glm(formula, data = data, family = quasipoisson(link = "log"))
  } else if (model_type == "Negative Binomial") {
    MASS::glm.nb(formula, data = data)
  } else if (model_type == "Zero-Inflated Poisson") {
    pscl::zeroinfl(formula, data = data, dist = "poisson")
  } else if (model_type == "Zero-Inflated Negative Binomial") {
    pscl::zeroinfl(formula, data = data, dist = "negbin")
  } else if (model_type == "Generalized Poisson") {
    VGAM::vglm(formula, data = data, family = VGAM::genpoisson())
  }
}

#zero inflation ouptut for server.r
output$zero_results <- renderUI({
  req(values$model)

  result <- check_zero_inflation_dharma(values$model)

  HTML(paste0(
    "<p><b>p-value:</b> ", round(result$p.value, 4), "</p>",
    "<p>", result$interpretation, "</p>"
  ))
})