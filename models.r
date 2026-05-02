library(dplyr)
library(tibble)
library(MASS)
library(pscl)
library(emmeans)

#fits the model based on user selection
fit_count_model <- function(formula_text, data, model_type, offset_var = NULL) {
  form = add_offset_to_formula(formula_text, offset_var)
  if (model_type == "Poisson") {
    glm(form, data = data, family = poisson(link = "log"))

  } else if (model_type == "Quasi-Poisson") {
    glm(form, data = data, family = quasipoisson(link = "log"))

  } else if (model_type == "Negative Binomial") {
    MASS::glm.nb(form, data = data)

  } else if (model_type == "Zero-Inflated Poisson") {
    pscl::zeroinfl(form, data = data, dist = "poisson")

  } else {
    stop("Unsupported model type.")
  }
}


#function that displays the summary of the model (including percent change)
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
      percent_change = round((exp(coefs[, "Estimate"]) - 1)*100, 2),
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
    out <- dplyr::bind_rows(count_part, zero_part)

    out$percent_change <- round((exp(out$estimate) - 1) * 100, 2)

    return(out)
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

#function that tries to run each model, in order to run a comparison
fit_candidate_models <- function(formula_text, data, offset_var = NULL) {
  form <- add_offset_to_formula(formula_text, offset_var)

  models <- list()
   #tryCatch() syntax help from AI
  models$Poisson <- tryCatch(
    glm(form, data = data, family = poisson(link = "log")),
    error = function(e) NULL
  )

  models$`Quasi-Poisson` <- tryCatch(
    glm(form, data = data, family = quasipoisson(link = "log")),
    error = function(e) NULL
  )

  models$`Negative Binomial` <- tryCatch(
    MASS::glm.nb(form, data = data),
    error = function(e) NULL
  )

  models$`Zero-Inflated Poisson` <- tryCatch(
    pscl::zeroinfl(form, data = data, dist = "poisson"),
    error = function(e) NULL
  )

  models$`Zero-Inflated Negative Binomial` <- tryCatch(
    pscl::zeroinfl(form, data = data, dist = "negbin"),
    error = function(e) NULL
  )

  models
}

#function that runs the model comparison
make_model_comparison_table <- function(formula_text, data, offset_var = NULL) {
  models <- fit_candidate_models(formula_text, data, offset_var)

  rows <- lapply(names(models), function(model_name) {
    mod <- models[[model_name]]

    if (is.null(mod)) {
      return(tibble::tibble(
        model = model_name,
        logLik = NA_real_,
        AIC = NA_real_,
        BIC = NA_real_,
        dispersion_ratio = NA_real_,
        note = "Model failed to fit."
      ))
    }

    is_quasi <- model_name == "Quasi-Poisson"

    disp <- tryCatch(
      sum(residuals(mod, type = "pearson")^2) / df.residual(mod),
      error = function(e) NA_real_
    )

    tibble::tibble(
      model = model_name,
      logLik = ifelse(is_quasi, NA_real_, as.numeric(logLik(mod))),
      AIC = ifelse(is_quasi, NA_real_, AIC(mod)),
      BIC = ifelse(is_quasi, NA_real_, BIC(mod)),
      dispersion_ratio = disp,
      note = dplyr::case_when(
        is_quasi ~ "No AIC/logLik for quasi models.",
        TRUE ~ "Fit successfully."
      )
    )
  })

  dplyr::bind_rows(rows) %>%
    dplyr::mutate(
      AIC = round(AIC, 3),
      BIC = round(BIC, 3),
      logLik = round(logLik, 3),
      dispersion_ratio = round(dispersion_ratio, 3)
    )
}

#allows for user to choose an offset 
add_offset_to_formula <- function(formula_text, offset_var = NULL) {
  if (is.null(offset_var) || offset_var == "None") {
    return(as.formula(formula_text))
  }


  as.formula(
    paste0(formula_text, " + offset(log(", offset_var, "))")
  )
}

#Estimated marginal means plot for selected predictor
make_emmeans_plot <- function(model, data, formula_text, predictor) {
  
  response <- all.vars(as.formula(formula_text))[1]
  predictors <- all.vars(as.formula(formula_text))[-1]
  
  if (!(predictor %in% predictors)) {
    stop("Selected predictor is not in the model.")
  }
  
  pred_values <- data[[predictor]]
  
  if (!is.numeric(pred_values)) {
    
    em <- emmeans::emmeans(
      model,
      specs = as.formula(paste("~", predictor)),
      type = "response"
    )
    
    em_df <- as.data.frame(em)
    
    ggplot2::ggplot(em_df, ggplot2::aes(x = .data[[predictor]], y = response)) +
      ggplot2::geom_point(size = 3) +
      ggplot2::geom_errorbar(
        ggplot2::aes(ymin = asymp.LCL, ymax = asymp.UCL),
        width = 0.15
      ) +
      ggplot2::labs(
        x = predictor,
        y = paste("Estimated Marginal Mean"),
        title = paste("Estimated Marginal Means Across", predictor)
      ) +
      ggplot2::theme_bw()
    
  } else {
    
    at_list <- list()
    
    at_list[[predictor]] <- seq(
      min(pred_values, na.rm = TRUE),
      max(pred_values, na.rm = TRUE),
      length.out = 100
    )
    
    other_predictors <- setdiff(predictors, predictor)
    
    for (v in other_predictors) {
      if (is.numeric(data[[v]])) {
        at_list[[v]] <- mean(data[[v]], na.rm = TRUE)
      }
    }
    
    em <- emmeans::emmeans(
      model,
      specs = as.formula(paste("~", predictor)),
      at = at_list,
      type = "response"
    )
    
    em_df <- as.data.frame(em)
    
    lower_col <- intersect(c("asymp.LCL", "lower.CL"), names(em_df))[1]
    upper_col <- intersect(c("asymp.UCL", "upper.CL"), names(em_df))[1]
    
    ggplot2::ggplot(em_df, ggplot2::aes(x = .data[[predictor]], y = response)) +
      ggplot2::geom_line(linewidth = 1) +
      ggplot2::geom_ribbon(
        ggplot2::aes(
          ymin = .data[[lower_col]],
          ymax = .data[[upper_col]]
        ),
        alpha = 0.2
      ) +
      ggplot2::labs(
        x = predictor,
        y = paste("Estimated expected", response),
        title = paste("Estimated Mean", response, "Across", predictor)
      ) +
      ggplot2::theme_bw()
  }
}