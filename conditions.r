library(dplyr)
library(tibble)
library(ggplot2)
library(DHARMa)

#calculates dispersion ratio
dispersion_ratio <- function(model) {
  sum(residuals(model, type = "pearson")^2) / model$df.residual
}

#table for all condition checks in the conditions tab
make_condition_table <- function(data, response, model, model_type, removed.n = 0) {
  y <- data[[response]]

  mean_y <- mean(y, na.rm = TRUE)
  var_y <- var(y, na.rm = TRUE)
  disp <- dispersion_ratio(model)

  yn <- function(x) ifelse(isTRUE(x), "Yes", "No")

  tibble::tibble(
    condition = c(
      "Rows removed for missing model variables",
      "Response is numeric",
      "Response is nonnegative",
      "Response uses integer counts",
      "Mean of response",
      "Variance of response",
      "Dispersion ratio"
    ),
    result = c(
      removed.n,
      yn(is.numeric(y)),
      yn(all(y >= 0, na.rm = TRUE)),
      yn(all(y %% 1 == 0, na.rm = TRUE)),
      round(mean_y, 3),
      round(var_y, 3),
      round(disp, 3)
    ),
    interpretation = c(
      ifelse(removed.n == 0, "No rows removed.", paste(removed.n, "rows were removed.")),
      "Count responses should be numeric.",
      "Counts should not be negative.",
      "Counts should be whole numbers.",
      "Used to compare mean and variance.",
      "For Poisson, variance should be close to the mean.",
      ifelse(
        model_type == "Poisson",
        ifelse(disp < 1.5, "Poisson dispersion looks acceptable.", "Poisson may be overdispersed."),
        ifelse(model_type == "Negative Binomial", "NB is designed to handle overdispersion.", "Interpret based on selected model.")
      )
    )
  )
}


#function to test zero-inlfation using DHARMa package, outputting the p-value
check_zero_inflation_dharma <- function(model) {
  sim_res <- DHARMa::simulateResiduals(model)
  test <- DHARMa::testZeroInflation(sim_res)

  data.frame(
    check = "Zero inflation",
    statistic = unname(test$statistic),
    p.value = test$p.value,
    interpretation = ifelse(
      test$p.value < 0.05,
      "Evidence of zero inflation. Consider a zero-inflated model.",
      "No strong evidence of zero inflation."
    )
  )
}

#Makes RQR plot and qq plot with dispersion ratio
make_conditions_plot <- function(model) {
  counts <- model$y
  lambdas <- fitted(model)
  rqr <- rep(NA, length(lambdas))
  for(i in 1:length(lambdas)){
    ai <- ppois(counts[i]-1, lambda=lambdas[i])
    bi <- ppois(counts[i], lambda=lambdas[i])
    # this works even when ai=bi
    ui <- ai + runif(1) * (bi - ai)
    ui <- max(min(ui, 1-10^(-6)), 10^(-6))
    rqr[i] <- qnorm(ui)
  }
  pearson.ratio <- sum(residuals(model, type = "pearson")^2) / model$df.residual
  p1 <- ggplot(data=tibble(lambda=lambdas,
                          e=rqr)) + 
               geom_hline(yintercept=0, linetype="dotted")+
               geom_point(aes(x=lambda, y=e)) +
               theme_bw()+
               xlab(bquote(lambda))+
               ylab("Randomized Quantile Residuals")
  p2 <- ggplot(data=tibble(e=rqr)) +
               stat_qq(aes(sample=e)) +
               stat_qq_line(aes(sample=e)) +
               theme_bw() +
               ggtitle(paste("Dispersion Ratio =", round(pearson.ratio, 4)))
  p1+p2
}

#plots fitted vs pearson residual squared
make_pearson_squared_plot <- function(model) {
  lambdas <- fitted(model, type="response")

  ggdat <- tibble(r = resid(model, type="pearson")^2,
                lambda=lambdas)

  pearson.ratio <- sum(residuals(model, type = "pearson")^2) / df.residual(model)

  plot = ggplot(ggdat) + 
    geom_point(aes(x=lambda, y=r)) + 
    geom_smooth(aes(x=lambda, y=r)) +
    theme_bw()+
    labs(x=bquote(lambda),
        y="Squared Pearson Residual")+
    ggtitle(paste("Dispersion Ratio: ", round(pearson.ratio,4))) 
  plot
}