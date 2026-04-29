library(dplyr)
library(tibble)
library(ggplot2)
library(DHARMa)

#calculates dispersion ratio
dispersion_ratio <- function(model) {
  sum(residuals(model, type = "pearson")^2) / model$df.residual
}

#function to interpret dispersion ratio
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

#function to text zero-inlfation using DHARMa package
check_zero_inflation_dharma <- function(model) {
  test <- DHARMa::testZeroInflation(model)

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
make_poisson_conditions_plot <- function(model) {
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