library(dplyr)
library(tibble)

#function checking if response is a count variable
is_count_response <- function(x) {
  is.numeric(x) &&
    all(!is.nan(x)) &&
    all(x >= 0, na.rm = TRUE) &&
    all(x %% 1 == 0, na.rm = TRUE)
}

get_response_name <- function(formula_text) {
  trimws(strsplit(formula_text, "~")[[1]][1])
}

#function cleans data and stores those changes
clean_model_data <- function(data, formula_text) {
  vars <- all.vars(as.formula(formula_text))

  before.n <- nrow(data)

  cleaned <- data %>%
    dplyr::select(all_of(vars)) %>%
    tidyr::drop_na()

  after.n <- nrow(cleaned)

  list(
    data = cleaned,
    removed.n = before.n - after.n,
    kept.n = after.n,
    vars = vars
  )
}

#function to summarize response variable
summarize_response <- function(data, response) {
  y <- data[[response]]

  tibble(
    response = response,
    n = length(y),
    missing = sum(is.na(y)),
    min = min(y, na.rm = TRUE),
    mean = mean(y, na.rm = TRUE),
    variance = var(y, na.rm = TRUE),
    max = max(y, na.rm = TRUE),
    zeros = sum(y == 0, na.rm = TRUE),
    zero_percent = round(mean(y == 0, na.rm = TRUE) * 100, 2),
    is_count = is_count_response(y)
  )
}