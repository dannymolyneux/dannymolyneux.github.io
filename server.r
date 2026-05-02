# #################################################################################################
# #################################################################################################
# #################################################################################################
# #################################################################################################
#' Count Regression Application
#' Danny Molyneux
#' @description Server code for the Count Regression app
# #################################################################################################
# #################################################################################################
# #################################################################################################
# #################################################################################################

library(dplyr)
library(shiny)
library(DT)
library(ggplot2)
library(tibble)
library(MASS)
library(pscl)
library(performance)
library(car)
library(emmeans)


source("helpers.R")
source("models.R")
source("conditions.R")

server <- (function(input, output, session){
  #############################################################################################
  # DATA OBJECTS
  #############################################################################################
  
  #initialize data and model objects
  vals <- reactiveValues(
    dataset = NULL,
    model_data = NULL,
    model = NULL,
    model_type = NULL,
    response = NULL,
    comparison_table = NULL,
    removed.n = 0,
    sample = FALSE
  )
  
  ##############################################
  # DATASET PREVIEW
  ##############################################
  output$preview_data <- DT::renderDataTable({
    req(vals$dataset)
    DT::datatable(vals$dataset, options = list(scrollX = TRUE))
  }, server = FALSE)
  
  #############################################################################################
  # When data is uploaded
  #############################################################################################
  observeEvent(input$file_upload,{
    req(input$file_upload)
    dat <- read.csv(input$file_upload$datapath)
    dat <- dat %>%
      mutate(across(where(is.character), as.factor))

    vals$dataset <- dat
    vals$model <- NULL
    vals$model_data <- NULL
    vals$response <- NULL
    vals$model_type <- NULL
    vals$removed.n <- 0

    updateSelectizeInput(
      session,
      "select_factors",
      choices = names(dat),
      selected = names(dat)[sapply(dat, is.factor)]
    )

    updateSelectInput(
      session,
      "offset_var",
      choices = c("None", names(dat)),
      selected = "None"
    )

  updateTextInput(session, "equation", value = "")
  })
  
  
  observeEvent(input$select_factors,{
    req(vals$dataset)
    dat = vals$dataset

    for (var in names(dat)) {
      if (var %in% input$select_factors) {
        dat[[var]] <- as.factor(dat[[var]])
      }
      else {
        if (is.factor(dat[[var]]) && suppressWarnings(!any(is.na(as.numeric(as.character(dat[[var]])))))) {
          dat[[var]] <- as.numeric(as.character(dat[[var]]))
        }
      }
    }
    
    vals$dataset = dat
    vals$model = NULL
    vals$model_data = NULL
    vals$response = NULL
  })
  
 
  load_sample_data <- function(choice) {
    if (choice == "Brockmann 1996") {
      dat = read.csv("www/brockmann96.csv") %>%
        mutate(
          color = factor(color),
          spine = factor(spine)
        )

    } else if (choice == "Kitsberg 2025") {
        dat = read.csv("www/kitsberg25_nucleus.csv") %>%
          rename("viruses" = "viruses.within.nucleus")

    } else if (choice == "Ache Monkey (McMillan)") {
        dat = read.csv("www/ache_monkey.csv")

    } else if (choice == "Ache Monkey Trips") {
        dat = read.csv("www/ache_monkey_trips.csv") %>%
          mutate(Age = scale(Age))

    } else if (choice == "Niyogi 2025") {
        dat = read.csv("www/niyogi25.csv")

    } else if (choice == "Bad Data: Missing Values") {
        dat <- read.csv("www/bad_missing.csv")
    } else {
        dat = NULL
    }
    dat
  }

  
  #############################################################################################
  # When sample data is loaded
  #############################################################################################
  observeEvent(input$sample, {
    if (input$sample %% 2 == 1) { #tracks how many times it has been pressed (help from AI)

    shinyjs::hide("file_upload")
    shinyjs::show("choose_sample")
    updateActionButton(session, "sample", label = "<- Back")

  } else {

    vals$dataset <- NULL
    vals$model <- NULL
    vals$model_data <- NULL
    vals$response <- NULL
    vals$model_type <- NULL
    vals$comparison_table <- NULL
    vals$removed.n <- 0

    shinyjs::show("file_upload")
    shinyjs::hide("choose_sample")

    updateSelectizeInput(session, "select_factors",
                         choices = character(0),
                         selected = character(0))

    updateSelectInput(session, "offset_var",
                      choices = c("None"),
                      selected = "None")

    updateTextInput(session, "equation", value = "")

    updateActionButton(session, "sample", label = "Use Sample Data")
  }

  })
  
  #############################################################################################
  # When a new sample selected
  #############################################################################################
  observeEvent(input$sample_data_choice,{
    req(input$sample_data_choice)
    if (input$sample_data_choice == "Select a sample dataset") {
      vals$dataset <- NULL
      vals$model <- NULL
      vals$model_data <- NULL
      vals$model_type <- NULL
      vals$response <- NULL
      vals$removed.n <- 0

      updateSelectizeInput(
        session,
        "select_factors",
        choices = NULL,
        selected = NULL
      )

      updateSelectInput(
      session,
      "offset_var",
      choices = "None",
      selected = "None"
    )
      
      updateTextInput(session, "equation", value = "")
      
      return(NULL)
   }
    req(input$sample_data_choice)
    req(input$sample_data_choice != "Select a sample dataset")
    dat <- load_sample_data(input$sample_data_choice)
    req(dat)

    dat <- dat %>%
      mutate(across(where(is.character), as.factor))

    vals$dataset <- dat

    updateSelectizeInput(
    session,
    "select_factors",
    choices = names(dat),
    selected = names(dat)[sapply(dat, is.factor)]
  )

  # --- Update offset variable choices ---
  updateSelectInput(
    session,
    "offset_var",
    choices = c("None", names(dat)),
    selected = "None"
  )
   shinyjs::show("select_factors")
  })
   
  
  #############################################################################################
  # When a new sample selected
  #############################################################################################
  observeEvent(input$DoCompute,{
    req(vals$dataset)
    req(input$equation)
    
    response <- get_response_name(input$equation)


    if (!response %in% names(vals$dataset)) {
      showNotification("Response variable not found in dataset.", type = "error")
      return(NULL)
    }

    if (!is_count_response(vals$dataset[[response]])) {
      showNotification("Response must be a nonnegative integer count variable.", type = "error")
      return(NULL)
    }

    cleaned <- clean_model_data(vals$dataset, input$equation, offset_var = input$offset_var)

    vals$model_data <- cleaned$data
    vals$response <- response
    vals$removed.n <- cleaned$removed.n

    vals$model <- fit_count_model(input$equation, cleaned$data, input$model_type, offset_var = NULL)

    vals$model_type <- input$model_type

    vals$comparison_table <- make_model_comparison_table(
      formula_text = input$equation,
      data = cleaned$data,
      offset_var = input$offset_var
    )

    predictors <- all.vars(as.formula(input$equation))[-1]

    updateSelectInput(
      session,
      "emmeans_predictor",
      choices = c("None", predictors),
      selected = ifelse(length(predictors) > 0, predictors[1], "None")
    )
    updateTabsetPanel(session, "workPanel", selected = "Data Summary")
    
    
  })
  

  output$response_summary <- DT::renderDataTable({
    req(vals$model_data, vals$response)

    DT::datatable(
      summarize_response(vals$model_data, vals$response),
      options = list(dom = "t")
    )
  })

  output$count_distribution <- renderPlot({
    req(vals$model_data, vals$response)

    ggplot(vals$model_data, aes(x = .data[[vals$response]])) +
      geom_bar() +
      labs(
        x = vals$response,
        y = "Frequency",
        title = "Distribution of Count Response"
      ) +
      theme_minimal()
  })

  output$condition_table <- DT::renderDataTable({
    req(vals$model_data, vals$response, vals$model, vals$model_type)

    DT::datatable(
    make_condition_table(
      data = vals$model_data,
      response = vals$response,
      model = vals$model,
      model_type = vals$model_type,
      removed.n = vals$removed.n
    ),
    options = list(dom = "t", scrollX = TRUE)
  )
  })

  output$dispersion_results <- renderUI({
    req(vals$model, vals$model_type)

    ratio <- dispersion_ratio(vals$model)

    message = if(vals$model_type == "Poisson") {
      if (ratio < 1.5) {
        "For a Poisson model, this dispersion ratio looks reasonably close to 1."
    } else {
        "For a Poisson model, this suggests overdispersion. Negative Binomial or Quasi-Poisson may be more appropriate."
    }
      
    } else if (vals$model_type == "Quasi-Poisson") {
        "Quasi-Poisson allows the variance to differ from the mean because it estimates a dispersion parameter. This is helpful when Poisson is overdispersed."

    } else if(vals$model_type == "Negative Binomial") {
        "Negative Binomial regression is designed for overdispersed count data, so a high dispersion ratio is less concerning than it would be for Poisson."

    } else if (vals$model_type %in% c("Zero-Inflated Poisson", "Zero-Inflated Negative Binomial")) {
        "Zero-inflated models address excess zeros by separately modeling zeros from the count process."

    } else {
        "Dispersion interpretation depends on the selected model."
    }

    HTML(paste0(
    "<p><b>Dispersion ratio:</b> ", round(ratio, 3), "</p>",
    "<p>", message, "</p>"
     ))
  })

  output$model_summary <- DT::renderDataTable({
    req(vals$model, vals$model_type)

    DT::datatable(
      tidy_count_model(vals$model, vals$model_type, input$alpha),
    options = list(scrollX = TRUE)
    )
  })
  
  output$zero_inflation_results <- renderUI({
    req(vals$model)

    #makes sure the zero inflation check runs with no error
    result <- tryCatch(
      check_zero_inflation_dharma(vals$model),
      error = function(e) NULL
    )

    if (is.null(result)) {
      return(HTML("<p>Zero-inflation test could not be computed for this model.</p>"))
    }

    HTML(paste0(
      "<p><b>DHARMa zero-inflation p-value:</b> ", round(result$p.value, 4), "</p>",
      "<p>", result$interpretation, "</p>"
    ))
  })
  
  # output$irr_table <- DT::renderDataTable({
  #   req(vals$model, vals$model_type)

  #   DT::datatable(
  #     tidy_count_model(vals$model, vals$model_type, input$alpha),
  #     options = list(pageLength = 10, scrollX = TRUE)
  #   )
  # })

  output$condition_plots <- renderPlot({
    req(vals$model)
    make_conditions_plot(vals$model)
  })

  output$pearson_squared_plot <- renderPlot({
    req(vals$model)
    make_pearson_squared_plot(vals$model)
  })

  output$gof_table <- DT::renderDataTable({
    req(vals$model, vals$model_type)

    DT::datatable(
      count_gof_table(vals$model, vals$model_type),
      options = list(dom = "t")
    )
  })

  output$model_comparison <- DT::renderDataTable({
  req(vals$comparison_table)

  DT::datatable(
    vals$comparison_table,
    options = list(pageLength = 10, scrollX = TRUE)
  )
  })

  output$comparison_recommendation <- renderUI({
    req(vals$comparison_table)

    table <- vals$comparison_table

    cleaned.table <- table %>%
      dplyr::filter(!is.na(AIC))

    if (nrow(cleaned.table) == 0) {
      return(HTML("<p>No models were available for AIC comparison.</p>"))
    }

    best <- cleaned.table$model[which.min(cleaned.table$AIC)]

    HTML(paste0(
      "<p>Among the models other than Quasi-Poisson, the lowest AIC is for <b>",
      best,
      "</b>. A lower AIC suggests a better model fit, but diagnostics and dispersion need to be considered as well.</p>",
      "<p>Note: Quasi-Poisson was not included in the AIC comparison because it does not have a full likelihood.</p>"
    ))
  })

  output$model_interpretation <- renderUI({
    req(vals$model, vals$model_type)

    tab <- tidy_count_model(vals$model, vals$model_type, input$alpha)

    # remove intercept
    tab <- tab %>% dplyr::filter(term != "(Intercept)")

    if (nrow(tab) == 0) {
      return(HTML("<p>No predictors in the model.</p>"))
    }
    if(vals$model_type %in% c("Poisson", "Quasi-Poisson", "Negative Binomial")){
      text <- paste(
      apply(tab, 1, function(row) {

        pct <- as.numeric(row[["percent_change"]])

        direction <- ifelse(pct >= 0, "increase", "decrease")

        paste0(
          "<p>Holding other variables constant, a one-unit increase in <b>",
          row[["term"]],
          "</b> is associated with a <b>",
          abs(pct),
          "% ",
          direction,
          "</b> in the expected count.</p>"
        )
      }),
      collapse = ""
    )
    }
    #interpretation for zero-inflated model
    else {
      count_tab <- tab %>% dplyr::filter(component == "count")
      zero_tab  <- tab %>% dplyr::filter(component == "zero")

      count_text <- paste(
        apply(count_tab, 1, function(row) {
          pct <- as.numeric(row[["percent_change"]])
          direction <- ifelse(pct >= 0, "increase", "decrease")

          paste0(
            "<p><b>Count model:</b> Holding other variables constant, a one-unit increase in <b>",
            row[["term"]],
            "</b> is associated with a <b>",
            abs(pct),
            "% ",
            direction,
            "</b> in the expected count among observations in the count-generating process.</p>"
          )
        }),
        collapse = ""
      )
      #interpretation for zero part
      zero_text <- paste(
        apply(zero_tab, 1, function(row) {
          pct <- as.numeric(row[["percent_change"]])
          direction <- ifelse(pct >= 0, "increase", "decrease")

          paste0(
            "<p><b>Zero-inflation model:</b> Holding other variables constant, a one-unit increase in <b>",
            row[["term"]],
            "</b> is associated with a <b>",
            abs(pct),
            "% ",
            direction,
            "</b> in the odds of being in the structural-zero group.</p>"
          )
        }),
        collapse = ""
      )

      text <- paste0(
        "<p>Zero-inflated models have two parts. The <b>count model</b> explains expected counts, while the <b>zero-inflation model</b> explains the probability of belonging to the structural-zero group.</p>",
        count_text,
        zero_text
      )
      }
      

    HTML(text)
  })
  
  output$emmeans_plot <- renderPlot({
  req(vals$model, vals$model_data, vals$response)
  req(input$emmeans_predictor)
  req(input$emmeans_predictor != "None")
  
  if (vals$model_type %in% c(
    "Zero-Inflated Poisson",
    "Zero-Inflated Negative Binomial",
    "Generalized Poisson"
  )) {
    plot.new()
    text(
      0.5, 0.5,
      "Estimated mean plots are currently available for Poisson, Quasi-Poisson, and Negative Binomial models."
    )
    return()
  }
  #tryCatch syntax help from AI (makes sure emmeans plot can run)
  tryCatch(
    {
      make_emmeans_plot(
        model = vals$model,
        data = vals$model_data,
        formula_text = input$equation,
        predictor = input$emmeans_predictor
      )
    },
    error = function(e) {
      plot.new()
      text(
        0.5, 0.5,
        paste("Estimated mean plot could not be created:", e$message)
      )
    }
  )
  })

  output$emmeans_plot <- renderPlot({
    req(vals$model, vals$model_data, vals$response)
    req(input$emmeans_predictor)
    req(input$emmeans_predictor != "None")
    
    if (vals$model_type %in% c(
      "Zero-Inflated Poisson",
      "Zero-Inflated Negative Binomial",
      "Generalized Poisson"
    )) {
      plot.new()
      text(
        0.5, 0.5,
        "Estimated mean plots are currently available for Poisson, Quasi-Poisson, and Negative Binomial models."
      )
      return()
    }
    
    tryCatch(
      {
        make_emmeans_plot(
          model = vals$model,
          data = vals$model_data,
          formula_text = input$equation,
          predictor = input$emmeans_predictor
        )
      },
      error = function(e) {
        plot.new()
        text(
          0.5, 0.5,
          paste("Estimated mean plot could not be created:", e$message)
        )
      }
    )
  })
  
})