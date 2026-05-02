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
    removed.n = 0
  )
  
  ##############################################
  # PROCESS UPLOADED DATA
  ##############################################
  # upload_data<-reactive({
  #   req(input$file_upload)
  #   tryCatch({
  #     dat <- read.csv(input$file_upload$datapath)},
  #     error = function(e){
  #       # return a safeError if a parsing error occurs
  #       stop(safeError(e))
  #     }
  #   )
  #   globalVars$dataset <- dat %>% mutate_if(is.character,as.factor)%>%
  #     mutate_if(is.integer,as.numeric)
  #   globalVars$dataset.original <- globalVars$dataset
    
  #   updateFactorsSelectize()
  #   emptyEquation()
  # })
  
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

    updateSelectizeInput(
      session,
      "select_factors",
      choices = names(dat),
      selected = names(dat)[sapply(dat, is.factor)]
    )
  })
  
  #############################################################################################
  # When factors are selected -- update dataset
  #############################################################################################
  
  # can.be.numeric <- function(x) {
  #   stopifnot(is.atomic(x) || is.list(x)) # check if x is a vector
  #   numNAs <- sum(is.na(x))
  #   numNAs_new <- suppressWarnings(sum(is.na(as.numeric(as.character(x))))) # if already a factor as.numeric works
  #   return(numNAs_new == numNAs)
  # }
  
  observeEvent(input$select_factors,{
    req(vals$dataset)
    dat = vals$dataset

    for (var in names(dat)) {
      if (var %in% input$select_factors) {
        dat[[var]] <- as.factor(dat[[var]])
      }
    }
    
    vals$dataset = dat
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
          rename(viruses = `viruses within nucleus`)

    } else if (choice == "Ache Monkey (McMillan)") {
        dat = read.csv("www/ache_monkey.csv")

    } else if (choice == "Ache Monkey Trips") {
        dat = read.csv("www/ache_monkey_trips.csv") %>%
          mutate(Age = scale(Age))

    } else if (choice == "Niyogi 2025") {
        dat = read.csv("www/niyogi25.csv")

    } else {
        dat = NULL
    }
    dat
  }

  
  #############################################################################################
  # When sample data is loaded
  #############################################################################################
  observeEvent(input$sample, {
    # globalVars$changed.input <- TRUE
    # updateTabsetPanel(session, "workPanel", selected = "data")
    # globalVars$model <- NULL
    # hideAllTabs()
    # hideInteractionInput()
    # uncheckAllAssumptions()
    # emptyEquation()
    if (is.null(vals$sample)) {
      vals$sample <- FALSE
    } 

    vals$sample <- !vals$sample


    if(vals$sample){
      #globalVars$sample <- TRUE
      #globalVars$dataset <- NULL
      shinyjs::hide("file_upload")
      shinyjs::show("choose_sample")
      #shinyjs::show("select_factors")
      
      # if(input$sample_data_choice=="Palmer Penguins"){
      #   library(palmerpenguins)
      #   dat<-data.frame(penguins)
      # }else if(input$sample_data_choice=="Bracht et al. MFAP4" ){
      #   dat<-read.csv("www/mfap4.csv")%>%
      #     mutate(Age=as.numeric(Age))
      # }else if(input$sample_data_choice=="U.S. News College Data"){
      #   library(ISLR)
      #   dat<-College
      # }else if(input$sample_data_choice=="Cooley's Poor Beliefs Data"){
      #   dat<-read.csv("www/poorbeliefs.csv") %>% mutate(Democrat = factor(Democrat))
      # }
      # globalVars$dataset <- dat %>% mutate_if(is.character,as.factor)%>%
      #   mutate_if(is.integer,as.numeric)
      # globalVars$dataset.original <- dat %>% mutate_if(is.character,as.factor)%>%
      #   mutate_if(is.integer,as.numeric)
      
      updateActionButton(session, "sample", label = "<- Back")
      # updateFactorsSelectize()
      
    } else {
      #globalVars$sample <- FALSE
      vals$dataset <- NULL
      vals$model <- NULL
      vals$model_data <- NULL
      shinyjs::show("file_upload")
      shinyjs::hide("choose_sample")
      
      # if(!is.null(input$file_upload)){
      #   inFile <<- upload_data()
      #   shinyjs::show("select_factors")
      #   updateFactorsSelectize()
        
      # }else{
      #   shinyjs::hide("select_factors")
      #   globalVars$dataset <- NULL
      #   globalVars$dataset.original <- NULL
      #   globalVars$fcts <- NULL
      #   updateSelectizeInput(session, "select_factors",
      #                        "Specify Categorical Variables in the Data:",
      #                        choices = c(""),
      #                        selected = NULL)
      # }
      
      updateActionButton(session, "sample", label = "Sample Data")
    }
  })
  
  #############################################################################################
  # When a new sample selected
  #############################################################################################
  observeEvent(input$sample_data_choice,{
    if (input$sample_data_choice == "Select a dataset") {
      vals$dataset <- NULL
      vals$model <- NULL
      vals$model_data <- NULL
      vals$response <- NULL
      vals$removed.n <- 0
      return(NULL)
    }
    req(input$sample_data_choice)
    req(input$sample_data_choice != "Select a dataset")
    dat <- load_sample_data(input$sample_data_choice)
    req(dat)

    dat <- dat %>%
      mutate(across(where(is.character), as.factor))

    vals$dataset <- dat
    vals$model <- NULL
    vals$model_data <- NULL
 
    updateSelectizeInput(
      session,
      "select_factors",
      choices = names(dat),
      selected = names(dat)[sapply(dat, is.factor)]
    )

    shinyjs::show("select_factors")
    
    # if(globalVars$sample){
    #   if(input$sample_data_choice=="Palmer Penguins"){
    #     library(palmerpenguins)
    #     dat<-data.frame(penguins)
    #   }else if(input$sample_data_choice=="Bracht et al. MFAP4" ){
    #     dat<-read.csv("www/mfap4.csv")%>%
    #       mutate(Age=as.numeric(Age))
    #   }else if(input$sample_data_choice=="U.S. News College Data"){
    #     library(ISLR)
    #     dat<-College
    #   }else if(input$sample_data_choice=="Cooley's Poor Beliefs Data"){
    #     dat<-read.csv("www/poorbeliefs.csv") %>% mutate(Democrat = factor(Democrat))
    #   } else if(input$sample_data_choice=="Lai et al. Tree Data"){
    #     dat<-read.csv("www/LaiTreeData.csv") %>% mutate(sp = factor(sp))
    #   } else if(input$sample_data_choice=="Lai et al. Schima Superba"){
    #     dat<-read.csv("www/LaiTreeData-SS.csv") %>% mutate(sp = factor(sp))
    #   } else if(input$sample_data_choice=="Loven et al. Road Weather Data"){
    #     dat<-read.csv("www/HalikkoAsphalt.csv") %>% mutate(RoadState = factor(RoadState))
    #   }
    #   shinyjs::show("select_factors")
    #   globalVars$dataset <- dat %>% mutate_if(is.character,as.factor)%>%
    #     mutate_if(is.integer,as.numeric)
    #   globalVars$dataset.original <- globalVars$dataset
      
    #   updateFactorsSelectize()
    #   hideInteractionInput()
    #   emptyEquation()
    #   uncheckAllAssumptions()
    #   hideAllTabs()
    # }
  })
  
  ########################################
  # Check Equation Input
  ########################################
  # checkEquationValidity <- function(){
    
  #   run <- TRUE
  #     dat <- globalVars$dataset
      
  #     if (run & grepl("~", input$equation)) {
  #       subbed <- str_replace_all(input$equation,fixed("+"),"~")
  #       subbed <- str_replace_all(subbed,fixed("*"),"~")
  #       subbed <- str_replace_all(subbed,fixed(":"),"~")
  #       variables <- trimws(str_split(string = subbed, pattern = "~", simplify = T))
        
  #       if(length(variables)>=2 && variables[2]!=""){
  #         globalVars$response <- variables[1]
          
  #         if(!(substr(globalVars$response, start=0, stop=16) %in% c("ihs.transformed.", "log.transformed.", "lp1.transformed."))){
  #           globalVars$transform.type <- "none"
            
  #           globalVars$dataset <- globalVars$dataset %>%
  #             dplyr::select(-starts_with("ihs.transformed.")) %>%
  #             dplyr::select(-starts_with("log.transformed.")) %>%
  #             dplyr::select(-starts_with("lp1.transformed."))
  #         }
          
  #         globalVars$equation <- input$equation

  #         #Check for misspellings 
  #         if(!all(variables %in% colnames(globalVars$dataset))){
  #           run <- FALSE
  #           shinyalert("Error!", text = "One of the variables in your equation does not exist in the data set. Please check your equation again for spelling or other errors.", type = "error")          
  #           # check for correct response
  #         }else if(!(is.numeric(globalVars$dataset[[variables[,1]]]))){
  #           run <- FALSE
  #           shinyalert("Error!", text = "Please make sure to choose a numeric response variable.", type = "error")
  #           # check for bad categorical variables
  #         }else{
  #           # Check for too many levels on explanatory variables
  #           for (i in 2:length(variables)){
  #             if(!is.numeric(globalVars$dataset[[variables[i]]])){
  #               if (length(unique(as.character(globalVars$dataset[[variables[i]]]))) > 12){
  #                 shinyalert("Error!", text = "One of your categorical predictor variables has more than the maximum supported number of unique levels (12). Please check that the predictor is indeed categorical or adjust your regression equation.", type = "error")
  #                 run <- FALSE
  #                 break
  #               }
  #             }
  #           } 
  #         }
  #       }else{
  #         run <- FALSE
  #         shinyalert("Error!", text="Your regression equation is not correctly specified, please rewrite it.", type = "error")
  #       }
  #     }else{
  #       run <- FALSE
  #       shinyalert("Error!", text="Your regression equation is not correctly specified, please rewrite it.", type = "error")
  #     }
  #   return(run)
  #   }
  
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

    cleaned <- clean_model_data(vals$dataset, input$equation)

    vals$model_data <- cleaned$data
    vals$response <- response
    vals$removed.n <- cleaned$removed.n

    vals$model <- fit_count_model(input$equation, cleaned$data, input$model_type)

    vals$model_type <- input$model_type

    vals$comparison_table <- make_model_comparison_table(
      formula_text = input$equation,
      data = cleaned$data
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
    req(vals$model)

    DT::datatable(
      broom::tidy(vals$model),
      options = list(pageLength = 10, scrollX = TRUE)
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
  
  output$irr_table <- DT::renderDataTable({
    req(vals$model, vals$model_type)

    DT::datatable(
      tidy_count_model(vals$model, vals$model_type, input$alpha),
      options = list(pageLength = 10, scrollX = TRUE)
    )
  })

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
      return(HTML("<p>No likelihood-based models were available for AIC comparison.</p>"))
    }

    best <- cleaned.table$model[which.min(cleaned.table$AIC)]

    HTML(paste0(
      "<p>Among the models other than Quasi-Poisson, the lowest AIC is for <b>",
      best,
      "</b>. A lower AIC suggests a better model fit, but diagnostics need to be considered as well.</p>",
      "<p>Note: Quasi-Poisson was not included in the AIC comparison because it does not have a full likelihood.</p>"
    ))
  })
  
})