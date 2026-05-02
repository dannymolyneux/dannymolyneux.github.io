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

  output$count_checks <- renderUI({
    req(vals$model_data, vals$response)

    y <- vals$model_data[[vals$response]]

    HTML(paste0(
      "<p><b>Rows removed for missing model variables:</b> ", vals$removed.n, "</p>",
      "<p><b>Response is numeric:</b> ", is.numeric(y), "</p>",
      "<p><b>Response is nonnegative:</b> ", all(y >= 0), "</p>",
      "<p><b>Response uses integer counts:</b> ", all(y %% 1 == 0), "</p>",
      "<p><b>Mean:</b> ", round(mean(y), 3), "</p>",
      "<p><b>Variance:</b> ", round(var(y), 3), "</p>"
    ))
  })

  output$dispersion_results <- renderUI({
    req(vals$model)

    result <- check_overdispersion(vals$model)

    HTML(paste0(
      "<p><b>Dispersion ratio:</b> ", result$value, "</p>",
      "<p>", result$interpretation, "</p>"
    ))
  })

  output$model_summary <- DT::renderDataTable({
    req(vals$model)

    DT::datatable(
      broom::tidy(vals$model),
      options = list(pageLength = 10, scrollX = TRUE)
    )
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
  
})