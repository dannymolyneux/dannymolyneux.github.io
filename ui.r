# #################################################################################################
# #################################################################################################
# #################################################################################################
# #################################################################################################
#' Count Regression Application
#' @author Danny Molyneux
#' @description UI code for the Count Regression Toolkit
# #################################################################################################
# #################################################################################################
# #################################################################################################
# #################################################################################################
library(Hmisc)      #correlations and tests (still here, here due to overriding summarize)

##########################################
# Shiny
##########################################
library(shiny)
library(shinyalert)
library(shinyjs)
library(shinymeta)
library(shinythemes)
library(shinycssloaders)
library(shinyAce)
library(shinyBS)
library(DT)
##########################################
# Data
##########################################
library(palmerpenguins)
library(ISLR)
##########################################
# General
##########################################
library(tidyverse) #ggplot2, tibble, tidyr, reader, 
#purrr, dplyr, stringr, forcats
library(magrittr)
library(patchwork) #combining plots
library(xtable) #LaTex Plots

##########################################
# Regression
##########################################
library(GGally)      #pairwise plots
library(gtools)      #pvales for cormat
#library(Hmisc)      #correlations and tests (still here, not loaded due to overriding summarize)
library(lmtest)      #constant error variance
library(margins)     #for marginal effects
library(multcomp)    #glht
library(interactions)#for johnson neyman
library(ggeffects)   #for marginal effects plots
library(emmeans)     #for estimated marginal means
library(car)         #for anova
#library(crayon)      #for removing style from Johnson Neyman output
library(effectsize)
#library(ggforce)
#--------------------------------------------
#---------------     UI    ------------------
#--------------------------------------------
ui<-tagList(tags$head(tags$link(rel = "icon",  type = "image/x-icon", 
                                href = "www/favicon.png"), 
                      tags$style(HTML(".paragraph {margin:auto;max-width: 50%;font-size: 15px; text-align:justify;}
                                        h1 {text-align:center;}")),
                      tags$style(HTML("div.MathJax_Display{text-align: left !important;}"))),
            
            # action buttons smaller
            tags$style(HTML(".btn {padding:5px; font-size:12px;}")),
            # checkbox label text smaller
            tags$style(HTML(".checkbox {font-size:12px; margin:5px;}")),
            # input label text smaller
            tags$style(HTML(".control-label {font-size:14px;}")),
            # text input smaller
            tags$style(HTML(".form-control {height:auto; padding:5px;}")),
            tags$style(HTML(".shiny-input-text {font-size:12px;}")),
            tags$style(HTML(".shiny-input-number {font-size:12px;}")),
            # assumption text smaller
            #tags$style(HTML(".shiny-bound-output {font-size:13px;}")), # this changes all UIOutput()
            tags$style(HTML("#asmp_1note {font-size:12px;}")),
            tags$style(HTML("#asmp_2note {font-size:12px;}")),
            tags$style(HTML("#asmp_3note {font-size:12px;}")),
            tags$style(HTML("#asmp_4note {font-size:12px;}")),
            tags$style(HTML("#check_note {font-size:12px;}")),
            tags$style(HTML("#asmp_note {font-size:12px;}")),
            # dropdowns smaller
            tags$style(HTML(".item {height:auto;}")),
            tags$style(HTML(".selectize-input, .selectize-dropdown {height:auto; padding:5px; font-size: 12px;}")),
            # reduce space between inputs
            tags$style(HTML(".form-group {margin-bottom: 8px; }")),
            tags$style(HTML(".help-text {font-size: 12px; color: #555;}")),
            
            useShinyjs(), #useShinyalert(), withMathJax(),  # load functions from packages
  navbarPage(title="Count Regression Toolkit", id = "tabs",theme = shinytheme("flatly"),
             #Dataset and Model Summary Panel
             tabPanel("Dataset & Model",
                      sidebarPanel(
                        fileInput("file_upload", "Upload CSV File", accept=c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
                        actionButton("sample", "Use Sample Data"),
                        hidden(div(id='choose_sample',
                                   selectInput("sample_data_choice","Sample Data:",
                                               choices = c("Select a sample dataset",
                                                           "Dataset 1",
                                                           "Dataset 2"),
                                               selected = "Select a sample dataset"))),
                        tags$hr(),
                        tags$a(href = "https://dannymolyneux.github.io",target = "_blank",
                               class = "btn btn-primary",
                               "Back to Website"),
                        tags$style(type="text/css", ".selectize-input{overflow: auto;}"), # this fixes overflow in selectize
                        hidden(selectizeInput("select_factors",
                                       "Specify Categorical Variables",
                                       choices = NULL,
                                       selected = NULL,
                                       multiple = TRUE)),
                        
                        textInput("equation", "Enter your desired regression equation:", placeholder = "count_response ~ x1 + x2 + x3"),
                        #bsTooltip("equation", "Example: response ~ explanatory_1 + explanatory_2 + ... + explanatory_k",
                                  #"right", trigger = "hover", options = list(container = "body")),
                        
                        #checkboxInput("scalevars", "Scale all variables (standardize)", FALSE),
                        selectInput("model_type",
                                    "Choose count regression model:",
                                    choices = c("Poisson","Quasi-Poisson","Negative Binomial","Zero-Inflated Poisson",
                                                "Zero-Inflated Negative Binomial","Generalized Poisson"),
                                         selected = "Poisson"),
                        
                        
                        numericInput("alpha", "Significance level (\u03B1): ", value = 0.05, step = 0.001, min = 0, max = 1),     # alpha level
                        checkboxInput("remove_missing","Remove rows with missing values used in the model",value = TRUE),
                        div(class = "text-center", actionButton("DoCompute", "Fit Model")),
                        tags$hr(),
                        div(class="help-text",p("Model suggestions will be based on overdispersion, zero inflation, and count-response checks."))
                      ),
                      #visualize dataset and display dataset
                      mainPanel(
                        tabsetPanel(id="workPanel",
                                    tabPanel("Data Preview", br(), value="data",
                                             shinycssloaders::withSpinner(DT::dataTableOutput("preview_data"))),
                                    tabPanel("Data Summary",value = "summary", br(),
                                            h3("Response Summary"),
                                            shinycssloaders::withSpinner(DT::dataTableOutput("response_summary")),
                                            br(),
                                            h3("Variable Summary"),
                                            shinycssloaders::withSpinner(DT::dataTableOutput("variable_summary")),
                                            br(),
                                            h3("Count Distribution"),
                                            shinycssloaders::withSpinner(plotOutput("count_distribution"))
                                            ),
                                    tabpanel("Poisson Checks", value = "checks", br(),
                                            h3("Count Regression Conditions"),
                                            htmlOutput("count_checks"),
                                            br(),
                                            h3("Overdispersion"),
                                            htmlOutput("dispersion_results"),
                                            br(),
                                            h3("Zero Inflation"),
                                            htmlOutput("zero_results"),
                                            br(),
                                            h3("Suggested Model"),
                                            htmlOutput("model_suggestion"))
                          )
                        )
             ),
  )
)