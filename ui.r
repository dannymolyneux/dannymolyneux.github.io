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
library(Hmisc)     

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
#library(GGally)      #pairwise plots
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
ui <- tagList(
  useShinyjs(),

  tags$head(
    tags$style(HTML("
      .btn {padding: 6px 10px; font-size: 13px;}
      .control-label {font-size: 14px;}
      .form-control {height: auto; padding: 6px;}
      .form-group {margin-bottom: 10px;}
      .selectize-input, .selectize-dropdown {font-size: 13px;}
      h3 {margin-top: 20px;}
    "))
  ),

  navbarPage(
    title = "Count Regression Toolkit",
    theme = shinytheme("flatly"),

    tabPanel(
      "App",

      sidebarLayout(
        sidebarPanel(
          fileInput(
            "file_upload",
            "Upload CSV File",
            accept = c(".csv")
          ),

          actionButton("sample", "Use Sample Data"),
          hidden(
            div(id = "choose_sample",
                selectInput("sample_data_choice","Use Sample Data:",
                            choices = c("Select a sample dataset",
                                        "Brockmann 1996",
                                        "Kitsberg 2025",
                                        "Ache Monkey (McMillan)",
                                        "Ache Monkey Trips",
                                        "Niyogi 2025",
                                        "Bad Data: Missing Values"),
                            selected = "Select a sample dataset"
                )
              )
          ),


          selectizeInput(
            "select_factors",
            "Specify Categorical variables:",
            choices = NULL,
            selected = NULL,
            multiple = TRUE
          ),

          textInput(
            "equation",
            "Model formula:",
            placeholder = "count_response ~ predictor1 + predictor2"
          ),

          selectInput(
          "offset_var",
          "Optional offset variable:",
          choices = c("None"),
          selected = "None"
          ),


          numericInput(
            "alpha",
            "Significance level:",
            value = 0.05,
            min = 0.001,
            max = 0.999,
            step = 0.001
          ),

          selectInput(
            "model_type",
            "Choose count regression model:",
            choices = c(
                      "Poisson",
                      "Quasi-Poisson",
                      "Negative Binomial",
                      "Zero-Inflated Poisson",
                      "Generalized Poisson"
                      ),
            selected = "Poisson"
          ),

          actionButton("DoCompute", "Fit Model"),

          tags$hr(),

          tags$a(
            href = "https://dannymolyneux.github.io/",
            target = "_blank",
            class = "btn btn-primary",
            "Back to Website"
          )
        ),

        mainPanel(
          tabsetPanel(
            id = "workPanel",

            tabPanel(
              "Data Preview",
              br(),
              shinycssloaders::withSpinner(
                DT::dataTableOutput("preview_data")
              )
            ),

            tabPanel(
              "Data Summary",
              br(),
              h3("Response Summary"),
              shinycssloaders::withSpinner(
              DT::dataTableOutput("response_summary")),
              br(),
              h3("Count Distribution"),
              shinycssloaders::withSpinner(
              plotOutput("count_distribution"))
            ),

            tabPanel(
              "Model Conditions",
              br(),
              h3("Condition Checks"),
              shinycssloaders::withSpinner(
              DT::dataTableOutput("condition_table")),
              br(),
              h3("Dispersion"),
              htmlOutput("dispersion_results"),
              br(),
              h3("Zero Inflation"),
              htmlOutput("zero_inflation_results")
              #br(),
              #h3("Model Suggestion"),
              #htmlOutput("model_suggestion")
            ),

            tabPanel(
              "Diagnostics",
              br(),
              helpText("RQR Plot: Residuals should be randomly scattered around 0 with no clear pattern."),
              helpText("QQ Plot: Points should lie close to the diagonal line. Heavy tails can indicate overdispersion or outliers, and severe deviations can suggest a poor model choice."),
              helpText("Pearson Residual Plot: No increasing trend should be visible. A strong upward pattern suggests overdispersion. A very scattered plot could suggest outliers or poor fit."),
              h3("Randomized Quantile Residuals and QQ Plot"),
              shinycssloaders::withSpinner(
                plotOutput("condition_plots")
              ),
              br(),
              h3("Fitted Values vs. Squared Pearson Residuals"),
              shinycssloaders::withSpinner(
                plotOutput("pearson_squared_plot")
              ),
              br(),
              h3("Goodness-of-Fit Statistics"),
              shinycssloaders::withSpinner(
                DT::dataTableOutput("gof_table")
              )
            ),

            tabPanel(
              "Model Comparison",
              br(),
              h3("Candidate Model Comparison"),
              shinycssloaders::withSpinner(
                DT::dataTableOutput("model_comparison")
              ),
              br(),
              h3("Recommended Model"),
              htmlOutput("comparison_recommendation")
            ),

            tabPanel(
                "Model Output and Interpretation",
                br(),

                h3("Model Summary"),
                helpText("This table shows coefficient estimates, standard errors, test statistics, p-values, and percent change in expected count (with confidenc interval)."),
                shinycssloaders::withSpinner(
                  DT::dataTableOutput("model_summary")
                ),

                br(),
                h3("Coefficient Interpretation"),
                helpText("Each sentence below gives the actual percent change in the expected count for each predictor."),
                htmlOutput("model_interpretation"),

                br(),
                h3("Estimated Marginal Mean Plots"),
                helpText("This plot shows estimated expected counts across a selected predictor, holding other predictors at their mean."),

                selectInput(
                  "emmeans_predictor",
                  "Choose predictor for estimated means plot:",
                  choices = c("None"),
                  selected = "None"
                ),
                shinycssloaders::withSpinner(
                  plotOutput("emmeans_plot")
                )
            )

            
          )
        )
      )
    ),
    tabPanel(
      "References",
      fluidPage(
        includeHTML("www/bib.html")
      )
    )  
  )
)