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
                selectInput("sample_data_choice","Sample Data:",
                            choices = c("Select a sample dataset",
                                        "Brockmann 1996",
                                        "Kitsberg 2025",
                                        "Ache Monkey (McMillan)",
                                        "Ache Monkey Trips",
                                        "Niyogi 2025"),
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
            placeholder = "count ~ x1 + x2"
          ),

          numericInput(
            "alpha",
            "Significance level:",
            value = 0.05,
            min = 0.001,
            max = 0.999,
            step = 0.001
          ),

          actionButton("DoCompute", "Fit Poisson Model"),

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
              DT::dataTableOutput("preview_data")
            ),

            tabPanel(
              "Summary",
              h3("Response Summary"),
              DT::dataTableOutput("response_summary"),
              h3("Count Distribution"),
              plotOutput("count_distribution")
            ),

            tabPanel(
              "Poisson Checks",
              htmlOutput("count_checks"),
              h3("Overdispersion"),
              htmlOutput("dispersion_results")
            ),

            tabPanel(
              "Model Output",
              h3("Model Summary"),
              DT::dataTableOutput("model_summary"),
              h3("Incidence Rate Ratios"),
              DT::dataTableOutput("irr_table")
            ),

            tabPanel(
              "Conditions",
              plotOutput("condition_plots"),
              DT::dataTableOutput("gof_table")
            )
          )
        )
      )
    )
  )
)