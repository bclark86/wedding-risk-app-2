# CLARKLYTICS ----
# Shiny Business Analytics ----
# WEDDING BUDGET RISK MODELING -----
# Version 2

# APPLICATION DESCRIPTION ----
# - Assess financial risk of inviting N people to a wedding

# shiny
library(shiny)
library(shinyWidgets)
library(shinythemes)
library(shinyjs)

# utilities
library(config)

# python
library(reticulate)
use_miniconda("wedding_risk", required = TRUE)
source_python("py/simulator.py")

# core
library(tidyverse)

# plotting functions
source("R/plot.R")

config <- config::get()

# UI ----
ui <- tagList(
  
  # CSS ----
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = shinytheme("cosmo")),
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  
  # JS ----
  shinyjs::useShinyjs(),
  
  # Website ----
  uiOutput(outputId = "website")
) 

# SERVER ----
server <- function(input, output) {

  # 1.0 SIMULATION ----
  simulation = py$RiskSimulator(k = 10000, 
                                n = 150, 
                                p = list('left' = .60,
                                         'mode' = .75,
                                         'right' = .90),
                                fixed_cost = 22000, 
                                variable_guest_cost = 125, 
                                guest_base = 50, 
                                budget = 30000,
                                risk_tolerance = 0.2,
                                distribution = 'triangular')
  
  simulation$run()
  
  output$table <- renderTable(simulation$simulation_data %>% head())
  
  # 2.0 RENDER WEBSITE ----
  output$website <- renderUI({
    
    navbarPage(
      
      # 1. SETTINGS ----
      
      # 1.1 Title ----
      title = "Wedding Budget Risk Model",
      # 1.2 Behavior ----
      fluid = TRUE, 
      collapsible = TRUE,
      theme = shinytheme("cosmo"),
      
      # 2. APP UI ----
      
      # 2.1 Analyzer ----
      tabPanel(
        title = "Analyzer",
        # 2.1.1 Quote ----
        div(
          class = "container-fluid",
          column(
            width = 3,
            div(
              class = "container-fluid",
              p(
                class = "text-center",
                tags$em('"If you\'re a bird, I\'m a bird."')
              )
            )
          ),
          column(width = 9)
        ),
        # 2.1.2 App UI----
        div(
          class = "container-fluid",
          id = "application_ui",
          # 2.1.2.1 Inputs ----
          column(
            width = 3, 
            wellPanel(
              style = "margin: 15px 10px 10px 10px",
              div(
                class = "panel-header",
                style = "text-align: center;",
                h2("Model Inputs",
                   style = "margin-top: -5px;")
              ),
              tags$table(
                id = "inputs-table", 
                style = "width: 100%",
                tags$tr(
                  tags$td(
                    style = "width: 50%; text-align: center; padding: 5px;",
                    numericInput(
                      "invited_guests", "Guests Invited",
                      150, min = 0, max = 1e6, step = NA
                    )
                  ), # column 1
                  tags$td(
                    style = "width: 50%; text-align: center; padding: 5px;",
                    div(
                      class = "form-group shiny-input-container",
                      numericInput(
                        "guest_base", "Guest Base Count",
                        50, min = 0, max = 1e6, step = 1
                      )
                    )
                  ) # column 2
                ), # table row 1
                tags$tr(
                  tags$td(
                    style = "width: 50%; text-align: center; padding: 5px;",
                    numericInput(
                      inputId = "fixed_cost", "Base Cost",
                      22000, min = 0, max = 1e9, step = 100
                    )
                  ), # column 1
                  tags$td(
                    style = "width: 50%; text-align: center; padding: 5px;",
                    div(
                      class = "form-group shiny-input-container",
                      numericInput(
                        inputId = "var_cost", "Cost Per Guest",
                        125, min = 0, max = 1000,  step = NA
                      )
                    )
                  ) # column 2
                ) # table row 2
              )
            )
            # wellPanel(
            #   div(
            #     id = "input_header",
            #     h2("Model Inputs")
            #   ),
            #   div(
            #     id = "input_main",
            #     numericInput(
            #       inputId = "budget", "Total Budget",
            #       30000, min = 0, max = 1e9,  step = 100
            #     ),
            #     numericInput(
            #       inputId = "invited_guests", "Guests Invited",
            #       150, min = 0, max = 1e6, step = NA
            #     ),
            #     sliderInput(
            #       inputId = "risk_tolerance", "Risk Tolerance: ",
            #       min = 0, max = 100, value = 20, step = 5, post  = " %"
            #     ),
            #     numericInput(
            #       inputId = "fixed_cost", "Guest Base Cost: ",
            #       22000, min = 0, max = 1e9,step = 100
            #     ),
            #     numericInput(
            #       inputId = "guest_base", "Guest Base Count: ",
            #       50, min = 0, max = 1e6, step = 1, width = NULL
            #     ),
            #     numericInput(
            #       inputId = "var_cost", "Variable Guest Cost: ", 
            #       125, min = 0, max = 1000, step = NA, width = NULL
            #     )
            #   ),
            #   div(
            #     id = "input_buttons",
            #     hr(),
            #     actionButton(inputId = "analyze", label = "Analyze", icon = icon("download")),
            #     div(
            #       class = "pull-right",
            #       actionButton(inputId = "settings_toggle", label = NULL, icon = icon("cog"))
            #     )
            #   ),
            #   div(
            #     id = "input_settings",
            #     sliderInput(inputId = "mavg_short", 
            #                 label   = "Short Moving Average (Days)", 
            #                 value   = 5, 
            #                 min     = 5, 
            #                 max     = 40),
            #     sliderInput(inputId = "mavg_long", 
            #                 label   = "Long Moving Average (Days)", 
            #                 value   = 50, 
            #                 min     = 50, 
            #                 max     = 120),
            #     sliderInput(inputId = "time_window", 
            #                 label   = "Time Window (Days)", 
            #                 value   = 180, 
            #                 min     = 180, 
            #                 max     = 730),
            #     actionButton(inputId = "apply_and_save", label = "Apply & Save", icon = icon("save"))
            #   )
            # )
          ),
          column(
            width = 8,
            div(
              class = "container",
              tableOutput('table')
            )
          )
        )
      ),
      # 2.2 About ----
      tabPanel(
        title = "Learn More",
        div(
          class = "container",
          id = "header",
          h1(class = "page-header", "Wedding Budget Risk Analyzer", tags$small("by Clarklytics")),
          p(class = "lead", "This is version 2.0 of the original app created using flexdashboard:", 
            a(href = "https://github.com/bclark86/WeddingRiskModel", target = "_blank", "Wedding Risk Modeling App"))
        ),
        div(
          class = "container",
          p("Placeholder for rmarkdown with the overview of how it works.")
        )
      )
    )
  })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
