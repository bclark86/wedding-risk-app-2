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
library(plotly)
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
  
  ## 1.1 Settings ----
  
  observeEvent(input$settings_toggle, {
    toggle(id = "input_settings", anim = TRUE)
  })
  
  # 1.2 User Input ----
  observeEvent(input$analyze, {
    simulation = py$RiskSimulator(
      k = 10000, 
      n = input$invited_guests,
      p = list('left' = (input$guest_prob[1] / 100),
               'mode' = (input$peak / 100),
               'right' = (input$guest_prob[2] / 100)),
      fixed_cost = input$fixed_cost, 
      variable_guest_cost = input$var_cost, 
      guest_base = input$guest_base, 
      budget = input$budget,
      risk_tolerance = input$risk / 100,
      distribution = 'triangular'
    )
    
    simulation$run()
    
    output$guest_chart <- output$cost_chart <- renderPlot(
      plot_guest_count(simulation$simulation_data)
      )
    
    output$risk_chart <- renderPlot(
      plot_risk(simulation$simulation_data, simulation$variable_guest_cost)
      )
    
    output$cost_chart <- renderPlot(
      plot_cost(simulation$simulation_data, simulation$variable_guest_cost)
    )
    
    output$recommendation_chart <-  renderPlot(
      plot_recommendation(simulation_obj = simulation)
    )
    
  })
  
  
  
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
        fluidRow(
          column(
            width = 3,
            div(
              class = "container-fluid",
              p(
                class = "text-center",
                tags$em('"If you\'re a bird, I\'m a bird." - The Notebook')
              )
            )
          ),
          column(
            width = 9
          )
        ),
        # 2.1.2 User Inputs ----
        fluidRow(
          column(
            width = 3,
            div(
              style = "background-color: #FAFAFA; padding: 10px; border: 1px solid #424242",
              div(
                style = "margin-top: -20px;",
                h2("Budget Risk Engine", style = "text-align: center;")
              ),
              div(
                style = "padding-right: 5px;",
                id = "input_main",
                hr(),
                # 2.1.2.1 Wedding Info ----
                div(
                  h4("Wedding & Budget Information"),
                ),
                splitLayout(
                  div(
                    style = "padding-right: 10px;",
                    numericInput(
                      "budget", "Total Budget:", 30000, min = 0, max = 1e9, 
                      step = 100
                    )
                  ),
                  div(
                    style = "padding-right 10px;",
                    numericInput(
                      "risk", "Risk Tolerance %:", 20, min = 0, max = 100, 
                      step = 5
                    )
                  )
                ),
                splitLayout(
                  div(
                    style = "padding-right: 10px;",
                    numericInput(
                      "fixed_cost", "Guest Base Cost: ",
                      22000, min = 0, max = 1e9, step = 100
                    )
                  ),
                  div(
                    style = "padding-right 10px;",
                    numericInput(
                      "var_cost", "Variable Guest Cost:",
                      125, min = 0, max = 1000,  step = NA, width = NULL
                    )
                  )
                ),
                splitLayout(
                  div(
                    style = "padding-right: 10px;",
                    numericInput(
                      "guest_base", "Guest Base Count:",
                      50, min = 0, max = 1e6, step = 1, width = NULL
                    )
                  ),
                  div(
                    style = "padding-right 10px;",
                    numericInput(
                      "invited_guests", "Total Guests Invited:",
                      150, min = 0, max = 1e6,  step = NA, width = NULL
                    )
                  )
                )
              ),
              # 2.1.2.2 Probabilities ----
              div(
                id = "input_settings",
                hr(),
                div(
                  h4("Guest Probability to Attend"),
                ),
                div(
                  style="vertical-align:top;",
                  splitLayout(
                    cellWidths = c("70%", "30%"),
                    cellArgs = list(style = "padding-right: 10px;"),
                    sliderInput(
                      "guest_prob", "Range (%):",
                      min = 0, max = 100, value = c(60, 85), step = 5, post  = " %"
                    ),
                    numericInput(
                      "peak", "Peak (%):", 75, min = 0, max = 100, 
                      step = 5
                    )
                  )
                )
              ),
              div(
                style = "vertical-align: middle;",
                id = "input_buttons",
                actionButton(
                  inputId = "analyze",
                  label = "Analyze",
                  icon = icon("refresh")
                ),
                div(
                  class = "pull-right",
                  actionBttn(
                    inputId = "Id103",
                    label = NULL,
                    style = "material-circle", 
                    color = "default",
                    size = "xs",
                    icon = icon("question")
                  )
                )
              )
            )
          ),
          # 2.1.3 Simulation Plots ----
          div(
            class = "container-fluid",
            style = "margin-right: 5px;",
            column(
              width = 9,
              fluidRow(
                splitLayout(
                  div(
                    style = "margin-right: 5px; margin-bottom: 5px;",
                    div(
                      style = "margin: 5px;",
                      plotOutput('recommendation_chart', height = 290)
                    )
                  ),
                  div(
                    style = "margin-right: 5px; margin-bottom: 5px;",
                    div(
                      style = "margin: 5px;",
                      plotOutput('risk_chart', height = 290))
                  )
                )
              ),
              fluidRow(
                splitLayout(
                  div(
                    style = "margin-right: 5px; margin-top: 5px;",
                    div(
                      style = "margin: 5px;",
                      plotOutput('guest_chart', height = 290)
                    )
                  ),
                  div(
                    style = "margin-right: 5px; margin-top: 5px;",
                    div(
                      style = "margin: 5px;",
                      plotOutput('cost_chart', height = 290))
                  )
                )
              )
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
          h1(class = "page-header", "Wedding Budget Risk Analyzer",
             tags$small("by Clarklytics")),
          p(class = "lead",
            "This is version 2.0 of the original app created using flexdashboard:", 
            a(href = "https://github.com/bclark86/WeddingRiskModel",
              target = "_blank", "Wedding Risk Modeling App"))
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
