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

# python
library(reticulate)
use_condaenv("py3.8", required = TRUE)
source_python("py/simulator.py")

# core
library(tidyverse)

# plotting functions
source("R/plot.R")


# UI ----
ui <- tagList(
  
  # CSS ----
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = shinytheme("cosmo"))
  ),
  
  # JS ----
  shinyjs::useShinyjs(),
  
  # Website ----
  uiOutput(outputId = "website")
) 

# SERVER ----
server <- function(input, output, session) {

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
      plot_risk(simulation_obj = simulation)
      )
    
    output$cost_chart <- renderPlot(
      plot_cost(simulation$simulation_data, simulation$variable_guest_cost)
    )
    
    output$recommendation_chart <-  renderPlot(
      plot_recommendation(simulation_obj = simulation)
    )
    
  })
  
  observeEvent(input$guest_prob, {
    
  peak_value_check = between(
    input$peak, input$guest_prob[1], input$guest_prob[2]
    )
    
    updateNumericInput(session, "peak", min = input$guest_prob[1])
    updateNumericInput(session, "peak", max = input$guest_prob[2])
    
    if (!peak_value_check) {
      updateNumericInput(
        session, "peak",
        value = mean(input$guest_prob) %>% ceiling()
      )
    }
  })  
  
  # 1.3 Help ----
  observeEvent(input$help, {
    modalDialog(
      title = "Input Definitions",
      size = "m",
      easyClose = TRUE,
      h3("Total Budget"),
      p("What is the total available budget for the wedding?"),
      h3("Risk Tolerance %"),
      p("What is the largest chance of going over budget you can tolerate?"),
      h3("Guest Base Cost"),
      p("What is the fixed cost amount?"),
      h3("Guest Base Count"),
      p("How many guests are including in the base cost amount?"),
      h3("Variable Guest Cost"),
      p("What is cost per additional guest beyond the base amount?"),
      h3("Total Guests Invited"),
      p("How many guests are you inviting?"),
      h3("Guest Probability to Attend"),
      p("What is the range for the probability that each guest RSVPs yes?",
        "What is the most likely (peak) probability that each guest RSVPs yes?"
      ),
      footer = modalButton("Exit")
    ) %>% showModal()
  })


  # 2.0 RENDER WEBSITE ----
  output$website <- renderUI({
    
    navbarPage(
      
      # 1. SETTINGS ----
      
      # 1.1 Title ----
      title = "Wedding Budget Risk Asessment",
      # 1.2 Behavior ----
      fluid = TRUE, 
      collapsible = TRUE,
      theme = shinytheme("cosmo"),
      
      # 2. APP UI ----
      
      # 2.1 Analyzer ----
      tabPanel(
        title = "Analyzer",
        icon = icon("tachometer"),
        # 2.1.1 Header Row ----
        fluidRow(
          # 2.1.1.1 Quote ----
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
          # 2.1.1.2 Summary Values ----
          # get rid of and include in the chart titles
          # include simple sentence that explains what is happening here
          # use function with placeholders. 
          column(
            width = 9
          )
        ),
        # 2.1.2 User Inputs ----
        fluidRow(
          column(
            width = 3,
            div(
              style = str_c("background-color: #FAFAFA;",
                            "padding: 10px; border: 1px solid #424242"),
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
                      min = 0, max = 100,
                      value = c(60, 85), step = 5, post = " %"
                    ),
                    numericInput(
                      "peak", "Peak (%):", value = 75, min = 0, max = 100, 
                      step = 5
                    )
                  )
                )
              ),
              div(
                style = "vertical-align: bottom;",
                id = "input_buttons",
                actionButton(
                  inputId = "analyze",
                  label = "Analyze",
                  icon = icon("refresh")
                ),
                div(
                  class = "pull-right",
                  actionBttn(
                    inputId = "help",
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
      # 2.2 Learn More ----
      tabPanel(
        title = "Learn More",
        icon = icon("book"),
        includeHTML("learn_more.html")
      )
    )
  })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
