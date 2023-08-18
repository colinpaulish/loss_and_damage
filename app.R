#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
setwd("~/Projects/loss_and_damages/Loss_and_Damages")

ui <- fluidPage(
  # Set the background color
  theme = shinytheme("cerulean"),
  
  # App title
  titlePanel("Loss and Damage Calculator"),
  
  fluidRow(
  # Input: Funds to distribute
  column(width = 3,
         numericInput("funds", "Total Funds:", value = 100000, step = 1000),
  ),
  # Input: Weights for GDP, emissions, renewable share
  column(width = 3,
         numericInput("weight_gdp", "Weight (GDP):", value = 0.4, min = 0, max = 1, step = 0.01),
  ),
  column(width = 3,
                numericInput("weight_emissions", "Weight (Emissions):", value = 0.3, min = 0, max = 1, step = 0.01),
  ),
  column(width = 3,
         numericInput("weight_renewable", "Weight (Renewable Share):", value = 0.3, min = 0, max = 1, step = 0.01),
  )),
  # Output: Allocation results
  verbatimTextOutput("allocation_result")
)
  
  server <- function(input, output) {
    # Your processed data (replace with your actual data)
    ld_processed <- read.csv('/data/ld_processed.csv')
    print(head(ld_processed))
    # Calculate the allocated funds based on weights and input funds
    output$allocation_result <- renderText({
      allocated_funds <- input$funds * (input$weight_gdp * processed_data$composite_score +
                                          input$weight_emissions * processed_data$composite_score +
                                          input$weight_renewable * processed_data$composite_score)
      paste("Allocated Funds: $", prettyNum(allocated_funds, big.mark = ","))
    })
  }
  

shinyApp(ui, server)