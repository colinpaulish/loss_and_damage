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
library(plotly)
library(leaflet)
library(sf)
library(rnaturalearth)

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
           numericInput("weight_gdp", "Weight (GDP):", value = 0.55, min = 0, max = 1, step = 0.01),
    ),
    column(width = 3,
           numericInput("weight_emissions", "Weight (Emissions):", value = 0.35, min = 0, max = 1, step = 0.01),
    ),
    column(width = 3,
           numericInput("weight_renewable", "Weight (Renewable Share):", value = 0.1, min = 0, max = 1, step = 0.01),
    )), 
  fluidRow(
    # Input: Funds to distribute
    column(width = 3,
           numericInput("vulnerability", "Weight (Vulnerability):",  value = 0.2, min = 0, max = 1, step = 0.01),
    ),
    # Input: Weights for GDP, emissions, renewable share
    column(width = 3,
           numericInput("readiness", "Weight (Readiness):", value = 0.1, min = 0, max = 1, step = 0.01),
    ),
    column(width = 3,
           numericInput("gdp", "Weight (GDP):", value = 0.5, min = 0, max = 1, step = 0.01),
    ),
    column(width = 3,
           numericInput("exposure", "Weight (Environmental Exposure):", value = 0.2, min = 0, max = 1, step = 0.01),
    )), 
  fluidRow(column(width = 5, 
    tableOutput("contribute_amt_table")),
    (column(width = 5, 
                  tableOutput("receive_amt_table")))
  ),
  fluidRow(column(width = 5,
            plotlyOutput("contribute_plot"))),
  fluidRow(column(width = 12,
                  leafletOutput("leaflet_map")))
  
)

server <- function(input, output) { 
  
  
    ld_processed <-  read.csv('../data/ld_processed.csv')
    

    output$ld_processed <- renderTable({ld_processed})

  
  
  # Automatically multiply when emissions_share input changes
 
  
  # Display table with multiplied values
  contribute_df <- reactive({
      #multiplier <- input$weight_emissions
      data <- ld_processed %>% 
        filter(designation == 'advanced_economy') %>%
        mutate(contribute_raw = input$weight_emissions * emissions_share_scaled + 
                 input$weight_gdp * gdp_reformed_scaled + 
                 input$weight_renewable * renewable_share_2020_scaled) %>%
        mutate(contribute_scaled = (contribute_raw / sum(contribute_raw, na.rm = T)*100)) %>%
        mutate(funds = contribute_scaled * input$funds,
               contribute_gdp_perc = funds / gdp_2021) %>%
        arrange(desc(contribute_scaled))

      data2 <- data %>% select(country = country.x, contribute_scaled, funds, contribute_gdp_perc)
      
      data3 <- data2[order(data2$contribute_scaled, decreasing = FALSE), ]
      data3$country <- factor(data3$country, levels = data3$country)       
      
      data3
      
      })
  
  output$contribute_amt_table <- renderTable({contribute_df()})
  
  
  
  
  output$contribute_plot <- renderPlotly({ plot_ly(contribute_df(), x = ~contribute_scaled, y = ~country, type = "bar", marker = list(color = "green")) %>%
      layout(title = "Contribution Funds by Country",
             xaxis = list(title = "Contribution Fund"),
             yaxis = list(title = "Country"))
  })
  
  
  receive_df <- reactive({
    #multiplier <- input$weight_emissions
    data <- ld_processed %>% 
      filter(designation != 'advanced_economy') %>%
      mutate(receive_raw = input$vulnerability * vulnerability_2021_scaled + 
               input$exposure * env_exposure_2022_scaled + 
               input$gdp * gdp_reformed_scaled + 
               input$readiness * readiness_2021_scaled) %>%
      mutate(receive_scaled = (receive_raw / sum(receive_raw, na.rm = T)*100)) %>%
      mutate(funds = (receive_scaled * input$funds)/100,
             receieve_gdp_perc = funds/gdp_2021) %>%
      arrange(desc(receive_scaled))
    print(class(data$funds))
    
    data2 <- data %>% select(country = country.x, receive_scaled, funds,receieve_gdp_perc)
    
    print(sum(data2$receive_scaled, na.rm = T))
    
    na_countries <- data2 %>% filter(is.na(receive_scaled))
    na_list <- na_countries$country
    print(na_list)
    
    data2
  })
  
  output$receive_amt_table <- renderTable({receive_df()})
  
  
  
  output$leaflet_map <- renderLeaflet({
      world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
      
      leaflet(data = world) %>%
        addTiles() %>%
        addPolygons(
          fillColor = ~colorNumeric(
            palette = c("gray", "blue"),
            domain = receive_df()$receive_scaled
          )(receive_df()$receive_scaled),
          fillOpacity = 0.7,
          stroke = FALSE
        )
  })

  
  
}
  
  
  

shinyApp(ui, server)