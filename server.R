library(shiny)
library(ggplot2)
library(tidyverse)
library(readxl)
library(plotly)


 #Renderblock passport 


passport_info <- read.csv("passport-index-tidy.csv") 
currencyVcountry <- read.csv("currencyVcountry.csv")
vaccinationVcountry <- read.csv("vaccinationVcountry.csv")

arrival_2025 <- read_excel("arrival information 2025.xlsx")
  colnames(arrival_2025) <- c("rank", "airport", "pct_on_time")
  arrival_2025$airport <- reorder(arrival_2025$airport, arrival_2025$pct_on_time)
  

function(input, output) {
  
 #Render block passport 

   #passport_info$Requirement <- v[passport_info$Requirement]
  output$Requirement <- renderText({
    result <- passport_info %>% 
      filter(Passport == input$Passport, Destination == input$Destination) 
    if(nrow(result) > 0) {
      result$Requirement[1]
    } else {
      "No information available"
    }
  })
  
  #Renderblock currency
  output$Currency <- renderText({
    currencyVcountry [currencyVcountry$Country == input$Country, "Currency"]
     })
    
  #Renderblock vaccinations NEED TO FIX NEXT CLASS!!!
  output$`Vaccination Required` <- renderText({
    vaccinationVcountry [vaccinationVcountry$Country == input$Country, "`Vaccination Required`"]
  })
  

  #Render block arrival 2025
  output$percentage_on_time <- renderPlot({
    ggplot(arrival_2025, aes(x = airport, y = pct_on_time, fill = pct_on_time,
                                  )) +
      geom_col() +
      coord_flip() +
      scale_fill_gradient(low = "#f4a261", high = "#2a9d8f") +
      labs(
        title = "Airports Ranked by On-Time Arrival Percentage",
        x = "On-Time Percentage (%)",
        y = "Airport",
        fill = "% On-Time"
      ) +
      theme_minimal()
  })
  
  # Filter route on button click
  route_data <- eventReactive(input$search, {
    req(input$origin, input$dest)
    airfare_data %>%
      filter(
        (city1 == input$origin & city2 == input$dest) |
          (city1 == input$dest   & city2 == input$origin)
      )
  })
  
  # Bar chart: cheapest carrier for selected route
  output$fare_plot <- renderPlot({
    df <- route_data()
    validate(need(nrow(df) > 0, "No data found for this route. Try swapping origin and destination."))
    
    df %>%
      group_by(carrier_low) %>%
      summarise(avg_fare = mean(fare_low, na.rm = TRUE), .groups = "drop") %>%
      ggplot(aes(x = reorder(carrier_low, avg_fare), y = avg_fare, fill = avg_fare)) +
      geom_col() +
      geom_text(aes(label = paste0("$", round(avg_fare, 2))), hjust = -0.1, size = 3.5) +
      coord_flip() +
      scale_fill_gradient(low = "#2a9d8f", high = "#e76f51") +
      scale_y_continuous(limits = c(0, max(df$fare_low, na.rm = TRUE) * 1.2)) +
      labs(title = paste("Cheapest Carriers:", input$origin, "->", input$dest),
           x = "Carrier", y = "Average Lowest Fare ($)") +
      theme_minimal() +
      theme(legend.position = "none")
  })
  
  # Summary table for selected route
  output$fare_table <- renderTable({
    df <- route_data()
    validate(need(nrow(df) > 0, "No data found for this route."))
    
    df %>%
      group_by(carrier_low) %>%
      summarise(
        `Avg Lowest Fare`        = paste0("$", round(mean(fare_low, na.rm = TRUE), 2)),
        `Avg Large Carrier Fare` = paste0("$", round(mean(fare_lg,  na.rm = TRUE), 2)),
        `Routes Found`           = n(),
        .groups = "drop"
      ) %>%
      rename(Carrier = carrier_low) %>%
      arrange(`Avg Lowest Fare`)
  })
  
  # Bar chart: which carrier is cheapest most often overall
  output$carrier_count_plot <- renderPlot({
    airfare_data %>%
      count(carrier_low, sort = TRUE) %>%
      ggplot(aes(x = reorder(carrier_low, n), y = n, fill = n)) +
      geom_col() +
      geom_text(aes(label = n), hjust = -0.1, size = 3.5) +
      coord_flip() +
      scale_fill_gradient(low = "#f4a261", high = "#2a9d8f") +
      labs(title = "Which Airline is Most Often the Cheapest?",
           x = "Carrier Code", y = "Number of Routes as Cheapest") +
      theme_minimal() +
      theme(legend.position = "none")
  })
}
server <- function(input, output) {
  output$sites_table <- renderTable({
    heritage_data[heritage_data$country == input$country, "site", drop = FALSE]
  })
}



