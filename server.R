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

}



