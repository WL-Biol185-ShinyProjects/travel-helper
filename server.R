library(shiny)
library(ggplot2)
library(tidyverse)
library(readxl)
library(dplyr)

 #Renderblock passport 


# Data loading
UNESCO <- read_excel("UNESCO_World_Heritage_Sites.xlsx")
passport_info <- read.csv("passport-index-tidy.csv") 
currencyVcountry <- read.csv("currencyVcountry.csv")
adapter_data <- read.csv("travel_adapter_converter.csv")
vaccinationVcountry <- read.csv("vaccinationVcountry_correct.csv")
arrival_2025 <- read_excel("arrival information 2025.xlsx")
  colnames(arrival_2025) <- c("rank", "airport", "pct_on_time")
  arrival_2025$airport <- reorder(arrival_2025$airport, arrival_2025$pct_on_time)

  v <- c(
    "90"            = "90 Days Visa Free",
    "30"            = "30 Days Visa Free",
    "60"            = "60 Days Visa Free",
    "360"           = "360 Days Visa Free",
    "21"            = "21 Days Visa Free",
    "28"            = "28 Days Visa Free",
    "19"            = "19 Days Visa Free",
    "180"           = "180 Days Visa Free",
    "14"            = "14 Days Visa Free",
    "42"            = "42 Days Visa Free",
    "15"            = "15 Days Visa Free",
    "240"           = "240 Days Visa Free",
    "120"           = "120 Days Visa Free",
    "eta"           = "Electronic Travel Authorization",
    "e-visa"        = "Electronic Visa Needed",
    "visa required" = "Visa Required",
    "visa on arrival" = "Visa on Arrival",
    "visa free"     = "Visa Free",
    "-1"            = "In-Country, No Visa Needed"
  )
  passport_info$Requirement <- recode(passport_info$Requirement, !!!v)
  
# Server
function(input, output) {
  
  # Passport requirement
  output$Requirement <- renderText({
    result <- passport_info %>% 
      filter(Passport == input$Passport, Destination == input$Destination) 
    if (nrow(result) > 0) {
      result$Requirement[1]
    } else {
      "No information available"
    }
  })
  
  # Currency
  output$Currency <- renderText({

    currencyVcountry [currencyVcountry$Country == input$Country, "Currency"]
     })
  
  # Vaccinations
  output$vaccination_required <- renderText({
    vaccinationVcountry[vaccinationVcountry$country_vaccination == input$country_vaccination, "vaccination_required"]
  })
  
  # Adapter
  adapter_result <- reactive({
    req(input$origin_country, input$dest_country)
    
    adapter_data %>%
      filter(Origin.Country == input$origin_country,
             Destination.Country == input$dest_country)
  })
  
  output$adapter_needed <- renderText({
    req(nrow(adapter_result()) > 0)
    adapter_result()$Adapter.Needed
  })
  
  output$converter_needed <- renderText({
    req(nrow(adapter_result()) > 0)
    adapter_result()$Converter.Needed
  })
  
  output$adapter_rec <- renderText({
    req(nrow(adapter_result()) > 0)
    adapter_result()$Adapter.Recommendation
  })
  
  output$converter_rec <- renderText({
    req(nrow(adapter_result()) > 0)
    adapter_result()$Converter.Recommendation
  })
  
  # Airports chart
  output$percentage_on_time <- renderPlot({
    ggplot(arrival_2025, aes(x = airport, y = pct_on_time, fill = pct_on_time)) +
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
  
  # UNESCO sites
  output$sites_table <- renderUI({
    req(input$UNESCOCountry)
    sites <- UNESCO[UNESCO$Country == input$UNESCOCountry, "World Heritage Site", drop = TRUE]
    tagList(
      h4(paste0(input$UNESCOCountry, " (", length(sites), " sites)")),
      tags$ul(lapply(sites, tags$li))
    )
  })
}