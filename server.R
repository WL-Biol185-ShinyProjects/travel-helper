library(shiny)
library(ggplot2)
library(tidyverse)

passport_info <- read.csv("passport-index-tidy.csv") 
currencyVcountry <- read.csv("currencyVcountry.csv")
  
function(input, output) {
  
 #Renderblock passport 
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
    
  }
