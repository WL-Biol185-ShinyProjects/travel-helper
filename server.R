library(shiny)
library(ggplot2)
library(tidyverse)

passport_info <- read.csv("passport-index-tidy.csv")

function(input, output) {
  output$Requirement <- renderText({
    result <- passport_info %>% 
        filter(Passport == input$Passport, Destination == input$Destination) 
    if(nrow(result) > 0) {
      result$Requirement[1]
    } else {
      "No information available"
    }
    })


}
  