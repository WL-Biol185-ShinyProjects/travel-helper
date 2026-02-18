library(shiny)
library(ggplot2)
library(tidyverse)

passport_info <- read.csv("passport-index-tidy.csv")

Requirement_new <- c("90 Days Visa Free","30 Days Visa Free")
names(Requirement_new) <- c("90", "30")
passport_info$Requirement <- Requirement_new(passport_info$Requirement)

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
  