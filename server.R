library(shiny)
library(ggplot2)
library(tidyverse)

passport_info <- read.csv("passport-index-tidy.csv")

v <- c("90 Days Visa Free", "30 Days Visa Free", "60 Days Visa Free", "360 Days Visa Free", "21 Days Visa Free", "28 Days Visa Free", "19 Days Visa Free",
       "60 Days Visa Free","180 Days Visa Free", "14 Days Visa Free","42 Days Visa Free","15 Days Visa Free", "240 Days Visa Free", "120 Days Visa Free", 
       "Electronic Travel Authorization","Electronic Visa Needed", "Visa Required", "Visa on Arrival", "Visa Free", "In-Country, No Visa Needed")
names(v) <- c(90, 30,60,360,21,28,19,15,180,14,42,15,240,120, "eta" , "e-visa", "visa required", "visa on arival", "visa free", -1)
passport_info$Requirement <- v[passport_info$Requirement]

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
  