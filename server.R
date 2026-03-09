library(shiny)
library(ggplot2)
library(tidyverse)
library(readxl)
UNESECO <- read_excel(UNESCO_World_Heritage_Sites.xlsx)

passport_info <- read.csv("passport-index-tidy.csv") 
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
  
}
server <- function(input, output) {
  output$sites_table <- renderTable({
    heritage_data[heritage_data$country == input$country, "site", drop = FALSE]
  })
}


