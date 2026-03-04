library(shiny)
library(ggplot2)
library(tidyverse)

<<<<<<< HEAD
function(input, output) {passport_info <- read.csv("passport-index-tidy.csv") }


=======
passport_info <- read.csv("passport-index-tidy.csv") 
  
>>>>>>> 3e29dd50e8c56f565ebefb1ba7370ffbc50a5459

  
<<<<<<< HEAD



=======
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
>>>>>>> 3e29dd50e8c56f565ebefb1ba7370ffbc50a5459
