library(shiny)

fluidPage(
  
  sidebarLayout(
    sidebarPanel(
      selectizeInput("Passport", 
                     label = "Your Passport",
                     choices = passport_info$Passport
        
      ),
      selectizeInput("Destination", 
                     label = "Your Destination",
                     choices = passport_info$Destination
    )
    ),
    mainPanel(
      h4("Requirement"), 
      verbatimTextOutput("Requirement")
    )
  ),
  
  
  
  
  
  sidebarLayout(
    sidebarPanel(
                     
      ),
    mainPanel(
      plotOutput("%")
      )
    ),
    
    )



