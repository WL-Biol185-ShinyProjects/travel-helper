library(shiny)
library(shinydashboard)
passport_info <- read.csv("passport-index-tidy.csv")
library(readxl)
UNESECO <- read_excel(UNESCO_World_Heritage_Sites.xlsx)
dashboardPage(
  dashboardHeader(title = "Travel Helper"),
  # Sidebar content
  dashboardSidebar(
    sidebarMenu(
      menuItem("Welcome",            tabName = "welcome"),
      menuItem("International Travel", tabName = "international_travel"),
      menuItem("Weather",            tabName = "weather"),
      menuItem("Airports",           tabName = "airports"),
      menuItem("Airlines",           tabName = "airlines"),
      menuItem("Pricing",            tabName = "pricing"),
      menuItem("Travel Suggestions", tabName = "travel_suggestions")
    )
  ),

  # Body content
  dashboardBody(
    tabItems(
      
      # Welcome tab
      tabItem(tabName = "welcome",
              fluidRow(
                box(),
                box()
              )
      ),
      
      # International Travel tab
      tabItem(tabName = "international_travel",
              fluidRow(
                box(
                  selectizeInput("Passport", 
                                 label = "Your Passport",
                                 choices = unique(passport_info$Passport)
                                 
                                ),
                  selectizeInput("Destination", 
                                 label = "Your Destination",
                                 choices = unique(passport_info$Destination)
                                ),
              
                  h4("Requirement"), 
                  verbatimTextOutput("Requirement")
                  
                  ),
                
                box()
              )
      ),
      
      # Weather tab
      tabItem(tabName = "weather",
              fluidRow(
                box(),
                box()
              )
      ),
      
      # Airports tab
      tabItem(tabName = "airports",
              fluidRow(
                box(),
                box()
              )
      ),
      
      # Airlines tab
      tabItem(tabName = "airlines",
              fluidRow(
                box(),
                box()
              )
      ),
      
      # Pricing tab
      tabItem(tabName = "pricing",
              fluidRow(
                box(),
                box()
              )
      ),
      
      # Travel Suggestions tab
      tabItem(tabName = "travel_suggestions",
              fluidRow(
                box(selectizeInput("Destination", 
                                   label = "Destination Country",
                                   choices = unique(UNESECO$Country)),
                    mainPanel(
                      tableOutput("sites_table"),
                   
                box()
              )
      )
  )
  )
)
)
)