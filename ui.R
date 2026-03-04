library(shiny)
library(shinydashboard)
library(plotly)

passport_info <- read.csv("passport-index-tidy.csv") 
arrival_2025 <- read_excel("arrival information 2025.xlsx")
  colnames(arrival_2025) <- c("rank", "airport", "pct_on_time")
  arrival_2025$airport <- reorder(arrival_2025$airport, arrival_2025$pct_on_time)



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
                  title = "What Travel Requirements do you Need?", status = "primary", solidHeader = TRUE,
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
                box(
                  title = "What is the Best Airport to Fly into?", status = "primary", solidHeader = TRUE,
                  width = 8,
                  plotOutput("percentage_on_time", height = 400)),
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
                box(),
                box()
              )
      )
  )
  )
)
