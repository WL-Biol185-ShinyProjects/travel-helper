library(shiny)
library(shinydashboard)
library(readxl)
library(tidyverse)
library(dplyr)


passport_info <- read.csv("passport-index-tidy.csv") 
currencyVcountry <- read.csv("currencyVcountry.csv")
vaccinationVcountry <- read.csv("vaccinationVcountry_correct.csv")
arrival_2025 <- read_excel("arrival information 2025.xlsx")
adapter_data <- read.csv("travel_adapter_converter.csv")
  colnames(arrival_2025) <- c("rank", "airport", "pct_on_time")
  arrival_2025$airport <- reorder(arrival_2025$airport, arrival_2025$pct_on_time)
UNESCO <- read_excel("UNESCO_World_Heritage_Sites.xlsx")

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
                
                box(
                  title = "Currency Info", status = "primary", solidHeader = TRUE,
                  selectizeInput("Country", 
                                 label = "Destination Country",
                                 choices = (currencyVcountry$Country)
                  ),
                  h4("Currency"), 
                  verbatimTextOutput("Currency")
                ),
                
                box(
                  title = "Vaccination Nedded", status = "primary", solidHeader = TRUE,
                  selectizeInput("country_vaccination", 
                                 label = "Country of destination",
                                 choices = (vaccinationVcountry$country_vaccination)
                  ),
                  h4("Vaccination required"), 
                  verbatimTextOutput("vaccination_required")
                ), 
                
                box(
                  title = "Electrical Adapter & Converter Requirements", status = "primary", solidHeader = TRUE,
                  selectizeInput("origin_country",
                                 label = "Country of Origin",
                                 choices = sort(unique(adapter_data$Origin.Country)),
                                 options = list(placeholder = "Select your home country...")
                  ),
                  selectizeInput("dest_country",
                                 label = "Destination Country",
                                 choices = sort(unique(adapter_data$Destination.Country)),
                                 options = list(placeholder = "Select your destination...")
                  ),
                  h4("Adapter Needed"),
                  verbatimTextOutput("adapter_needed"),
                  h4("Converter Needed"),
                  verbatimTextOutput("converter_needed"),
                  h4("Adapter Recommendation"),
                  verbatimTextOutput("adapter_rec"),
                  h4("Converter Recommendation"),
                  verbatimTextOutput("converter_rec")
                )
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
                box()
              )
      ),
      
      # Travel Suggestions tab
      tabItem(tabName = "travel_suggestions",
              fluidRow(
                box( ),
  )
  )
)
)
)
