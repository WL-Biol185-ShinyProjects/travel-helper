library(shiny)
library(shinydashboard)
library(readxl)
library(plotly)
library(leaflet)

passport_info <- read.csv("passport-index-tidy.csv")
currencyVcountry <- read.csv("currencyVcountry.csv")
vaccinationVcountry <- read.csv("vaccinationVcountry.csv")
arrival_2025 <- read_excel("arrival information 2025.xlsx")
colnames(arrival_2025) <- c("rank", "airport", "pct_on_time")
arrival_2025$airport <- reorder(arrival_2025$airport, arrival_2025$pct_on_time)
UNESCO <- read_excel("UNESCO_World_Heritage_Sites.xlsx")
world_cities <- read_excel("worldcities.xlsx")
world_cities <- world_cities[!is.na(world_cities$population) & 
                               world_cities$population > 500000, ]

dashboardPage(
  dashboardHeader(title = "Travel Helper"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Welcome",              tabName = "welcome"),
      menuItem("International Travel", tabName = "international_travel"),
      menuItem("Weather",              tabName = "weather"),
      menuItem("Airports",             tabName = "airports"),
      menuItem("Airlines",             tabName = "airlines"),
      menuItem("Pricing",              tabName = "pricing"),
      menuItem("Travel Suggestions",   tabName = "travel_suggestions")
    )
  ),
  
  dashboardBody(
    tabItems(
      
      # Welcome tab
      tabItem(tabName = "welcome",
              fluidRow(
                box(
                  width = 12, status = "primary", solidHeader = TRUE,
                  title = "Welcome to Travel Helper! ✈️",
                  h3("Welcome to Travel Helper!"),
                  p("Navigate through the sidebar tabs so we can help you plan for your trip!"),
                  br(),
                  p("Here's what you can find in each tab:"),
                  tags$ul(
                    tags$li("🌍 International Travel — Check visa requirements, currency, and vaccination info"),
                    tags$li("🌤️ Weather — Click any city on the map for a 7-day forecast"),
                    tags$li("✈️ Airports — Find the best airports ranked by on-time arrival"),
                    tags$li("💺 Airlines — Explore airline options"),
                    tags$li("💰 Pricing — Compare travel pricing"),
                    tags$li("🗺️ Travel Suggestions — Discover UNESCO World Heritage Sites to visit")
                  )
                )
              ),
              fluidRow(
                box(
                  width = 6, status = "info", solidHeader = TRUE,
                  title = "💡 Travel Tips",
                  p("Always check your passport expiration date before booking — many countries require
                    at least 6 months validity beyond your travel dates!"),
                  br(),
                  h4("📋 Enroll in STEP!"),
                  p("The Smart Traveler Enrollment Program (STEP) is a free service from the U.S. Department
                    of State that allows U.S. citizens traveling abroad to register their trip with the nearest
                    U.S. Embassy or Consulate."),
                  p("Benefits of enrolling:"),
                  tags$ul(
                    tags$li("Receive safety alerts and updates for your destination"),
                    tags$li("Make it easier for the Embassy to contact you in an emergency"),
                    tags$li("Help family and friends reach you in a crisis")
                  ),
                  tags$a(href = "https://step.state.gov", target = "_blank",
                         "👉 Enroll in STEP at step.state.gov")
                ),
                box(
                  width = 6, status = "warning", solidHeader = TRUE,
                  title = "📋 Trip Planning Checklist",
                  tags$ul(
                    tags$li("✅ Check visa requirements"),
                    tags$li("✅ Verify passport expiration"),
                    tags$li("✅ Review vaccination requirements"),
                    tags$li("✅ Research local currency"),
                    tags$li("✅ Find the best airport"),
                    tags$li("✅ Discover sites to visit")
                  )
                )
              )
      ),
      
      # International Travel tab
      tabItem(tabName = "international_travel",
              fluidRow(
                box(
                  title = "What Travel Requirements do you Need?", status = "primary", solidHeader = TRUE,
                  selectizeInput("Passport",
                                 label = "Your Passport",
                                 choices = unique(passport_info$Passport)),
                  selectizeInput("Destination",
                                 label = "Your Destination",
                                 choices = unique(passport_info$Destination)),
                  h4("Requirement"),
                  verbatimTextOutput("Requirement")
                ),
                box(
                  selectizeInput("Country",
                                 label = "Destination Country",
                                 choices = currencyVcountry$Country),
                  h4("Currency"),
                  verbatimTextOutput("Currency")
                ),
                box(
                  selectizeInput("Country",
                                 label = "Destination Country",
                                 choices = vaccinationVcountry$Country),
                  h4("Vaccination Required"),
                  verbatimTextOutput("Vaccination Required")
                )
              )
      ),
      
      # Weather tab
      tabItem(tabName = "weather",
              fluidRow(
                box(
                  width = 12, status = "primary", solidHeader = TRUE,
                  title = "🌤️ World Weather",
                  p("Use the dropdowns to jump to a country and city, or browse the map and click any city marker to get the current conditions and 7-day forecast."),
                  p("🔍 Cities are clustered — zoom in to see individual city markers, then click to get weather.")
                )
              ),
              fluidRow(
                box(
                  width = 4, status = "info", solidHeader = TRUE,
                  title = "Jump to a Location",
                  selectizeInput("weather_country", "Select Country",
                                 choices = c("", sort(unique(world_cities$country))),
                                 options = list(placeholder = "Type or select a country...")),
                  uiOutput("city_selector")
                )
              ),
              fluidRow(
                box(
                  width = 12, status = "success", solidHeader = TRUE,
                  title = "🗺️ Click a City for Weather",
                  leafletOutput("weather_map", height = 550)
                )
              )
      ),
      
      # Airports tab
      tabItem(tabName = "airports",
              fluidRow(
                box(
                  title = "What is the Best Airport to Fly into?", status = "primary", solidHeader = TRUE,
                  width = 8,
                  plotOutput("percentage_on_time", height = 400)
                ),
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
                box(
                  title = "Where Are You Going?", status = "primary", solidHeader = TRUE,
                  selectizeInput("UNESCOCountry",
                                 label = "Select Your Destination",
                                 choices = sort(unique(UNESCO$Country)))
                )
              ),
              fluidRow(
                box(
                  title = "UNESCO World Heritage Sites to Visit", status = "success", solidHeader = TRUE,
                  width = 6,
                  uiOutput("sites_table")
                )
              )
      )
      
    )
  )
)