library(shiny)
library(shinydashboard)
library(readxl)
library(tidyverse)
library(dplyr)
library(leaflet)

# --- Data loading ---
passport_info        <- read.csv("passport-index-tidy.csv")
currencyVcountry     <- read.csv("currencyVcountry.csv")
vaccinationVcountry  <- read.csv("vaccinationVcountry.csv")
colnames(vaccinationVcountry) <- c("Country", "Vaccination_required")  # FIX: rename columns in UI too


arrival_2025 <- read_excel("arrival information 2025.xlsx")
colnames(arrival_2025) <- c("rank", "airport", "pct_on_time")
arrival_2025$airport <- reorder(arrival_2025$airport, arrival_2025$pct_on_time)

# Data loading
passport_info <- read.csv("passport-index-tidy.csv")
currencyVcountry <- read.csv("currencyVcountry.csv")
vaccinationVcountry <- read.csv("vaccinationVcountry_correct.csv")
arrival_2025 <- read_excel("arrival information 2025.xlsx")
adapter_data <- read.csv("travel_adapter_converter.csv")
  colnames(arrival_2025) <- c("rank", "airport", "pct_on_time")
  arrival_2025$airport <- reorder(arrival_2025$airport, arrival_2025$pct_on_time)

UNESCO <- read_excel("UNESCO_World_Heritage_Sites.xlsx")

airfare_data <- read.csv("Consumer_Airfare_Report__Table_1_-_Top_1,000_Contiguous_State_City-Pair_Markets_20260309.csv") %>%
  mutate(
    fare_low = as.numeric(gsub("[$,]", "", fare_low)),
    fare_lg  = as.numeric(gsub("[$,]", "", fare_lg)),
    fare     = as.numeric(gsub("[$,]", "", fare)),
    city1    = trimws(city1),
    city2    = trimws(city2)
  )

carrier_names <- c(
  "AA" = "American Airlines",   "AS" = "Alaska Airlines",
  "B6" = "JetBlue Airways",     "DL" = "Delta Air Lines",
  "F9" = "Frontier Airlines",   "G4" = "Allegiant Air",
  "HA" = "Hawaiian Airlines",   "NK" = "Spirit Airlines",
  "OO" = "SkyWest Airlines",    "UA" = "United Airlines",
  "WN" = "Southwest Airlines",  "YX" = "Republic Airways",
  "YV" = "Mesa Airlines",       "QX" = "Horizon Air",
  "SY" = "Sun Country Airlines","EV" = "ExpressJet",
  "CO" = "Continental Airlines","DH" = "Independence Air",
  "FL" = "AirTran Airways",     "HP" = "America West Airlines",
  "NW" = "Northwest Airlines",  "TW" = "Trans World Airlines",
  "TZ" = "ATA Airlines",        "US" = "US Airways",
  "VX" = "Virgin America",      "3M" = "Silver Airways",
  "9N" = "Trans States Airlines","A7" = "Air Midwest",
  "E9" = "Boston-Maine Airways","FF" = "Tower Air",
  "J7" = "Valujet Airlines",    "JI" = "Midway Airlines",
  "KP" = "Kiwi International",  "KW" = "Carnival Air Lines",
  "L4" = "Mountain Air Express","MX" = "Mexicana",
  "N5" = "Nolinor Aviation",    "N7" = "National Airlines",
  "NJ" = "Vanguard Airlines",   "OE" = "Westair Airlines",
  "P9" = "Pro Air",             "PN" = "Pan American Airways",
  "QQ" = "Reno Air",            "RP" = "Chautauqua Airlines",
  "RU" = "Cape Air",            "SM" = "Sunjet International",
  "SX" = "Skybus Airlines",     "T3" = "Eastern Airways",
  "TB" = "USAir Shuttle",       "U5" = "USA 3000 Airlines",
  "W7" = "Western Pacific Airlines","W9" = "Aloha Airlines",
  "WV" = "Air South",           "XP" = "Casino Express",
  "ZA" = "Access Air",          "ZW" = "Air Wisconsin"
)

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


world_cities <- read_excel("worldcities.xlsx")
world_cities <- world_cities[!is.na(world_cities$population) & 
                               world_cities$population > 500000, ]
# Rename in the dataframe
airfare_data <- airfare_data %>%
  mutate(
    carrier_lg  = recode(carrier_lg,  !!!carrier_names),
    carrier_low = recode(carrier_low, !!!carrier_names)
  )

world_cities <- read_excel("worldcities.xlsx")
world_cities <- world_cities[!is.na(world_cities$population) &
                               world_cities$population > 500000, ]

# --- UI ---
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

      # --- Welcome tab ---
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

      # --- International Travel tab ---
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
                  title = "Currency", status = "info", solidHeader = TRUE,
                  # FIX: renamed from "Country" to "currency_country"
                  selectizeInput("currency_country",
                  title = "Currency Info", status = "primary", solidHeader = TRUE,
                  selectizeInput("Country", 
                                 label = "Destination Country",
                                 choices = (currencyVcountry$Country)
                  ),
                  h4("Currency"), 
                  verbatimTextOutput("Currency")
                ),
                
                box(
                  title = "Vaccinations", status = "warning", solidHeader = TRUE,
                  # FIX: renamed from "Country" to "vaccination_country"
                  selectizeInput("vaccination_country",
                                 label = "Destination Country",
                                 choices = vaccinationVcountry$Country),
                  h4("Vaccination Required"),
                  # FIX: removed space from output ID
                  verbatimTextOutput("Vaccination_required"),
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
      # --- Weather tab ---

      
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
                # FIX: added width = 4 to empty box
                box(width = 4),
                  plotOutput("percentage_on_time", height = 400)),
                box()
              )
      ),

      # --- Airlines tab ---
      tabItem(tabName = "airlines",
              fluidRow(
                box(),
                box()
              )
      ),

      # --- Pricing tab ---
      tabItem(tabName = "pricing",
              fluidRow(
                box(width = 4,
                    title = "Search a Route",
                    status = "primary",
                    solidHeader = TRUE,
                    selectizeInput("origin",
                                   label = "Departure City",
                                   choices = sort(unique(airfare_data$city1)),
                                   options = list(placeholder = "Select departure city...")),
                    uiOutput("dest_dropdown"),
                    actionButton("search", "Search",
                                 class = "btn-primary", width = "100%")
                ),
                box(width = 8,
                    title = "Route Information",
                    status = "primary",
                    solidHeader = TRUE,
                    uiOutput("route_results"))
              )
      ),

      # --- Travel Suggestions tab ---
      tabItem(tabName = "travel_suggestions",
              fluidRow(
                box(
                  title = "Where Are You Going?", status = "primary", solidHeader = TRUE,
                  selectizeInput("UNESCOCountry",
                                 label = "Select Your Destination",
                                 choices = sort(unique(UNESCO$Country)))
                ),
                box(   title = "UNESCO World Heritage Sites to Visit", status = "success", solidHeader = TRUE,
                       width = 6,
                       uiOutput("sites_table")
                )
              )
      )

#last three parentheses green, orange, pink
    )
  )
  )
