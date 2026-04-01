library(shiny)
library(shinydashboard)
library(readxl)
library(tidyverse)
library(dplyr)
library(leaflet)
library(scales)
library(lubridate)
library(shinyjs)


# Data loading
airports_busy <- read.csv("airports_busy.csv")
passport_info <- read.csv("passport-index-tidy.csv")
currencyVcountry <- read.csv("currencyVcountry.csv")
passport_info      <- read.csv("passport-index-tidy.csv")
currencyVcountry   <- read.csv("currencyVcountry.csv")
vaccinationVcountry <- read.csv("vaccinationVcountry_correct.csv")
adapter_data       <- read.csv("travel_adapter_converter.csv")
passport_info <- read.csv("passport-index-tidy.csv")
currencyVcountry <- read.csv("currencyVcountry.csv")
vaccinationVcountry <- read.csv("vaccinationVcountry_correct.csv")
arrival_2025 <- read_excel("arrival information 2025.xlsx")
arrival_2025 <- arrival_2025 %>% mutate(across(where(is.list), as.character))
adapter_data <- read.csv("travel_adapter_converter.csv")
colnames(arrival_2025) <- c("rank", "airport", "pct_on_time")
arrival_2025$airport <- reorder(arrival_2025$airport, arrival_2025$pct_on_time)
UNESCO <- read_excel("UNESCO_World_Heritage_Sites.xlsx")
UNESCO <- UNESCO %>% mutate(across(where(is.list), as.character))
unesco_coords <- read_excel("UNESCO_COORDS.xlsx")
unesco_coords <- unesco_coords[!is.na(unesco_coords$Latitude) & 
                                 !is.na(unesco_coords$Longitude), ]
airfare_data <- read.csv("airfare_data.csv") %>%
  mutate(
    fare_low = as.numeric(gsub("[$,]", "", fare_low)),
    fare_lg  = as.numeric(gsub("[$,]", "", fare_lg)),
    fare     = as.numeric(gsub("[$,]", "", fare)),
    city1    = trimws(city1),
    city2    = trimws(city2)
  )
travel_quiz <- read.csv("Worldwide_Travel_Cities_Dataset.csv")

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
  "90" = "90 Days Visa Free", "30" = "30 Days Visa Free", "60" = "60 Days Visa Free",
  "360" = "360 Days Visa Free", "21" = "21 Days Visa Free", "28" = "28 Days Visa Free",
  "19" = "19 Days Visa Free", "180" = "180 Days Visa Free", "14" = "14 Days Visa Free",
  "42" = "42 Days Visa Free", "15" = "15 Days Visa Free", "240" = "240 Days Visa Free",
  "120" = "120 Days Visa Free", "eta" = "Electronic Travel Authorization",
  "e-visa" = "Electronic Visa Needed", "visa required" = "Visa Required",
  "visa on arrival" = "Visa on Arrival", "visa free" = "Visa Free",
  "-1" = "In-Country, No Visa Needed"
)
passport_info$Requirement <- recode(passport_info$Requirement, !!!v)

world_cities <- read_excel("worldcities.xlsx")
world_cities <- world_cities[!is.na(world_cities$population) &
                               world_cities$population > 500000, ]
world_cities <- world_cities %>% 
  mutate(across(where(is.list), as.character))

airfare_data <- airfare_data %>%
  mutate(
    carrier_lg  = recode(carrier_lg,  !!!carrier_names),
    carrier_low = recode(carrier_low, !!!carrier_names)
  )

final_flights <- readRDS("final_flights.rds")

carrier_names_flights <- c(
  "WN" = "Southwest Airlines",
  "DL" = "Delta Air Lines",
  "AA" = "American Airlines",
  "OO" = "SkyWest Airlines",
  "UA" = "United Airlines",
  "YX" = "Midwest Airlines",
  "B6" = "JetBlue Airways",
  "MQ" = "Envoy Air",
  "NK" = "Spirit Airlines",
  "AS" = "Alaska Airlines",
  "OH" = "Comair",
  "9E" = "Endeavor Air",
  "F9" = "Frontier Airlines",
  "G4" = "Allegiant Air",
  "PT" = "Piedmont Airlines",
  "YV" = "Mesa Airlines",
  "QX" = "Horizon Air",
  "HA" = "Hawaiian Airlines",
  "C5" = "CommuteAir",
  "G7" = "GoJet Airlines",
  "ZW" = "Air Wisconsin"
)

final_flights <- final_flights %>%
  mutate(MKT_UNIQUE_CARRIER = carrier_names_flights[MKT_UNIQUE_CARRIER])

# --- UI ---

# Sidebar content


dashboardPage(
  dashboardHeader(title = "Travel Helper"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Welcome",              tabName = "welcome"),
      menuItem("International Travel", tabName = "international_travel"),
      menuItem("Weather",              tabName = "weather"),
      menuItem("Airports",             tabName = "airports"),
      menuItem("Airlines",             tabName = "airlines"),
      menuItem("Pricing and Statistics",              tabName = "pricing"),
      menuItem("Travel Suggestions",   tabName = "travel_suggestions"),
      menuItem("Where Should I Go?",         tabName = "travel_quiz"),
      menuItem("About Us", tabName="about")

 

    )
  ),
  
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
                    tags$li("💰 Pricing and Statistics — Compare travel pricing and travel statistics"),
                    tags$li("🗺️ Travel Suggestions — Discover UNESCO World Heritage Sites to visit")
                  )
                ),
                
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
                                 choices = unique(passport_info$Passport)),
                  selectizeInput("Destination",
                                 label = "Your Destination",
                                 choices = unique(passport_info$Destination)),
                  h4("Requirement"),
                  verbatimTextOutput("Requirement")
                ),
                box(
                  title = "Currency Info", status = "primary", solidHeader = TRUE,
                  selectizeInput("Country",
                                 label = "Destination Country",
                                 choices = currencyVcountry$Country),
                  h4("Currency"),
                  verbatimTextOutput("Currency")
                ),
                box(
                  title = "Vaccination Needed", status = "primary", solidHeader = TRUE,
                  selectizeInput("country_vaccination",
                                 label = "Country of Destination",
                                 choices = vaccinationVcountry$country_vaccination),
                  h4("Vaccination Required"),
                  verbatimTextOutput("vaccination_required")
                ),
                box(
                  title = "Electrical Adapter & Converter Requirements", status = "primary", solidHeader = TRUE,
                  selectizeInput("origin_country",
                                 label = "Country of Origin",
                                 choices = sort(unique(adapter_data$Origin.Country)),
                                 options = list(placeholder = "Select your home country...")),
                  selectizeInput("dest_country",
                                 label = "Destination Country",
                                 choices = sort(unique(adapter_data$Destination.Country)),
                                 options = list(placeholder = "Select your destination...")),
                  h4("Adapter Needed"),
                  verbatimTextOutput("adapter_needed"),
                  h4("Converter Needed"),
                  verbatimTextOutput("converter_needed"),
                  h4("Adapter Recommendation"),
                  verbatimTextOutput("adapter_rec"),
                  h4("Converter Recommendation"),
                  verbatimTextOutput("converter_rec")
                )
              ),
              fluidRow(
                box(
                  title = "🛡️ U.S. Travel Advisory", status = "danger", solidHeader = TRUE,
                  width = 12,
                  p("Travel advisory information from the U.S. Department of State. Select a country to see the current advisory level."),
                  selectizeInput("advisory_country",
                                 label = "Select Destination Country",
                                 choices = sort(c(
                                   "Afghanistan", "Albania", "Algeria", "Andorra", "Angola",
                                   "Antigua and Barbuda", "Argentina", "Armenia", "Australia",
                                   "Austria", "Azerbaijan", "Bahamas", "Bahrain", "Bangladesh",
                                   "Barbados", "Belarus", "Belgium", "Belize", "Benin", "Bhutan",
                                   "Bolivia", "Bosnia and Herzegovina", "Botswana", "Brazil",
                                   "Brunei", "Bulgaria", "Burkina Faso", "Burma", "Burundi",
                                   "Cabo Verde", "Cambodia", "Cameroon", "Canada",
                                   "Central African Republic", "Chad", "Chile", "China",
                                   "Colombia", "Comoros", "Congo (Brazzaville)", "Congo (Kinshasa)",
                                   "Costa Rica", "Cote d'Ivoire", "Croatia", "Cuba", "Cyprus",
                                   "Czech Republic", "Denmark", "Djibouti", "Dominica",
                                   "Dominican Republic", "Ecuador", "Egypt", "El Salvador",
                                   "Equatorial Guinea", "Eritrea", "Estonia", "Eswatini",
                                   "Ethiopia", "Fiji", "Finland", "France", "Gabon", "Gambia",
                                   "Georgia", "Germany", "Ghana", "Greece", "Grenada", "Guatemala",
                                   "Guinea", "Guinea-Bissau", "Guyana", "Haiti", "Honduras",
                                   "Hungary", "Iceland", "India", "Indonesia", "Iran", "Iraq",
                                   "Ireland", "Israel", "Italy", "Jamaica", "Japan", "Jordan",
                                   "Kazakhstan", "Kenya", "Kiribati", "Kosovo", "Kuwait",
                                   "Kyrgyzstan", "Laos", "Latvia", "Lebanon", "Lesotho", "Liberia",
                                   "Libya", "Liechtenstein", "Lithuania", "Luxembourg",
                                   "Madagascar", "Malawi", "Malaysia", "Maldives", "Mali", "Malta",
                                   "Marshall Islands", "Mauritania", "Mauritius", "Mexico",
                                   "Micronesia", "Moldova", "Monaco", "Mongolia", "Montenegro",
                                   "Morocco", "Mozambique", "Namibia", "Nauru", "Nepal",
                                   "Netherlands", "New Zealand", "Nicaragua", "Niger", "Nigeria",
                                   "North Korea", "North Macedonia", "Norway", "Oman", "Pakistan",
                                   "Palau", "Panama", "Papua New Guinea", "Paraguay", "Peru",
                                   "Philippines", "Poland", "Portugal", "Qatar", "Romania",
                                   "Russia", "Rwanda", "Saint Kitts and Nevis", "Saint Lucia",
                                   "Saint Vincent and the Grenadines", "Samoa", "San Marino",
                                   "Sao Tome and Principe", "Saudi Arabia", "Senegal", "Serbia",
                                   "Seychelles", "Sierra Leone", "Singapore", "Slovakia",
                                   "Slovenia", "Solomon Islands", "Somalia", "South Africa",
                                   "South Korea", "South Sudan", "Spain", "Sri Lanka", "Sudan",
                                   "Suriname", "Sweden", "Switzerland", "Syria", "Taiwan",
                                   "Tajikistan", "Tanzania", "Thailand", "Timor-Leste", "Togo",
                                   "Tonga", "Trinidad and Tobago", "Tunisia", "Turkey",
                                   "Turkmenistan", "Tuvalu", "Uganda", "Ukraine",
                                   "United Arab Emirates", "United Kingdom", "Uruguay",
                                   "Uzbekistan", "Vanuatu", "Venezuela", "Vietnam", "Yemen",
                                   "Zambia", "Zimbabwe"
                                 )),
                                 options = list(placeholder = "Type or select a country...")),
                  br(),
                  uiOutput("advisory_output")
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
                ),
                
                box(
                  width = 4, status = "info", solidHeader = TRUE,
                  title = "Jump to a Location",
                  selectizeInput("weather_country", "Select Country",
                                 choices = c("", sort(unique(as.character(world_cities$country)))),
                                 options = list(placeholder = "Type or select a country...")),
                  uiOutput("city_selector")
                ),
                
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
                  width = 12,
                  selectizeInput(
                    inputId = "airport_question",
                    label = "What would you like to explore?",
                    choices = c(
                      "What is the Best Airport to Fly into?" = "on_time",
                      "Which airport had the most cancellations in 2025?" = "cancellations",
                      "What are the top 10 busiest airports in the U.S.?" = "busiest"   # NEW
                    ),
                    options = list(placeholder = "Select a question...")
                  )
                )
              ),
              fluidRow(
                box(
                  width = 12,
                  # Switches between plot and table depending on selection
                  conditionalPanel(
                    condition = "input.airport_question != 'busiest'",
                    plotOutput("airport_plot", height = 400)
                  ),
                  conditionalPanel(
                    condition = "input.airport_question == 'busiest'",
                    tableOutput("busiest_table")          # NEW
                  )
                )
              )
      ),
      
      # --- Airlines tab ---
      tabItem(tabName = "airlines",
              fluidRow(
                box(
                  width = 12,
                  selectizeInput(
                    inputId = "airline_question",
                    label = "What would you like to explore?",
                    choices = c(
                      "Which airlines delay flights the most?" = "delays",
                      "Which airlines cancel flights the most?" = "cancellations",
                      "Which airlines have the most flights?" = "most_flights"  # NEW
                    ),
                    options = list(placeholder = "Select a question...")
                  )
                )
              ),
              fluidRow(
                box(
                  width = 12,
                  conditionalPanel(
                    condition = "input.airline_question != 'most_flights'",
                    plotOutput("airline_plot", height = 400)
                  ),
                  conditionalPanel(
                    condition = "input.airline_question == 'most_flights'",
                    tableOutput("airline_table")                                # NEW
                  )
                )
              )
      ),
      
      # --- Pricing and stats tab ---
      tabItem(tabName = "pricing",
              fluidRow(
                box(width = 4,
                    title = "Search a Route",
                    status = "primary",
                    solidHeader = TRUE,
                    selectizeInput("origin",
                                   label = "Departure City",
                                   choices = sort(unique(as.character(airfare_data$city1))),
                                   options = list(placeholder = "Select departure city...")),
                    uiOutput("dest_dropdown"),
                    actionButton("search", "Search",
                                 class = "btn-primary", width = "100%")
                ),
                box(width = 8,
                    title = "Route Information",
                    status = "primary",
                    solidHeader = TRUE,
                    uiOutput("route_results")
                ), 
                box(
                  plotOutput("busiest_days_plot"),
                  p("💡 The busiest travel day was the Sunday after Thanksgiving, as millions of Americans returned home from the holiday weekend!",
                    style = "color: #666666; font-size: 13px; margin-top: 8px; font-style: italic;")
                )
              )
      ),
      
      # Travel Suggestions tab
      tabItem(tabName = "travel_suggestions",
              fluidRow(
                box(
                  width = 12, status = "primary", solidHeader = TRUE,
                  title = "🗺️ Travel Suggestions",
                  p("Discover UNESCO World Heritage Sites around the world!"),
                  tags$ul(
                    tags$li("1️⃣ Select a country from the dropdown below"),
                    tags$li("2️⃣ Browse the list of UNESCO World Heritage Sites for that country"),
                    tags$li("3️⃣ Click any site name to see a photo and description"),
                    tags$li("4️⃣ Click the Wikipedia link to learn even more!"),
                    tags$li("5️⃣ Explore the map below — click any marker for details and photos!")
                  )
                )
              ),
              fluidRow(
                box(
                  title = "Where Are You Going?", status = "primary", solidHeader = TRUE,
                  selectizeInput("UNESCOCountry",
                                 label = "Select Your Destination",
                                 choices = sort(unique(UNESCO$Country)),
                                 options = list(placeholder = "Type or select a country..."))
                ),
                box(
                  title = "UNESCO World Heritage Sites to Visit", status = "success", solidHeader = TRUE,
                  width = 6,
                  uiOutput("sites_table")
                )
              ),
              fluidRow(
                box(
                  title = "🌍 UNESCO Sites Map", status = "warning", solidHeader = TRUE,
                  width = 6,
                  p("Click any marker to see site details and photos. Selecting a country above zooms the map to that country."),
                  leafletOutput("unesco_map", height = 500)
                ),
                box(
                  title = "Site Details", status = "info", solidHeader = TRUE,
                  width = 6,
                  p("👆 Click a site name above to see details and photos here."),
                  uiOutput("site_image")
                )
              ),
              fluidRow(
                box(
                  width = 4, status = "primary", solidHeader = TRUE,
                  title = "🧳 Packing List Generator",
                  p("Select your destination country above, then enter your travel dates to generate a packing list."),
                  dateInput("pack_depart", "Departure Date", value = Sys.Date()),
                  dateInput("pack_return", "Return Date",    value = Sys.Date() + 7),
                  actionButton("generate_list", "Generate Packing List 🧳",
                               class = "btn-primary", width = "100%")
                ),
                box(
                  width = 8, status = "success", solidHeader = TRUE,
                  title = "Your Packing List",
                  uiOutput("packing_list_output")
                )
              )

      ),

      tabItem(tabName="about",
              fluidRow(
                box(
                  width = 12, status = "primary", solidHeader = TRUE,
                  title = "Our Mission",
                  p(style = "font-size:16px; line-height:1.8;",
                    "Travel Helper was built to make international travel planning simple, 
       informed, and stress-free. We believe that access to clear, reliable 
       travel information should be available to everyone — whether you're 
       a first-time traveler or a seasoned explorer."
                  )
                )
              ),
              fluidRow(
                box(
                  width = 4, status = "info", solidHeader = TRUE,
                  title = "Lily Caldwell '27",
                  tags$img(
                    src   = "IMG_0464.JPG",
                    width = "60%",
                    style = "border-radius: 8px; margin-bottom: 10px;"
                  ),
                  p("Lily is a Neuroscience Major. Her favorite vacation was travelling to Cape Coast, Ghana!"),
                  p(style = "color:#999; font-size:12px;", "lcaldwell@mail.wlu.edu")
                ),
                box(
                  width = 4, status = "info", solidHeader = TRUE,
                  title = "Mac Palmer '27",
                  tags$img(
                    src   = "14CA98F3-1F2B-48A7-A1FE-09BB5E27DD4B.jpeg",
                    width = "60%",
                    style = "border-radius: 8px; margin-bottom: 10px;"
                  ),
                  p("Mac is an Integrated Engineering with Biology Major. His favorite vacation was going to Punta Cana, Dominican Republic!"),
                  p(style = "color:#999; font-size:12px;", "mpalmer@mail.wlu.edu")
                ),
                box(
                  width = 4, status = "info", solidHeader = TRUE,
                  title = "Maddie Montez '27",
                  tags$img(
                    src   = "IMG_0473.JPG",
                    width = "60%",
                    style = "border-radius: 8px; margin-bottom: 10px;"
                  ),
                  p("Maddie is a Neuroscience Major. Her favorite vacation was visiting Barcelona, Spain!"),
                  p(style = "color:#999; font-size:12px;", "mmontez@mail.wlu.edu")
                )
              )
              ),

      tabItem(tabName = "travel_quiz",
              fluidPage(tags$head(tags$style(HTML("
    :root {
      --teal:  #2a9d8f;
      --coral: #e76f51;
      --navy:  #264653;
      --sand:  #e9c46a;
      --light: #f8f9fa;
      --muted: #6c757d;
    }
    body { background: var(--light); font-family: 'Segoe UI', sans-serif; }

    /* Header */
    .app-header {
      background: linear-gradient(135deg, var(--navy) 0%, var(--teal) 100%);
      color: white; padding: 28px 32px 20px;
      margin-bottom: 28px; border-radius: 0 0 16px 16px;
      box-shadow: 0 4px 12px rgba(0,0,0,.18);
    }
    .app-header h1 { font-size: 2rem; margin: 0 0 6px; font-weight: 700; }
    .app-header p  { margin: 0; opacity: .85; font-size: 1rem; }

    /* Quiz cards */
    .quiz-card {
      background: white; border-radius: 14px;
      padding: 20px 24px; margin-bottom: 18px;
      box-shadow: 0 2px 8px rgba(0,0,0,.07);
      border-left: 5px solid var(--teal);
    }
    .quiz-card h4 {
      color: var(--navy); font-weight: 700;
      margin: 0 0 14px; font-size: 1rem;
    }

    /* Sliders */
    .irs--shiny .irs-bar,
    .irs--shiny .irs-bar-edge { background: var(--teal); border-color: var(--teal); }
    .irs--shiny .irs-handle    { background: var(--coral); border-color: var(--coral); }
    .irs--shiny .irs-from,
    .irs--shiny .irs-to,
    .irs--shiny .irs-single    { background: var(--navy); }

    /* Go button */
    #go_btn {
      background: linear-gradient(135deg, var(--coral), #c1440e);
      color: white; border: none; border-radius: 10px;
      padding: 14px 40px; font-size: 1.1rem; font-weight: 700;
      width: 100%; cursor: pointer;
      box-shadow: 0 4px 10px rgba(231,111,81,.35);
      transition: opacity .2s;
    }
    #go_btn:hover { opacity: .88; }

    /* Result cards */
    .result-card {
      background: white; border-radius: 14px;
      padding: 18px 22px; margin-bottom: 14px;
      box-shadow: 0 2px 8px rgba(0,0,0,.08);
      border-left: 6px solid var(--teal);
    }
    .result-rank   { font-size: 1.8rem; font-weight: 800; color: var(--coral);
                     line-height: 1; margin-right: 14px; }
    .result-city   { font-size: 1.2rem; font-weight: 700; color: var(--navy); }
    .result-country{ font-size: .88rem; color: var(--muted); margin-bottom: 6px; }
    .result-desc   { font-size: .92rem; color: #444; margin-bottom: 10px; }
    .badge-pill {
      display: inline-block; background: #e8f5f3; color: var(--teal);
      border-radius: 20px; padding: 3px 11px; font-size: .82rem;
      font-weight: 600; margin: 2px 3px 2px 0; border: 1px solid #b8e0da;
    }
    .score-bar-wrap { background: #eee; border-radius: 6px; height: 9px;
                      margin-top: 8px; overflow: hidden; }
    .score-bar { height: 100%; border-radius: 6px;
                 background: linear-gradient(90deg, var(--teal), var(--sand)); }

    /* Empty states */
    .empty-state { text-align: center; padding: 70px 20px; color: var(--muted); }
    .empty-state .icon { font-size: 3.5rem; }

    label { color: var(--navy); font-weight: 600; }
    .nav-tabs > li > a       { color: var(--navy); font-weight: 600; }
    .nav-tabs > li.active > a{ color: var(--coral);
                                border-bottom: 3px solid var(--coral); }
  "))),
                        
                        # ── Header ────────────────────────────────────────────────────────────────
                        div(class = "app-header",
                            h1("✈️ Travel Destination Quiz"),
                            p("Tell us what you're after and we'll match you with your perfect city.")
                        ),
                        
                        fluidRow(
                          
                          # ── LEFT: inputs ─────────────────────────────────────────────────────────
                          column(4,
                                 
                                 # Region
                                 div(class = "quiz-card",
                                     h4("🌍 Preferred region"),
                                     selectInput("region", label = NULL,
                                                 choices = c("No preference",
                                                             "Africa", "Asia", "Europe", "Middle East",
                                                             "North America", "Oceania", "South America"),
                                                 selected = "No preference")
                                 ),
                                 
                                 # Temperature
                                 div(class = "quiz-card",
                                     h4("🌡️ Preferred temperature (°C)"),
                                     sliderInput("temp_range", label = NULL,
                                                 min = -5, max = 35, value = c(15, 28), step = 1, post = "°C"),
                                     checkboxInput("temp_any", "I don't mind the temperature", value = FALSE)
                                 ),
                                 
                                 # Trip length
                                 div(class = "quiz-card",
                                     h4("🗓️ Ideal trip length"),
                                     selectInput("duration", label = NULL,
                                                 choices  = c("No preference", "Weekend", "Short trip",
                                                              "One week", "Long trip"),
                                                 selected = "No preference")
                                 ),
                                 
                                 # Budget
                                 div(class = "quiz-card",
                                     h4("💰 Budget level"),
                                     radioButtons("budget", label = NULL,
                                                  choices  = c("No preference", "Budget", "Mid-range", "Luxury"),
                                                  selected = "No preference")
                                 ),
                                 
                                 # Vibe sliders
                                 div(class = "quiz-card",
                                     h4("🎛️ What matters most to you?"),
                                     p(style = "color:#666; font-size:.87rem; margin-bottom:14px;",
                                       "1 = not important → 5 = must-have"),
                                     sliderInput("vibe_culture",   "🎭 Culture",    1, 5, 3, step = 1, ticks = TRUE, width = "100%"),
                                     sliderInput("vibe_adventure", "🧗 Adventure",  1, 5, 3, step = 1, ticks = TRUE, width = "100%"),
                                     sliderInput("vibe_nature",    "🌿 Nature",     1, 5, 3, step = 1, ticks = TRUE, width = "100%"),
                                     sliderInput("vibe_beaches",   "🏖️ Beaches",   1, 5, 3, step = 1, ticks = TRUE, width = "100%"),
                                     sliderInput("vibe_nightlife", "🎉 Nightlife",  1, 5, 3, step = 1, ticks = TRUE, width = "100%"),
                                     sliderInput("vibe_cuisine",   "🍽️ Cuisine",   1, 5, 3, step = 1, ticks = TRUE, width = "100%"),
                                     sliderInput("vibe_wellness",  "🧘 Wellness",   1, 5, 3, step = 1, ticks = TRUE, width = "100%"),
                                     sliderInput("vibe_urban",     "🏙️ Urban",     1, 5, 3, step = 1, ticks = TRUE, width = "100%"),
                                     sliderInput("vibe_seclusion", "🌄 Seclusion",  1, 5, 3, step = 1, ticks = TRUE, width = "100%")
                                 ),
                                 
                                 actionButton("go_btn", "Find My Destinations 🚀")
                          ),
                          
                          # ── RIGHT: results ────────────────────────────────────────────────────────
                          column(8,
                                 uiOutput("results_panel")
                          )
                        )
                        
              )
              
      ),
      
      tabItem(tabName="about",
              fluidRow(
                box(title = "Our Mission", status = "primary", solidHeader = TRUE, width = 6,
                    p("We all know planning a trip can be a hassle. Our mission is to help travelers, experienced and new, plan the perfect trip and reduce stress!")),
                box(title = "The Team", status = "warning", solidHeader = TRUE, width = 6,
                    tags$ul(
                      tags$li(strong("Lily Caldwell:"), " W&L '27"),
                      tags$li(strong("Mac Palmer:"), " W&L '27"),
                      tags$li(strong("Maddie Montez:"), " W&L '27")
                    )
                    )
              )
              

      )
     
)
)
)
