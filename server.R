library(shiny)
library(ggplot2)
library(tidyverse)
library(readxl)
library(dplyr)

# Data loading
UNESCO <- read_excel("UNESCO_World_Heritage_Sites.xlsx")
passport_info <- read.csv("passport-index-tidy.csv") 
currencyVcountry <- read.csv("currencyVcountry.csv")
vaccinationVcountry <- read.csv("vaccinationVcountry.csv")
colnames(vaccinationVcountry) <- c("Country", "Vaccination_required")
arrival_2025 <- read_excel("arrival information 2025.xlsx")
colnames(arrival_2025) <- c("rank", "airport", "pct_on_time")
arrival_2025$airport <- reorder(arrival_2025$airport, arrival_2025$pct_on_time)
airfare_data <- read.csv("Consumer_Airfare_Report__Table_1_-_Top_1,000_Contiguous_State_City-Pair_Markets_20260309.csv") %>%
  mutate(
    fare_low = as.numeric(gsub("[$,]", "", fare_low)),
    fare_lg  = as.numeric(gsub("[$,]", "", fare_lg)),
    fare     = as.numeric(gsub("[$,]", "", fare)),
    city1    = trimws(city1),
    city2    = trimws(city2)
  )
carrier_names <- c(
  "AA" = "American Airlines",
  "AS" = "Alaska Airlines",
  "B6" = "JetBlue Airways",
  "DL" = "Delta Air Lines",
  "F9" = "Frontier Airlines",
  "G4" = "Allegiant Air",
  "HA" = "Hawaiian Airlines",
  "NK" = "Spirit Airlines",
  "OO" = "SkyWest Airlines",
  "UA" = "United Airlines",
  "WN" = "Southwest Airlines",
  "YX" = "Republic Airways",
  "YV" = "Mesa Airlines",
  "QX" = "Horizon Air",
  "SY" = "Sun Country Airlines",
  "EV" = "ExpressJet",
  "CO" = "Continental Airlines",
  "DH" = "Independence Air",
  "FL" = "AirTran Airways",
  "HP" = "America West Airlines",
  "NW" = "Northwest Airlines",
  "TW" = "Trans World Airlines",
  "TZ" = "ATA Airlines",
  "US" = "US Airways",
  "VX" = "Virgin America",
  "3M" = "Silver Airways",
  "9N" = "Trans States Airlines",
  "A7" = "Air Midwest",
  "E9" = "Boston-Maine Airways",
  "FF" = "Tower Air",
  "J7" = "Valujet Airlines",
  "JI" = "Midway Airlines",
  "KP" = "Kiwi International Air Lines",
  "KW" = "Carnival Air Lines",
  "L4" = "Mountain Air Express",
  "MX" = "Mexicana",
  "N5" = "Nolinor Aviation",
  "N7" = "National Airlines",
  "NJ" = "Vanguard Airlines",
  "OE" = "Westair Airlines",
  "P9" = "Pro Air",
  "PN" = "Pan American Airways",
  "QQ" = "Reno Air",
  "RP" = "Chautauqua Airlines",
  "RU" = "Cape Air",
  "SM" = "Sunjet International",
  "SX" = "Skybus Airlines",
  "T3" = "Eastern Airways",
  "TB" = "USAir Shuttle",
  "U5" = "USA 3000 Airlines",
  "W7" = "Western Pacific Airlines",
  "W9" = "Aloha Airlines",
  "WV" = "Air South",
  "XP" = "Casino Express",
  "ZA" = "Access Air",
  "ZW" = "Air Wisconsin"
)

# Rename in the dataframe
airfare_data <- airfare_data %>%
  mutate(
    carrier_lg  = recode(carrier_lg,  !!!carrier_names),
    carrier_low = recode(carrier_low, !!!carrier_names)
  )

# Server
function(input, output) {
  
  # Passport requirement
  output$Requirement <- renderText({
    result <- passport_info %>% 
      filter(Passport == input$Passport, Destination == input$Destination) 
    if (nrow(result) > 0) {
      result$Requirement[1]
    } else {
      "No information available"
    }
  })
  
  # Currency
  output$Currency <- renderText({
    currencyVcountry[currencyVcountry$Country == input$Country, "Currency"]
  })
  
  # Vaccinations
  output$Vaccination_required <- renderText({
    vaccinationVcountry[vaccinationVcountry$Country == input$Country, "Vaccination_required"]
  })
  
  # Airports chart
  output$percentage_on_time <- renderPlot({
    ggplot(arrival_2025, aes(x = airport, y = pct_on_time, fill = pct_on_time)) +
      geom_col() +
      coord_flip() +
      scale_fill_gradient(low = "#f4a261", high = "#2a9d8f") +
      labs(
        title = "Airports Ranked by On-Time Arrival Percentage",
        x = "On-Time Percentage (%)",
        y = "Airport",
        fill = "% On-Time"
      ) +
      theme_minimal()
  })
  
  # UNESCO sites
  output$sites_table <- renderUI({
    req(input$UNESCOCountry)
    sites <- UNESCO[UNESCO$Country == input$UNESCOCountry, "World Heritage Site", drop = TRUE]
    tagList(
      h4(paste0(input$UNESCOCountry, " (", length(sites), " sites)")),
      tags$ul(lapply(sites, tags$li))
    )
  })
  
#Airling Pricing and Desitnation
  # Dynamically update destination choices based on selected origin
  output$dest_dropdown <- renderUI({
    req(input$origin)
    destinations <- airfare_data %>%
      filter(city1 == input$origin) %>%
      pull(city2) %>%
      unique() %>%
      sort()
    selectizeInput("dest",
                   label = "Destination City",
                   choices = destinations,
                   options = list(placeholder = "Select destination city..."))
  })
  
  # Filter data for selected route on button click
  route_data <- eventReactive(input$search, {
    req(input$origin, input$dest)
    airfare_data %>%
      filter(city1 == input$origin, city2 == input$dest) %>%
      arrange(desc(Year), desc(quarter)) %>%
      slice(1)  # take the most recent record
  })
  
  # Display results
  output$route_results <- renderUI({
    req(route_data())
    df <- route_data()
    
    validate(need(nrow(df) > 0, "No data found for this route."))
    
    tagList(
      h4(paste(df$city1, "→", df$city2)),
      br(),
      fluidRow(
        column(6,
               wellPanel(
                 h4("✈ Largest Carrier"),
                 h2(df$carrier_lg),
                 p(paste("Market share:", round(as.numeric(df$large_ms) * 100, 1), "%")),
                 p(paste("Avg fare: $", df$fare_lg))
               )
        ),
        column(6,
               wellPanel(
                 h4("Cheapest Carrier"),
                 h2(df$carrier_low),
                 p(paste("Market share:", round(as.numeric(df$lf_ms) * 100, 1), "%")),
                 p(paste("Avg fare: $", df$fare_low))
               )
        )
      ),
      br(),
      p(paste("Distance:", df$nsmiles, "miles  |  passengers:", df$passengers, " |  Data from: Q", df$quarter, df$Year))
    )
  })
}