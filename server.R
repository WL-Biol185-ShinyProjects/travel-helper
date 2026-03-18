library(shiny)
library(ggplot2)
library(tidyverse)
library(readxl)
library(dplyr)
library(httr)
library(jsonlite)
library(leaflet)

# Data loading
UNESCO <- read_excel("UNESCO_World_Heritage_Sites.xlsx")
passport_info <- read.csv("passport-index-tidy.csv") 
currencyVcountry <- read.csv("currencyVcountry.csv")
adapter_data <- read.csv("travel_adapter_converter.csv")
vaccinationVcountry <- read.csv("vaccinationVcountry_correct.csv")
arrival_2025 <- read_excel("arrival information 2025.xlsx")
colnames(arrival_2025) <- c("rank", "airport", "pct_on_time")
arrival_2025$airport <- reorder(arrival_2025$airport, arrival_2025$pct_on_time)
unesco_coords <- read_excel("UNESCO_COORDS.xlsx")
unesco_coords <- unesco_coords[!is.na(unesco_coords$Latitude) & 
                                 !is.na(unesco_coords$Longitude), ]
airfare_data <- read.csv("Consumer_Airfare_Report__Table_1_-_Top_1,000_Contiguous_State_City-Pair_Markets_20260309.csv") %>%
  mutate(
    fare_low = as.numeric(gsub("[$,]", "", fare_low)),
    fare_lg  = as.numeric(gsub("[$,]", "", fare_lg)),
    fare     = as.numeric(gsub("[$,]", "", fare)),
    city1    = trimws(city1),
    city2    = trimws(city2)
  )

carrier_names <- c(
  "AA" = "American Airlines", "AS" = "Alaska Airlines", "B6" = "JetBlue Airways",
  "DL" = "Delta Air Lines", "F9" = "Frontier Airlines", "G4" = "Allegiant Air",
  "HA" = "Hawaiian Airlines", "NK" = "Spirit Airlines", "OO" = "SkyWest Airlines",
  "UA" = "United Airlines", "WN" = "Southwest Airlines", "YX" = "Republic Airways",
  "YV" = "Mesa Airlines", "QX" = "Horizon Air", "SY" = "Sun Country Airlines",
  "EV" = "ExpressJet", "CO" = "Continental Airlines", "DH" = "Independence Air",
  "FL" = "AirTran Airways", "HP" = "America West Airlines", "NW" = "Northwest Airlines",
  "TW" = "Trans World Airlines", "TZ" = "ATA Airlines", "US" = "US Airways",
  "VX" = "Virgin America", "3M" = "Silver Airways", "9N" = "Trans States Airlines",
  "A7" = "Air Midwest", "E9" = "Boston-Maine Airways", "FF" = "Tower Air",
  "J7" = "Valujet Airlines", "JI" = "Midway Airlines", "KP" = "Kiwi International Air Lines",
  "KW" = "Carnival Air Lines", "L4" = "Mountain Air Express", "MX" = "Mexicana",
  "N5" = "Nolinor Aviation", "N7" = "National Airlines", "NJ" = "Vanguard Airlines",
  "OE" = "Westair Airlines", "P9" = "Pro Air", "PN" = "Pan American Airways",
  "QQ" = "Reno Air", "RP" = "Chautauqua Airlines", "RU" = "Cape Air",
  "SM" = "Sunjet International", "SX" = "Skybus Airlines", "T3" = "Eastern Airways",
  "TB" = "USAir Shuttle", "U5" = "USA 3000 Airlines", "W7" = "Western Pacific Airlines",
  "W9" = "Aloha Airlines", "WV" = "Air South", "XP" = "Casino Express",
  "ZA" = "Access Air", "ZW" = "Air Wisconsin"
)

airfare_data <- airfare_data %>%
  mutate(
    carrier_lg  = recode(carrier_lg,  !!!carrier_names),
    carrier_low = recode(carrier_low, !!!carrier_names)
  )

capitals <- read_excel("Capitals.xlsx")
world_cities <- read_excel("worldcities.xlsx")
world_cities <- world_cities[!is.na(world_cities$population) & 
                               world_cities$population > 500000, ]

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

#loading in data by month
jan <- read.csv("NJAN_T_ONTIME_MARKETING.csv")
feb <- read.csv("NFEB_T_ONTIME_MARKETING.csv")
march <- read.csv("NMARCHT_ONTIME_MARKETING.csv")
april <- read.csv("APRIL_T_ONTIME_MARKETING.csv")
may <- read.csv("MAY_T_ONTIME_MARKETING.csv")
june <- read.csv("JUNE_T_ONTIME_MARKETING.csv")
july <- read.csv("JULY_T_ONTIME_MARKETING.csv")
aug <- read.csv("AUG_T_ONTIME_MARKETING.csv")
sept <- read.csv("SEPT_T_ONTIME_MARKETING.csv")
oct <- read.csv("OCT_T_ONTIME_MARKETING.csv")
nov <- read.csv("NOV_T_ONTIME_MARKETING.csv")
dec <- read.csv("DEC_T_ONTIME_MARKETING.csv")

#combining data
new <- rbind(jan, feb)
new <- rbind( new, march)
new <- rbind( new, april)
new <- rbind( new, may)
new <- rbind( new, june)
new <- rbind( new, july)
new <- rbind( new, aug)
new <- rbind( new, sept)
new <- rbind( new, oct)
new <- rbind( new, nov)
final_flights <- rbind(new, dec)

# Server
function(input, output, session) {
  
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
  output$vaccination_required <- renderText({
    vaccinationVcountry[vaccinationVcountry$country_vaccination == input$country_vaccination, "vaccination_required"]
  })
  
  # Adapter
  adapter_result <- reactive({
    req(input$origin_country, input$dest_country)
    adapter_data %>%
      filter(Origin.Country == input$origin_country,
             Destination.Country == input$dest_country)
  })
  
  output$adapter_needed <- renderText({
    req(nrow(adapter_result()) > 0)
    adapter_result()$Adapter.Needed
  })
  
  output$converter_needed <- renderText({
    req(nrow(adapter_result()) > 0)
    adapter_result()$Converter.Needed
  })
  
  output$adapter_rec <- renderText({
    req(nrow(adapter_result()) > 0)
    adapter_result()$Adapter.Recommendation
  })
  
  output$converter_rec <- renderText({
    req(nrow(adapter_result()) > 0)
    adapter_result()$Converter.Recommendation
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
  
  # UNESCO coords
  unesco_coords <- read_excel("UNESCO_COORDS.xlsx")
  unesco_coords <- unesco_coords[!is.na(unesco_coords$Latitude) & 
                                   !is.na(unesco_coords$Longitude), ]
  
  # UNESCO sites list
  selected_site <- reactiveVal(NULL)
  
  output$sites_table <- renderUI({
    req(input$UNESCOCountry)
    sites <- UNESCO[UNESCO$Country == input$UNESCOCountry, "World Heritage Site", drop = TRUE]
    
    tagList(
      h4(paste0(input$UNESCOCountry, " (", length(sites), " sites)")),
      tags$ul(
        lapply(sites, function(site) {
          tags$li(
            actionLink(
              inputId = paste0("site_", gsub("[^a-zA-Z0-9]", "_", site)),
              label = site
            )
          )
        })
      )
    )
  })
  
  # Observe each site click
  observe({
    req(input$UNESCOCountry)
    sites <- UNESCO[UNESCO$Country == input$UNESCOCountry, "World Heritage Site", drop = TRUE]
    
    lapply(sites, function(site) {
      id <- paste0("site_", gsub("[^a-zA-Z0-9]", "_", site))
      observeEvent(input[[id]], {
        selected_site(site)
      }, ignoreInit = TRUE)
    })
  })
  
  output$site_image <- renderUI({
    req(selected_site())
    
    url <- paste0(
      "https://en.wikipedia.org/api/rest_v1/page/summary/",
      URLencode(selected_site())
    )
    
    response <- tryCatch(GET(url), error = function(e) NULL)
    
    if (!is.null(response) && status_code(response) == 200) {
      data <- fromJSON(content(response, "text", encoding = "UTF-8"))
      img_url <- data$thumbnail$source
      description <- data$extract
      wiki_url <- data$content_urls$desktop$page
      
      tagList(
        br(),
        h4(selected_site()),
        if (!is.null(img_url)) {
          tags$img(src = img_url, width = "100%",
                   style = "border-radius: 8px; margin-bottom: 10px;")
        },
        p(description),
        tags$a(href = wiki_url, target = "_blank", "📖 Read more on Wikipedia")
      )
    } else {
      p("No image available for this site.")
    }
  })
  
  # UNESCO map - initial render
  output$unesco_map <- renderLeaflet({
    leaflet(unesco_coords) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = 0, lat = 20, zoom = 2) %>%
      addMarkers(
        lng = ~Longitude,
        lat = ~Latitude,
        layerId = ~`World Heritage Site`,
        label = ~`World Heritage Site`,
        clusterOptions = markerClusterOptions()
      )
  })
  
  # Zoom map when country selected
  observeEvent(input$UNESCOCountry, {
    req(input$UNESCOCountry)
    
    filtered <- unesco_coords[unesco_coords$Country == input$UNESCOCountry, ]
    
    if (nrow(filtered) > 0) {
      avg_lat <- mean(filtered$Latitude, na.rm = TRUE)
      avg_lng <- mean(filtered$Longitude, na.rm = TRUE)
      
      leafletProxy("unesco_map") %>%
        clearMarkers() %>%
        clearMarkerClusters() %>%
        clearPopups() %>%
        addMarkers(
          data = filtered,
          lng = ~Longitude,
          lat = ~Latitude,
          layerId = ~`World Heritage Site`,
          label = ~`World Heritage Site`,
          clusterOptions = markerClusterOptions()
        ) %>%
        setView(lng = avg_lng, lat = avg_lat, zoom = 5)
    }
  }, ignoreInit = TRUE)
  
  # When marker clicked fetch image and show popup inside map
  observeEvent(input$unesco_map_marker_click, {
    click <- input$unesco_map_marker_click
    req(click$id)
    
    site_name <- click$id
    lat <- click$lat
    lng <- click$lng
    
    country_row <- unesco_coords[unesco_coords$`World Heritage Site` == site_name, ]
    country <- as.character(country_row$Country[1])
    
    gmaps_url <- paste0("https://www.google.com/maps?q=", lat, ",", lng)
    
    wiki_url <- paste0(
      "https://en.wikipedia.org/api/rest_v1/page/summary/",
      URLencode(site_name)
    )
    
    img_tag <- ""
    desc_tag <- ""
    
    tryCatch({
      resp <- GET(wiki_url)
      if (status_code(resp) == 200) {
        wiki_data <- fromJSON(content(resp, "text", encoding = "UTF-8"))
        if (!is.null(wiki_data$thumbnail$source)) {
          img_tag <- paste0(
            "<img src='", wiki_data$thumbnail$source,
            "' width='260px' style='border-radius:6px; margin:6px 0; display:block'>"
          )
        }
        if (!is.null(wiki_data$extract)) {
          desc <- substr(wiki_data$extract, 1, 200)
          desc_tag <- paste0("<p style='font-size:11px; margin:6px 0'>", desc, "...</p>")
        }
      }
    }, error = function(e) {})
    
    popup_content <- paste0(
      "<div style='width:270px'>",
      img_tag,
      "<b style='font-size:13px'>", site_name, "</b><br>",
      "<span style='color:#666'>🌍 ", country, "</span><br>",
      desc_tag,
      "<a href='", gmaps_url, "' target='_blank'>📍 Open in Google Maps</a>&nbsp;&nbsp;",
      "<a href='https://en.wikipedia.org/wiki/", URLencode(site_name),
      "' target='_blank'>📖 Wikipedia</a>",
      "</div>"
    )
    
    leafletProxy("unesco_map") %>%
      clearPopups() %>%
      addPopups(
        lng = lng,
        lat = lat,
        popup = popup_content
      )
  })
  # Airfare - destination dropdown
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
  
  # Airfare - route search
  route_data <- eventReactive(input$search, {
    req(input$origin, input$dest)
    airfare_data %>%
      filter(city1 == input$origin, city2 == input$dest) %>%
      arrange(desc(Year), desc(quarter)) %>%
      slice(1)
  })
  
  # Airfare - display results
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
                 h4("💰 Cheapest Carrier"),
                 h2(df$carrier_low),
                 p(paste("Market share:", round(as.numeric(df$lf_ms) * 100, 1), "%")),
                 p(paste("Avg fare: $", df$fare_low))
               )
        )
      ),
      br(),
      p(paste("Distance:", df$nsmiles, "miles  |  Passengers:", df$passengers, " |  Data from: Q", df$quarter, df$Year))
    )
  })
  
  # Weather map - initial render
  output$weather_map <- renderLeaflet({
    leaflet(world_cities) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = 0, lat = 20, zoom = 2) %>%
      addMarkers(
        lng = ~lng,
        lat = ~lat,
        layerId = ~id,
        label = ~paste0(city, ", ", country),
        clusterOptions = markerClusterOptions()
      )
  })
  
  # Render city dropdown based on selected country
  output$city_selector <- renderUI({
    req(input$weather_country)
    cities <- world_cities[world_cities$country == input$weather_country, ]
    selectizeInput("weather_city", "Select City",
                   choices = c("", sort(unique(cities$city))),
                   options = list(placeholder = "Type or select a city..."))
  })
  
  # Fly to country when dropdown selected
  observeEvent(input$weather_country, {
    req(input$weather_country)
    country_cities <- world_cities[world_cities$country == input$weather_country, ]
    if (nrow(country_cities) > 0) {
      avg_lat <- mean(country_cities$lat, na.rm = TRUE)
      avg_lng <- mean(country_cities$lng, na.rm = TRUE)
      leafletProxy("weather_map") %>%
        setView(lng = avg_lng, lat = avg_lat, zoom = 5)
    }
  })
  
  # Fly to city when selected
  observeEvent(input$weather_city, {
    req(input$weather_city)
    row <- world_cities[world_cities$city == input$weather_city & 
                          world_cities$country == input$weather_country, ]
    if (nrow(row) > 0) {
      leafletProxy("weather_map") %>%
        setView(lng = row$lng[1], lat = row$lat[1], zoom = 8)
    }
  })
  
  # When a city marker is clicked, fetch and display weather
  observeEvent(input$weather_map_marker_click, {
    click <- input$weather_map_marker_click
    row <- world_cities[world_cities$id == click$id, ]
    lat <- row$lat
    lon <- row$lng
    city <- row$city
    country <- row$country
    
    url <- paste0(
      "https://api.open-meteo.com/v1/forecast?",
      "latitude=", lat,
      "&longitude=", lon,
      "&current_weather=true",
      "&daily=temperature_2m_max,temperature_2m_min,precipitation_sum,weathercode",
      "&temperature_unit=fahrenheit",
      "&precipitation_unit=inch",
      "&timezone=auto",
      "&forecast_days=7"
    )
    
    response <- GET(url)
    data <- fromJSON(content(response, "text", encoding = "UTF-8"))
    weather <- data$current_weather
    forecast <- as.data.frame(data$daily)
    
    weather_description <- case_when(
      weather$weathercode == 0        ~ "Clear sky ☀️",
      weather$weathercode %in% 1:3   ~ "Partly cloudy ⛅",
      weather$weathercode %in% 45:48 ~ "Foggy 🌫️",
      weather$weathercode %in% 51:67 ~ "Rainy 🌧️",
      weather$weathercode %in% 71:77 ~ "Snowy ❄️",
      weather$weathercode %in% 80:82 ~ "Rain showers 🌦️",
      weather$weathercode %in% 95:99 ~ "Thunderstorm ⛈️",
      TRUE ~ "Unknown"
    )
    
    forecast_rows <- paste0(
      sapply(1:nrow(forecast), function(i) {
        paste0(
          "<tr>",
          "<td style='padding:3px 8px'>", forecast$time[i], "</td>",
          "<td style='padding:3px 8px'>", forecast$temperature_2m_max[i], "°F / ",
          forecast$temperature_2m_min[i], "°F</td>",
          "<td style='padding:3px 8px'>", forecast$precipitation_sum[i], " in</td>",
          "</tr>"
        )
      }),
      collapse = ""
    )
    
    popup_text <- paste0(
      "<b style='font-size:14px'>", city, ", ", country, "</b><br><br>",
      "🌡️ <b>Now:</b> ", weather$temperature, "°F<br>",
      "💨 <b>Wind:</b> ", weather$windspeed, " km/h<br>",
      "🌤️ <b>Conditions:</b> ", weather_description, "<br><br>",
      "<b>7-Day Forecast</b><br>",
      "<table style='font-size:11px; border-collapse:collapse'>",
      "<tr style='background:#f0f0f0'>",
      "<th style='padding:3px 8px'>Date</th>",
      "<th style='padding:3px 8px'>High / Low</th>",
      "<th style='padding:3px 8px'>Precip</th>",
      "</tr>",
      forecast_rows,
      "</table>"
    )
    
    leafletProxy("weather_map") %>%
      addPopups(lng = lon, lat = lat, popup = popup_text)
  })
  
}