library(shiny)
library(shinydashboard)
library(ggplot2)
library(tidyverse)
library(readxl)
library(dplyr)
library(httr)
library(jsonlite)
library(leaflet)
library(scales)
library(lubridate)
library(shinyjs)

# Data loading
UNESCO <- read_excel("UNESCO_World_Heritage_Sites.xlsx")
UNESCO <- UNESCO %>% mutate(across(where(is.list), as.character))
passport_info <- read.csv("passport-index-tidy.csv") 
currencyVcountry <- read.csv("currencyVcountry.csv")
adapter_data <- read.csv("travel_adapter_converter.csv")
vaccinationVcountry <- read.csv("vaccinationVcountry_correct.csv")
arrival_2025 <- read_excel("arrival information 2025.xlsx")
arrival_2025 <- arrival_2025 %>% mutate(across(where(is.list), as.character))
colnames(arrival_2025) <- c("rank", "airport", "pct_on_time")
arrival_2025$airport <- reorder(arrival_2025$airport, arrival_2025$pct_on_time)
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
world_cities <- world_cities %>% mutate(across(where(is.list), as.character))
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


# Convert to POSIXct (today's date + time)
#numeric_to_time <- function(x) {
 # hours   <- x %/% 100
 # minutes <- x %%  100
 # as.POSIXct(sprintf("%02d:%02d", hours, minutes), format = "%H:%M")
#}
#dep_times <- numeric_to_time(final_flights$CRS_DEP_TIME)
#del_times <- numeric_to_time(final_flights$DEP_TIME)

# ── Parse JSON columns once at startup ────────────────────────────────────────
# avg_temp_monthly  → annual average temperature (numeric)
# ideal_durations   → list of duration strings

travel_quiz$annual_avg_temp <- sapply(travel_quiz$avg_temp_monthly, function(x) {
  tryCatch({
    parsed <- fromJSON(x)
    mean(sapply(parsed, function(m) m$avg), na.rm = TRUE)
  }, error = function(e) NA_real_)
})

travel_quiz$durations_list <- lapply(travel_quiz$ideal_durations, function(x) {
  tryCatch(fromJSON(x), error = function(e) character(0))
})

# Clean region labels for display
travel_quiz$region_label <- dplyr::recode(travel_quiz$region,
                                          africa        = "Africa",
                                          asia          = "Asia",
                                          europe        = "Europe",
                                          middle_east   = "Middle East",
                                          north_america = "North America",
                                          oceania       = "Oceania",
                                          south_america = "South America"
)

# ── Constants ──────────────────────────────────────────────────────────────────

VIBE_ATTRS <- c("culture", "adventure", "nature", "beaches",
                "nightlife", "cuisine", "wellness", "urban", "seclusion")

VIBE_LABELS <- c(
  culture   = "🎭 Culture",   adventure = "🧗 Adventure",
  nature    = "🌿 Nature",    beaches   = "🏖️ Beaches",
  nightlife = "🎉 Nightlife", cuisine   = "🍽️ Cuisine",
  wellness  = "🧘 Wellness",  urban     = "🏙️ Urban",
  seclusion = "🌄 Seclusion"
)

# ── Scoring function ───────────────────────────────────────────────────────────

score_cities <- function(region, temp_range, duration, budget, vibe_weights) {
  df <- travel_quiz
  
  # Hard filters
  if (!is.null(region) && region != "No preference")
    df <- df[df$region_label == region, ]
  
  if (!is.null(budget) && budget != "No preference")
    df <- df[df$budget_level == budget, ]
  
  if (!is.null(duration) && duration != "No preference")
    df <- df[sapply(df$durations_list, function(d) duration %in% d), ]
  
  if (nrow(df) == 0) return(df)
  
  # Temperature score (soft, 0–1)
  if (!is.null(temp_range)) {
    t_mid  <- mean(temp_range)
    t_band <- diff(temp_range) / 2 + 5
    df$temp_score <- 1 - pmin(abs(df$annual_avg_temp - t_mid) / t_band, 1)
  } else {
    df$temp_score <- 1
  }
  
  # Vibe score: weighted dot-product (city scores normalised 1–5 → 0–1)
  total_w <- sum(abs(vibe_weights))
  if (total_w == 0) {
    df$vibe_score <- 1
  } else {
    vibe_matrix <- as.matrix(df[, names(vibe_weights)])
    vibe_norm   <- (vibe_matrix - 1) / 4          # 1→0, 5→1
    weight_norm <- as.numeric(vibe_weights) / total_w
    df$vibe_score <- as.numeric(vibe_norm %*% weight_norm)
  }
  
  df$total_score <- 0.3 * df$temp_score + 0.7 * df$vibe_score
  df[order(-df$total_score), ]
}


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
    
    avg_on_time <- mean(arrival_2025$pct_on_time, na.rm = TRUE)
    
    ggplot(arrival_2025, aes(x = airport, y = pct_on_time, fill = pct_on_time)) +
      geom_col(width = 0.7) +
      geom_text(
        aes(label = paste0(round(pct_on_time, 1), "%")),
        hjust = -0.15, size = 3.5, color = "#333333"
      ) +
      coord_flip() +
      geom_hline(yintercept = avg_on_time, linetype = "dashed", color = "#C84B8F", linewidth = 0.8) +
      annotate("text", y = avg_on_time + 0.2, x = 0.6,
               label = paste0("Avg: ", round(avg_on_time, 1), "%"),
               color = "#C84B8F", size = 3.2, hjust = 0) +
      scale_fill_gradient(low = "#f4a261", high = "#2a9d8f") +
      scale_y_continuous(
        expand = expansion(mult = c(0, 0.12)),
        labels = label_percent(scale = 1)
      ) +
      labs(
        title = "Airports Ranked by On-Time Arrival Percentage",
        subtitle = "% of flights arriving on time · Dashed line = overall average",
        x = NULL, y = NULL,
        caption = "Source: arrival_2025 dataset"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        plot.title         = element_text(face = "bold", size = 16),
        plot.subtitle      = element_text(color = "#666666", size = 11),
        panel.grid.major.x = element_blank(),
        panel.grid.minor   = element_blank(),
        legend.position    = "none",
        plot.caption       = element_text(color = "#aaaaaa", size = 9)
      )
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
      as.character() %>%
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
    
    if (nrow(df) == 0) {
      return(p("No data found for this route."))
    }
    
    tagList(
      h4(paste(as.character(df$city1), "→", as.character(df$city2))),
      br(),
      fluidRow(
        column(6,
               wellPanel(
                 h4("✈ Largest Carrier"),
                 h2(as.character(df$carrier_lg)),
                 p(paste("Market share:", round(as.numeric(df$large_ms) * 100, 1), "%")),
                 p(paste("Avg fare: $", as.character(df$fare_lg)))
               )
        ),
        column(6,
               wellPanel(
                 h4("💰 Cheapest Carrier"),
                 h2(as.character(df$carrier_low)),
                 p(paste("Market share:", round(as.numeric(df$lf_ms) * 100, 1), "%")),
                 p(paste("Avg fare: $", as.character(df$fare_low)))
               )
        )
      ),
      br(),
      p(paste("Distance:", df$nsmiles, "miles  |  Passengers:", df$passengers,
              " |  Data from: Q", df$quarter, df$Year))
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
  
  output$delay_plot <- renderPlot({
    
    delay_summary <- final_flights %>%
      group_by(MKT_UNIQUE_CARRIER) %>%
      summarise(
        total_flights = n(),
        delayed_flights = sum(DEP_DELAY > 15, na.rm = TRUE),
        delay_pct = delayed_flights / total_flights * 100
      ) %>%
      filter(total_flights >= 30) %>%
      arrange(delay_pct) %>%
      mutate(MKT_UNIQUE_CARRIER = fct_inorder(MKT_UNIQUE_CARRIER))
    
    avg_delay <- mean(delay_summary$delay_pct)
    
    ggplot(delay_summary, aes(x = delay_pct, y = MKT_UNIQUE_CARRIER, fill = delay_pct)) +
      geom_col(width = 0.7) +
      geom_text(
        aes(label = paste0(round(delay_pct, 1), "%")),
        hjust = -0.15, size = 3.5, color = "#333333"
      ) +
      geom_vline(xintercept = avg_delay, linetype = "dashed", color = "#E05C2A", linewidth = 0.8) +
      annotate("text", x = avg_delay + 0.5, y = 0.6,
               label = paste0("Avg: ", round(avg_delay, 1), "%"),
               color = "#E05C2A", size = 3.2, hjust = 0) +
      scale_fill_gradient(low = "#43AA8B", high = "#E05C2A") +
      scale_x_continuous(
        expand = expansion(mult = c(0, 0.12)),
        labels = label_percent(scale = 1)
      ) +
      labs(
        title = "Which Airlines Delay Flights the Most?",
        subtitle = "% of flights delayed more than 10 minutes · Dashed line = overall average",
        x = NULL, y = NULL,
        caption = "Source: final_flights dataset"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        plot.title = element_text(face = "bold", size = 16),
        plot.subtitle = element_text(color = "#666666", size = 11),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        plot.caption = element_text(color = "#aaaaaa", size = 9)
      )
  })
  
  output$cancellation_plot <- renderPlot({
    
    cancellation_summary <- final_flights %>%
      group_by(MKT_UNIQUE_CARRIER) %>%
      summarise(
        total_flights = n(),
        cancelled_flights = sum(CANCELLED == 1.00, na.rm = TRUE),
        cancellation_pct = cancelled_flights / total_flights * 100
      ) %>%
      filter(total_flights >= 30) %>%
      arrange(cancellation_pct) %>%
      mutate(MKT_UNIQUE_CARRIER = fct_inorder(MKT_UNIQUE_CARRIER))
    
    avg_cancellation <- mean(cancellation_summary$cancellation_pct)
    
    ggplot(cancellation_summary, aes(x = cancellation_pct, y = MKT_UNIQUE_CARRIER, fill = cancellation_pct)) +
      geom_col(width = 0.7) +
      geom_text(
        aes(label = paste0(round(cancellation_pct, 1), "%")),
        hjust = -0.15, size = 3.5, color = "#333333"
      ) +
      geom_vline(xintercept = avg_cancellation, linetype = "dashed", color = "#C84B8F", linewidth = 0.8) +
      annotate("text", x = avg_cancellation + 0.5, y = 0.6,
               label = paste0("Avg: ", round(avg_cancellation, 1), "%"),
               color = "#C84B8F", size = 3.2, hjust = 0) +
      scale_fill_gradient(low = "#9BD4C5", high = "#4B0082") +
      scale_x_continuous(
        expand = expansion(mult = c(0, 0.12)),
        labels = label_percent(scale = 1)
      ) +
      labs(
        title = "Which Airlines Cancel Flights the Most?",
        subtitle = "% of flights cancelled · Dashed line = overall average",
        x = NULL, y = NULL,
        caption = "Source: final_flights dataset"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        plot.title = element_text(face = "bold", size = 16),
        plot.subtitle = element_text(color = "#666666", size = 11),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        plot.caption = element_text(color = "#aaaaaa", size = 9)
      )
  })
  
  output$busiest_days_plot <- renderPlot({
    
    busiest_days <- final_flights %>%
      group_by(FL_DATE) %>%
      summarise(total_flights = n()) %>%
      arrange(desc(total_flights)) %>%
      slice_head(n = 10) %>%
      arrange(total_flights) %>%
      mutate(FL_DATE = fct_inorder(as.character(FL_DATE)))
    
    ggplot(busiest_days, aes(x = total_flights, y = FL_DATE, fill = total_flights)) +
      geom_col(width = 0.7) +
      geom_text(
        aes(label = scales::comma(total_flights)),
        hjust = -0.15, size = 3.5, color = "#333333"
      ) +
      scale_fill_gradient(low = "#F0C27F", high = "#C0392B") +
      scale_x_continuous(
        expand = expansion(mult = c(0, 0.12)),
        labels = scales::comma
      ) +
      labs(
        title = "Top 10 Busiest Travel Days of 2025",
        subtitle = "Days with the highest number of flights",
        x = NULL, y = NULL,
        caption = "Source: final_flights dataset"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        plot.title = element_text(face = "bold", size = 16),
        plot.subtitle = element_text(color = "#666666", size = 11),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        plot.caption = element_text(color = "#aaaaaa", size = 9)
      )
  })
  
  output$airport_cancellation_plot <- renderPlot({
    
    airport_cancel_summary <- final_flights %>%
      group_by(ORIGIN_CITY_NAME) %>%
      summarise(
        total_flights    = n(),
        cancelled_flights = sum(CANCELLED == 1.00, na.rm = TRUE),
        cancellation_pct  = cancelled_flights / total_flights * 100
      ) %>%
      filter(total_flights >= 30) %>%
      arrange(desc(cancellation_pct)) %>%
      slice_head(n = 20) %>%
      arrange(cancellation_pct) %>%
      mutate(ORIGIN_CITY_NAME = fct_inorder(ORIGIN_CITY_NAME))
    
    avg_cancellation <- mean(airport_cancel_summary$cancellation_pct)
    
    ggplot(airport_cancel_summary, aes(x = cancellation_pct, y = ORIGIN_CITY_NAME, fill = cancellation_pct)) +
      geom_col(width = 0.7) +
      geom_text(
        aes(label = paste0(round(cancellation_pct, 1), "%")),
        hjust = -0.15, size = 3.5, color = "#333333"
      ) +
      geom_vline(xintercept = avg_cancellation, linetype = "dashed", color = "#C84B8F", linewidth = 0.8) +
      annotate("text", x = avg_cancellation + 0.2, y = 0.6,
               label = paste0("Avg: ", round(avg_cancellation, 1), "%"),
               color = "#C84B8F", size = 3.2, hjust = 0) +
      scale_fill_gradient(low = "#9BD4C5", high = "#4B0082") +
      scale_x_continuous(
        expand = expansion(mult = c(0, 0.12)),
        labels = label_percent(scale = 1)
      ) +
      labs(
        title = "Which Airports Have the Most Cancellations?",
        subtitle = "Top 20 airports by cancellation rate · Dashed line = overall average",
        x = NULL, y = NULL,
        caption = "Source: final_flights dataset"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        plot.title    = element_text(face = "bold", size = 16),
        plot.subtitle = element_text(color = "#666666", size = 11),
        panel.grid.major.y = element_blank(),
        panel.grid.minor   = element_blank(),
        legend.position    = "none",
        plot.caption       = element_text(color = "#aaaaaa", size = 9)
      )
  })
  
  #travel quiz server
  # Disable temp slider when "any" is checked
  observe({
    if (isTRUE(input$temp_any)) {
      shinyjs::disable("temp_range")
    } else {
      shinyjs::enable("temp_range")
    }
  })
  
  # Score on button press
  results <- eventReactive(input$go_btn, {
    vibe_weights <- setNames(
      sapply(VIBE_ATTRS, function(a) input[[paste0("vibe_", a)]]),
      VIBE_ATTRS
    )
    score_cities(
      region       = input$region,
      temp_range   = if (isTRUE(input$temp_any)) NULL else input$temp_range,
      duration     = input$duration,
      budget       = input$budget,
      vibe_weights = vibe_weights
    )
  })
  
  # ── Results panel ────────────────────────────────────────────────────────────
  output$results_panel <- renderUI({
    
    # Pre-click welcome state
    if (is.null(input$go_btn) || input$go_btn == 0) {
      return(div(class = "empty-state",
                 div(class = "icon", "🗺️"),
                 h3(style = "color:#264653; margin-top:16px;", "Your matches will appear here"),
                 p("Fill in your preferences and click", strong("Find My Destinations."))
      ))
    }
    
    df <- results()
    
    if (nrow(df) == 0) {
      return(div(class = "empty-state",
                 div(class = "icon", "😕"),
                 h3("No destinations matched"),
                 p("Try relaxing your region, budget, or duration filters.")
      ))
    }
    
    top <- head(df, 10)
    
    tagList(
      # Summary strip
      div(style = "display:flex; align-items:center; gap:12px; margin-bottom:16px;",
          div(style = "background:#2a9d8f; color:white; border-radius:10px;
                     padding:9px 18px; font-weight:700;",
              paste0("✅ ", nrow(df), " destinations found")
          ),
          p(style = "color:#6c757d; margin:0; font-size:.9rem;",
            paste0("Showing top ", nrow(top), " ranked by your preferences"))
      ),
      
      tabsetPanel(id = "result_tabs",
                  
                  # Tab 1: Result cards
                  tabPanel("🏙️ Top Picks",
                           br(),
                           lapply(seq_len(nrow(top)), function(i) {
                             row  <- top[i, ]
                             pct  <- round(row$total_score * 100)
                             durs <- paste(row$durations_list[[1]], collapse = " · ")
                             
                             div(class = "result-card",
                                 div(style = "display:flex; align-items:flex-start;",
                                     div(class = "result-rank", paste0("#", i)),
                                     div(style = "flex:1;",
                                         div(class = "result-city",    row$city),
                                         div(class = "result-country",
                                             paste0(row$country, "  ·  ", row$region_label)),
                                         div(class = "result-desc", row$short_description),
                                         div(
                                           span(class = "badge-pill", paste0("💰 ", row$budget_level)),
                                           span(class = "badge-pill",
                                                paste0("🌡️ ", round(row$annual_avg_temp, 1), "°C avg")),
                                           if (nchar(durs) > 0)
                                             span(class = "badge-pill", paste0("🗓️ ", durs))
                                         ),
                                         div(class = "score-bar-wrap",
                                             div(class = "score-bar", style = paste0("width:", pct, "%;"))
                                         ),
                                         div(style = "font-size:.8rem; color:#888; margin-top:3px;",
                                             paste0("Match score: ", pct, "%"))
                                     )
                                 )
                             )
                           })
                  ),
                  
                  # Tab 2: Vibe comparison chart
                  tabPanel("📊 Vibe Comparison",
                           br(),
                           p(style = "color:#6c757d; font-size:.9rem;",
                             "Average vibe scores: your top picks vs. all cities in the dataset"),
                           plotOutput("vibe_chart", height = "420px")
                  )
      )
    )
  })
  
  # ── Vibe comparison bar chart ─────────────────────────────────────────────────
  output$vibe_chart <- renderPlot({
    req(input$go_btn > 0)
    df <- results()
    req(nrow(df) > 0)
    
    top <- head(df, 10)
    
    matched_means <- colMeans(top[, VIBE_ATTRS])
    all_means     <- colMeans(travel_quiz[, VIBE_ATTRS])
    
    plot_df <- data.frame(
      Attribute = rep(VIBE_LABELS[VIBE_ATTRS], 2),
      Score     = c(as.numeric(matched_means), as.numeric(all_means)),
      Group     = rep(c("Your Top Picks", "All Cities"), each = length(VIBE_ATTRS))
    )
    plot_df$Attribute <- factor(plot_df$Attribute,
                                levels = rev(VIBE_LABELS[VIBE_ATTRS]))
    
    ggplot(plot_df, aes(x = Attribute, y = Score, fill = Group)) +
      geom_col(position = position_dodge(width = .7), width = .6) +
      coord_flip() +
      scale_fill_manual(values = c("Your Top Picks" = "#2a9d8f",
                                   "All Cities"     = "#e9c46a")) +
      scale_y_continuous(limits = c(0, 5), breaks = 1:5) +
      labs(title = "Vibe Profile: Your Matches vs. All Destinations",
           x = NULL, y = "Average Score (1–5)", fill = NULL) +
      theme_minimal(base_size = 13) +
      theme(
        plot.title             = element_text(face = "bold", colour = "#264653"),
        legend.position        = "bottom",
        panel.grid.major.y     = element_blank(),
        panel.grid.minor       = element_blank()
      )
  })
  
}

