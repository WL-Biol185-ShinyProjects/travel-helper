library(shiny)
library(ggplot2)
library(tidyverse)
library(readxl)
library(dplyr)
library(httr)
library(jsonlite)
library(leaflet)
library(scales)
library(lubridate)

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
unesco_coords$Latitude  <- as.numeric(unesco_coords$Latitude)
unesco_coords$Longitude <- as.numeric(unesco_coords$Longitude)

airfare_data <- read.csv("airfare_data.csv") %>%
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
world_cities <- world_cities %>% mutate(across(where(is.list), as.character))
world_cities <- world_cities[!is.na(world_cities$population) &
                               world_cities$population > 500000, ]
world_cities$lat <- as.numeric(world_cities$lat)
world_cities$lng <- as.numeric(world_cities$lng)
world_cities$id  <- as.character(world_cities$id)

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
numeric_to_time <- function(x) {
  hours   <- x %/% 100
  minutes <- x %%  100
  as.POSIXct(sprintf("%02d:%02d", hours, minutes), format = "%H:%M")
}
dep_times <- numeric_to_time(final_flights$CRS_DEP_TIME)
del_times <- numeric_to_time(final_flights$DEP_TIME)


# Server
function(input, output, session) {
  
  # --- Passport requirement ---
  output$Requirement <- renderText({
    result <- passport_info %>%
      filter(Passport == input$Passport, Destination == input$Destination)
    if (nrow(result) > 0) result$Requirement[1] else "No information available"
  })
  
  # --- Currency ---
  output$Currency <- renderText({
    currencyVcountry[currencyVcountry$Country == input$Country, "Currency"]
  })
  
  # --- Vaccinations ---
  output$vaccination_required <- renderText({
    vaccinationVcountry[vaccinationVcountry$country_vaccination == input$country_vaccination, "vaccination_required"]
  })
  
  # --- Adapter ---
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
  
  # --- Airports chart ---
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
  
  # --- Airfare: destination dropdown ---
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
  
  # --- Airfare: route search ---
  route_data <- eventReactive(input$search, {
    req(input$origin, input$dest)
    airfare_data %>%
      filter(city1 == input$origin, city2 == input$dest) %>%
      arrange(desc(Year), desc(quarter)) %>%
      slice(1)
  })
  
  # --- Airfare: display results ---
  output$route_results <- renderUI({
    req(route_data())
    df <- route_data()
    if (nrow(df) == 0) return(p("No data found for this route."))
    tagList(
      h4(paste(as.character(df$city1), "->", as.character(df$city2))),
      br(),
      fluidRow(
        column(6,
               wellPanel(
                 h4("Largest Carrier"),
                 h2(as.character(df$carrier_lg)),
                 p(paste("Market share:", round(as.numeric(df$large_ms) * 100, 1), "%")),
                 p(paste("Avg fare: $", as.character(df$fare_lg)))
               )
        ),
        column(6,
               wellPanel(
                 h4("Cheapest Carrier"),
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
  
  # --- Weather map: initial render ---
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
  
  # --- Weather map: redraw when tab activated (fixes grey box) ---
  observeEvent(input$tabs, {
    if (!is.null(input$tabs) && input$tabs == "weather") {
      leafletProxy("weather_map") %>%
        setView(lng = 0, lat = 20, zoom = 2)
    }
  })
  
  # --- Weather: city dropdown ---
  output$city_selector <- renderUI({
    req(input$weather_country)
    cities <- world_cities[world_cities$country == input$weather_country, ]
    selectizeInput("weather_city", "Select City",
                   choices = c("", sort(unique(cities$city))),
                   options = list(placeholder = "Type or select a city..."))
  })
  
  # --- Weather: fly to country ---
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
  
  # --- Weather: fly to city ---
  observeEvent(input$weather_city, {
    req(input$weather_city)
    row <- world_cities[world_cities$city == input$weather_city &
                          world_cities$country == input$weather_country, ]
    if (nrow(row) > 0) {
      leafletProxy("weather_map") %>%
        setView(lng = row$lng[1], lat = row$lat[1], zoom = 8)
    }
  })
  
  # --- Weather: marker click ---
  observeEvent(input$weather_map_marker_click, {
    click <- input$weather_map_marker_click
    row <- world_cities[world_cities$id == click$id, ]
    lat <- row$lat
    lon <- row$lng
    city <- row$city
    country <- row$country
    
    url <- paste0(
      "https://api.open-meteo.com/v1/forecast?",
      "latitude=", lat, "&longitude=", lon,
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
      weather$weathercode == 0        ~ "Clear sky",
      weather$weathercode %in% 1:3   ~ "Partly cloudy",
      weather$weathercode %in% 45:48 ~ "Foggy",
      weather$weathercode %in% 51:67 ~ "Rainy",
      weather$weathercode %in% 71:77 ~ "Snowy",
      weather$weathercode %in% 80:82 ~ "Rain showers",
      weather$weathercode %in% 95:99 ~ "Thunderstorm",
      TRUE ~ "Unknown"
    )
    
    forecast_rows <- paste0(
      sapply(1:nrow(forecast), function(i) {
        paste0(
          "<tr>",
          "<td style='padding:3px 8px'>", forecast$time[i], "</td>",
          "<td style='padding:3px 8px'>", forecast$temperature_2m_max[i], "F / ",
          forecast$temperature_2m_min[i], "F</td>",
          "<td style='padding:3px 8px'>", forecast$precipitation_sum[i], " in</td>",
          "</tr>"
        )
      }),
      collapse = ""
    )
    
    popup_text <- paste0(
      "<b style='font-size:14px'>", city, ", ", country, "</b><br><br>",
      "<b>Now:</b> ", weather$temperature, " F<br>",
      "<b>Wind:</b> ", weather$windspeed, " km/h<br>",
      "<b>Conditions:</b> ", weather_description, "<br><br>",
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
  
  # --- UNESCO: sites list ---
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
  
  # --- UNESCO: observe site clicks ---
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
  
  # --- UNESCO: site image from Wikipedia ---
  output$site_image <- renderUI({
    req(selected_site())
    url <- paste0(
      "https://en.wikipedia.org/api/rest_v1/page/summary/",
      URLencode(selected_site())
    )
    response <- tryCatch(GET(url), error = function(e) NULL)
    if (!is.null(response) && status_code(response) == 200) {
      data <- fromJSON(content(response, "text", encoding = "UTF-8"))
      img_url     <- data$thumbnail$source
      description <- data$extract
      wiki_url    <- data$content_urls$desktop$page
      tagList(
        br(),
        h4(selected_site()),
        if (!is.null(img_url)) {
          tags$img(src = img_url, width = "100%",
                   style = "border-radius: 8px; margin-bottom: 10px;")
        },
        p(description),
        tags$a(href = wiki_url, target = "_blank", "Read more on Wikipedia")
      )
    } else {
      p("No image available for this site.")
    }
  })
  
  # --- UNESCO map: initial render ---
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
  
  # --- UNESCO map: redraw when tab activated (fixes grey box) ---
  observeEvent(input$tabs, {
    if (!is.null(input$tabs) && input$tabs == "travel_suggestions") {
      leafletProxy("unesco_map") %>%
        setView(lng = 0, lat = 20, zoom = 2)
    }
  })
  
  # --- UNESCO map: zoom to country ---
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
  
  # --- UNESCO map: marker click ---
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
    
    img_tag   <- NULL
    desc_tag  <- NULL
    wiki_link <- NULL
    
    tryCatch({
      resp <- GET(wiki_url)
      if (status_code(resp) == 200) {
        wiki_data <- fromJSON(content(resp, "text", encoding = "UTF-8"))
        if (!is.null(wiki_data$thumbnail$source)) {
          img_tag <- tags$img(
            src = wiki_data$thumbnail$source,
            width = "100%",
            style = "border-radius:8px; margin-bottom:10px;"
          )
        }
        if (!is.null(wiki_data$extract)) {
          desc_tag <- p(style = "font-size:12px", wiki_data$extract)
        }
        if (!is.null(wiki_data$content_urls$desktop$page)) {
          wiki_link <- tags$a(
            href = wiki_data$content_urls$desktop$page,
            target = "_blank",
            "Read more on Wikipedia"
          )
        }
      }
    }, error = function(e) {})
    
    output$unesco_popup_image <- renderUI({
      tagList(
        h4(site_name),
        span(style = "color:#666", paste0("Country: ", country)),
        br(), br(),
        img_tag,
        desc_tag,
        tags$a(href = gmaps_url, target = "_blank", "Open in Google Maps"),
        tags$span("  "),
        wiki_link
      )
    })
    
    leafletProxy("unesco_map") %>%
      clearPopups() %>%
      addPopups(
        lng = lng,
        lat = lat,
        popup = paste0("<b>", site_name, "</b><br>", country)
      )
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
  packing_list_data <- eventReactive(input$generate_list, {
    req(input$UNESCOCountry, input$pack_depart, input$pack_return)
    
    trip_days <- as.numeric(difftime(input$pack_return, input$pack_depart, units = "days"))
    
    # Pull forecast for destination country
    country_row <- world_cities[world_cities$country == input$UNESCOCountry, ]
    lat <- mean(country_row$lat, na.rm = TRUE)
    lon <- mean(country_row$lng, na.rm = TRUE)
    
    temps <- tryCatch({
      url <- paste0(
        "https://api.open-meteo.com/v1/forecast?",
        "latitude=", lat, "&longitude=", lon,
        "&daily=temperature_2m_max,temperature_2m_min",
        "&temperature_unit=fahrenheit",
        "&timezone=auto&forecast_days=7"
      )
      resp <- GET(url)
      data <- fromJSON(content(resp, "text", encoding = "UTF-8"))
      avg_high <- mean(data$daily$temperature_2m_max, na.rm = TRUE)
      avg_low  <- mean(data$daily$temperature_2m_min, na.rm = TRUE)
      list(high = avg_high, low = avg_low)
    }, error = function(e) list(high = NA, low = NA))
    
    cold <- !is.na(temps$low)  && temps$low  < 45
    warm <- !is.na(temps$high) && temps$high > 75
    
    tropical_countries <- c(
      "Thailand", "Indonesia", "Philippines", "Malaysia", "Maldives", "Sri Lanka",
      "Vietnam", "Cambodia", "Myanmar", "Laos", "Singapore", "Brunei", "Timor-Leste",
      "Mexico", "Costa Rica", "Panama", "Belize", "Honduras", "Guatemala", "Nicaragua",
      "El Salvador", "Cuba", "Jamaica", "Dominican Republic", "Haiti", "Puerto Rico",
      "Barbados", "Trinidad and Tobago", "Saint Lucia", "Antigua and Barbuda",
      "Saint Kitts and Nevis", "Grenada", "Dominica", "Bahamas", "Turks and Caicos",
      "Cayman Islands", "Aruba", "Curacao", "Bonaire",
      "Brazil", "Colombia", "Venezuela", "Ecuador", "Peru", "Guyana", "Suriname",
      "French Guiana", "Bolivia",
      "Nigeria", "Ghana", "Senegal", "Ivory Coast", "Cameroon", "Kenya", "Tanzania",
      "Mozambique", "Madagascar", "Mauritius", "Seychelles",
      "Australia", "Fiji", "Papua New Guinea", "Vanuatu", "Solomon Islands",
      "Samoa", "Tonga", "Kiribati", "Palau", "Marshall Islands", "Micronesia",
      "India", "Bangladesh", "Oman", "Yemen", "Djibouti",
      "United Arab Emirates", "Qatar", "Bahrain", "Kuwait",
      "Egypt", "Libya", "Tunisia", "Morocco", "Algeria"
    )
    
    tropical <- input$UNESCOCountry %in% tropical_countries
    
    # Build categories
    documents <- c(
      "Passport",
      "Flight confirmation & boarding passes",
      "Hotel / accommodation confirmation",
      "Travel insurance documents",
      "Emergency contacts list",
      if (trip_days > 14) "Extra passport photos"
    )
    
    clothing <- c(
      paste0(min(trip_days, 7), " pairs of underwear & socks"),
      paste0(ceiling(trip_days / 3), " casual tops"),
      if (cold)    c("Heavy coat or parka", "Thermal underlayers", "Warm hat & gloves", "Scarf", "Waterproof boots"),
      if (warm)    c("Light t-shirts / tank tops", "Shorts or light pants", "Comfortable walking shoes"),
      if (tropical) c("Swimsuit / bathing suit", "Beach cover-up or sarong", "Flip flops or sandals", "Sun hat or visor"),
      if (!cold && !warm && !tropical) c("Light jacket or hoodie", "Mix of long & short sleeve tops", "Comfortable walking shoes"),
      "Versatile pants or jeans",
      "One smart/dressy outfit",
      "Comfortable pajamas",
      "Laundry bag"
    )
    
    toiletries <- c(
      "Toothbrush & toothpaste",
      "Shampoo & conditioner",
      "Body wash / soap",
      "Deodorant",
      "Razor & shaving cream",
      "Skincare / moisturizer",
      if (warm || tropical) c("Sunscreen SPF 50+", "After-sun lotion / aloe vera", "Insect repellent"),
      if (tropical)         c("Waterproof sunscreen", "Reef-safe sunscreen"),
      if (cold)             "Lip balm & heavy moisturizer",
      "Nail clippers & tweezers",
      "Travel mirror"
    )
    
    health <- c(
      "Prescription medications (enough for trip + extra)",
      "Pain relievers (ibuprofen / acetaminophen)",
      "Antihistamines",
      "Antidiarrheal medicine",
      "Band-aids & blister pads",
      "Hand sanitizer",
      "Face masks",
      if (warm || tropical) "Oral rehydration salts"
    )
    
    electronics <- c(
      "Phone & charger",
      "Portable power bank",
      "Universal travel adapter",
      "Headphones or earbuds",
      "Camera (if not using phone)",
      if (trip_days > 5) "Laptop or tablet & charger",
      "E-reader or books"
    )
    
    misc <- c(
      "Local currency or travel card",
      "Reusable water bottle",
      "Day bag or small backpack",
      "Snacks for travel day",
      "Neck pillow & eye mask",
      "Luggage locks",
      if (trip_days > 10) "Collapsible extra bag for souvenirs",
      "Pen (for customs forms)"
    )
    
    list(
      trip_days = trip_days,
      temps     = temps,
      cold      = cold,
      warm      = warm,
      tropical  = tropical,
      categories = list(
        "📄 Documents"     = documents,
        "👕 Clothing"      = clothing,
        "🧴 Toiletries"    = toiletries,
        "💊 Health"        = health,
        "🔌 Electronics"   = electronics,
        "🎒 Miscellaneous" = misc
      )
    )
  })
  
  output$packing_list_output <- renderUI({
    req(packing_list_data())
    d <- packing_list_data()
    
    temp_msg <- if (d$cold) {
      "🥶 Cold weather expected — warm layers included."
    } else if (d$tropical) {
      "🏖️ Tropical destination — beach gear & sun protection included."
    } else if (d$warm) {
      "☀️ Warm weather expected — light clothing & sun protection included."
    } else {
      "🌤️ Mild weather expected — versatile layers included."
    }
    
    tagList(
      p(style = "color:#666; font-style:italic",
        paste0("Trip length: ", d$trip_days, " days  |  ", temp_msg)),
      br(),
      lapply(names(d$categories), function(cat_name) {
        items <- d$categories[[cat_name]]
        items <- items[!is.null(items) & !is.na(items) & nchar(items) > 0]
        tagList(
          h4(cat_name),
          tags$ul(lapply(items, function(item) tags$li(item)))
        )
      })
    )
  })
  # Fetch country list from State Dept API on app load
  observe({
    tryCatch({
      resp <- GET("https://travel.state.gov/content/travel/en/traveladvisories/traveladvisories.html/_jcr_content/ada_form_parsys/ada_form/items.model.json")
      
      if (status_code(resp) == 200) {
        data <- fromJSON(content(resp, "text", encoding = "UTF-8"))
        country_names <- sort(unique(data$items$country))
        updateSelectizeInput(session, "advisory_country",
                             choices  = country_names,
                             server   = TRUE)
      }
    }, error = function(e) NULL)
  })
  
  advisory_data <- eventReactive(input$get_advisory, {
    req(input$advisory_country)
    
    # Fetch the RSS feed
    resp <- tryCatch(
      GET("https://travel.state.gov/_res/rss/TAsTWs.rss"),
      error = function(e) NULL
    )
    
    if (is.null(resp) || status_code(resp) != 200) {
      return(list(error = "Could not reach the U.S. State Department. Please try again later."))
    }
    
    xml_text <- content(resp, "text", encoding = "UTF-8")
    
    # Split into individual items
    items <- strsplit(xml_text, "<item>")[[1]][-1]
    
    # Find the matching country
    matched <- NULL
    for (item in items) {
      title_match <- regmatches(item, regexpr("(?<=<title>)[^<]+(?=</title>)", item, perl = TRUE))
      if (length(title_match) > 0 && grepl(input$advisory_country, title_match, fixed = TRUE)) {
        matched <- item
        break
      }
    }
    
    if (is.null(matched)) {
      return(list(error = paste0("No advisory found for ", input$advisory_country, ".")))
    }
    
    # Extract fields
    title <- regmatches(matched, regexpr("(?<=<title>)[^<]+(?=</title>)", matched, perl = TRUE))
    
    level_num <- NA
    level_label <- "Unknown"
    if (grepl("Level 1", title)) { level_num <- 1; level_label <- "Level 1 — Exercise Normal Caution" }
    if (grepl("Level 2", title)) { level_num <- 2; level_label <- "Level 2 — Exercise Increased Caution" }
    if (grepl("Level 3", title)) { level_num <- 3; level_label <- "Level 3 — Reconsider Travel" }
    if (grepl("Level 4", title)) { level_num <- 4; level_label <- "Level 4 — Do Not Travel" }
    
    description <- regmatches(matched, regexpr("(?<=<description>)[^<]+(?=</description>)", matched, perl = TRUE))
    description <- if (length(description) > 0) trimws(description) else "No summary available."
    
    link <- regmatches(matched, regexpr("(?<=<link>)[^<]+(?=</link>)", matched, perl = TRUE))
    link <- if (length(link) > 0) trimws(link) else NA
    
    pub_date <- regmatches(matched, regexpr("(?<=<pubDate>)[^<]+(?=</pubDate>)", matched, perl = TRUE))
    pub_date <- if (length(pub_date) > 0) trimws(pub_date) else NA
    
    list(
      error       = NULL,
      country     = input$advisory_country,
      level_num   = level_num,
      level_label = level_label,
      description = description,
      link        = link,
      pub_date    = pub_date
    )
  })
  
  output$advisory_output <- renderUI({
    req(advisory_data())
    d <- advisory_data()
    
    if (!is.null(d$error)) {
      return(p(style = "color:red;", d$error))
    }
    
    # Color and icon based on level
    level_color <- switch(as.character(d$level_num),
                          "1" = "#2a9d8f",
                          "2" = "#e9c46a",
                          "3" = "#f4a261",
                          "4" = "#e63946",
                          "#888888"
    )
    
    level_icon <- switch(as.character(d$level_num),
                         "1" = "✅",
                         "2" = "⚠️",
                         "3" = "🔶",
                         "4" = "🚫",
                         "❓"
    )
    
    tagList(
      tags$div(
        style = paste0(
          "background-color:", level_color, "22;",
          "border-left: 5px solid ", level_color, ";",
          "padding: 12px 16px;",
          "border-radius: 4px;",
          "margin-bottom: 12px;"
        ),
        h3(style = paste0("color:", level_color, "; margin-top:0;"),
           paste0(level_icon, " ", d$level_label)),
        h4(style = "margin-top:0;", d$country)
      ),
      p(style = "font-size:14px; margin-top: 10px;", d$description),
      if (!is.na(d$pub_date)) p(style = "color:#999; font-size:12px;", paste0("Last updated: ", d$pub_date)),
      if (!is.na(d$link))
        tags$a(href = d$link, target = "_blank",
               class = "btn btn-sm btn-default",
               "View Full Advisory on State.gov ↗")
    )
  })
}

  
  
