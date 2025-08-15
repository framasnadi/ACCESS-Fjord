#############################################################
#                                                           #
#          ACCESS-Fjord Fish Catch Data Visualization       #
#                          ShinyApp                         #
#                 Author: Francesco Masnadi                 #
#                                                           #
#############################################################
library(shiny)
library(shinyWidgets)
library(ggplot2)
library(dplyr)
library(lubridate)
library(readxl)
library(ggpmisc)
library(rsconnect)
library(patchwork)
library(gam.hp)
# AMSY libraries
library(coda) 
library("gplots")
library(mvtnorm) # used for Kobe plot, ignore version error
library(crayon) # to display bold and italics in console

# UI ########################################################################
ui <- fluidPage(
  titlePanel("Reel4Science (SIMULATED DATA)"),
  helpText(tags$h4("Turn your catches into real science")),
  helpText(
    tags$span("Need help? ",
              tags$a(
                href = "https://www.su.se/english/profiles/frma6502-1.665005",
                HTML("Francesco Masnadi"), 
                style = "color: #007BFF; text-decoration: none;"
              )
    )
  ),
  sidebarLayout(
    mainPanel( selectInput("page_select", "Select View:",
                           choices = c("Sampling Area Map",
                                       "Leader Board",
                                       "Catch Plot",
                                       "Catch & Release vs Retained",
                                       "Length Frequency Distribution (LFD)",
                                       "Length-Weight (L-W) relationship",
                                       "CPUE trend",
                                       "Size-based indicators (L50 - L90)",
                                       "Stock Assessment (AMSY)",
                                       "Environmental Variables",
                                       "Environmental Influence Analysis"),
                           selectize = F),
               uiOutput("dynamic_panel")
    ),
    sidebarPanel(
      uiOutput("fisherman_filter"),
      uiOutput("species_filter_multi"),
      uiOutput("gear_filter"),
      uiOutput("station_filter"),
      dateRangeInput("date_filter", "Select Date Range:",
                     start = Sys.Date() - 1200, end = Sys.Date()),
      #actionButton("update", "Update Plots")
    )
  )
)
###################################################


###################################################
# Server
server <- function(input, output, session) {
  
  # Path to the dataset file
  # file_path <- "C:/Users/frma6502/Desktop/Other/ShinyAPPnorway/TESTdb.xlsx"  # Replace with your actual file path
  #  file_path <- "TESTdb.xlsx"  # Replace with your actual file path
  file_path <- "simulated_fish_data.xlsx" 
  file_path2 <-  "simulated_environmental_data.xlsx" 
  file_path3 <- "ALL_IDnorway.csv" 
  
  # Reactive to load fish dataset
  fish_data <- reactive({
    if (!file.exists(file_path)) {
      stop("The dataset file was not found at the specified path.")
    }
    ext <- tools::file_ext(file_path)
    if (ext == "csv") {
      read.csv(file_path)
    } else if (ext == "xlsx") {
      readxl::read_excel(file_path, sheet = "catchdata")
    } else {
      stop("Unsupported file format. Please use a CSV or Excel file.")
    }
  })
  # load depth station info
  depth_data <- reactive({
    if (!file.exists(file_path)) {
      stop("The dataset file was not found at the specified path.")
    }
    ext <- tools::file_ext(file_path)
    if (ext == "csv") {
      read.csv(file_path)
    } else if (ext == "xlsx") {
      readxl::read_excel(file_path, sheet = "station_depth")
    } else {
      stop("Unsupported file format. Please use a CSV or Excel file.")
    }
  })
  # load stratum info
  stratum_data <- reactive({
    if (!file.exists(file_path)) {
      stop("The dataset file was not found at the specified path.")
    }
    ext <- tools::file_ext(file_path)
    if (ext == "csv") {
      read.csv(file_path)
    } else if (ext == "xlsx") {
      readxl::read_excel(file_path, sheet = "stratum_info")
    } else {
      stop("Unsupported file format. Please use a CSV or Excel file.")
    }
  })
  # Reactive to load env dataset
  env_data <- reactive({
    if (!file.exists(file_path2)) {
      stop("The dataset file was not found at the specified path.")
    }
    ext <- tools::file_ext(file_path2)
    if (ext == "csv") {
      read.csv(file_path2)
    } else if (ext == "xlsx") {
      readxl::read_excel(file_path2)
    } else {
      stop("Unsupported file format. Please use a CSV or Excel file.")
    }
  })
  # load AMSY stock info 
  ALL_IDnorway <- reactive({
    if (!file.exists(file_path3)) {
      stop("The dataset file was not found at the specified path.")
    }
    ext <- tools::file_ext(file_path3)
    if (ext == "csv") {
      read.csv(file_path3)
    } else if (ext == "xlsx") {
      readxl::read_excel(file_path3)
    } else {
      stop("Unsupported file format. Please use a CSV or Excel file.")
    }
  })
 
  # dropdown menu page
  output$dynamic_panel <- renderUI({
    switch(input$page_select,
           
           "Sampling Area Map" = fluidPage(
             fluidRow(column(12, p("The sub-Arctic fjords system of Sørfolda, including its branches Leirfjorden and Aspfjorden, is a dynamic coastal environment in Northern Norway known for its complex hydrography and ecological significance. The system is ice-free throughout the year and features a deep basin (up to 574 meters in Sørfolda) with a sill depth of 265 meters, which limits water exchange and creates a highly stratified water column (Myksvoll et al., 2011). This stratification, driven by freshwater runoff from surrounding rivers and glaciers, results in a thin low-salinity surface layer and a unique estuarine circulation (seasonal variation in surface water temperature and salinity)."))), 
            fluidRow(column(12, plotOutput("samplingmap", width = "100%"))),
            
             fluidRow(column(12, p("Commercial fishing activity in the Sørfolda fjord system is relatively limited, largely due to its steep topography and deep basins, which make the area less suitable for intensive trawling or large-scale operations. The small local fishing fleet based in the region primarily operates in waters closer to the Lofoten area. Within Sørfolda itself, fishing is predominantly small-scale and localized, with activities focused on recreational angling and small-boat operations targeting species such as cod and pollock in the shallower, more accessible areas of the fjord. Sørfolda serves as a critical spawning and nursery habitat for Norwegian coastal cod, whose eggs are retained within the fjord due to their buoyancy and the circulation patterns, supporting genetic differentiation among cod populations (Myksvoll et al., 2011). However, hydropower developments in the region have regulated freshwater flow into Sørfolda, altering its estuarine circulation and reducing retention of coastal cod eggs (Myksvoll et al., 2014). Additionally, the area supports diverse benthic communities, including macrobenthic and infaunal species, which exhibit spatial heterogeneity due to sediment composition and water properties (Kokarev et al., 2021). These features, combined with the presence of aquaculture facilities (four on Sørfolda, one on the south side of Leirfjorden, and another two near the entrance of Aspfjorden) and their potential environmental impacts (e.g., salmon lice dispersion; Johnsen et al., 2016), highlight Sørfolda's importance as a focal point for integrated marine research making this fjord system an essential area for biodiversity conservation."))),
            ),
           
           "Leader Board" = fluidPage(
             fluidRow(column(12, h3("Biodiversity Prize"))),
             fluidRow(column(12, textOutput("biodiversity_prize"))),
             fluidRow(column(12, h3("Conservation Prize"))),
             fluidRow(column(12, textOutput("conservation_prize"))),
             fluidRow(column(12, h3("Top 10 Longest Fish (cm)"))),
             fluidRow(column(12, tableOutput("longest_fish_table"))),
             fluidRow(column(12, h3("Top 10 Heaviest Fish (g)"))),
             fluidRow(column(12, tableOutput("heaviest_fish_table")))
           ),
          
           
           "Catch Plot" = fluidPage(
             fluidRow(column(12, p("A catch plot is a simple time-series of catches per sampling day (date on the x-axis, catch on the y-axis). Who, how, where and when the catch happened."))),
             fluidRow(column(12, imageOutput("abundance_plot", width = "100%")))
           ),
           
           "Catch & Release vs Retained" = fluidPage(
             fluidRow(column(12, p("The Catch & Release vs. Retained plot shows, for each sampling day, how many fish were released versus kept. Releasing fish properly helps them survive and grow—good for future catches and a healthy stock."))),
             fluidRow(column(12, plotOutput("cr_plot", width = "100%")))
           ),
           
           "Length Frequency Distribution (LFD)" = fluidPage(
             fluidRow(column(12, p("It’s simply a bar chart showing how many fish fall into each length class in our data. It summarizes the population’s size structure at a glance, revealing typical size, pulses of small recruits, presence/loss of big fish, and it’s what we use to compute indicators like L50 (median) and L90 (large-fish size)."))),
             fluidRow(column(12, textOutput("longest_fish_info"))),
             fluidRow(column(12, plotOutput("lfd_plot", width = "100%")))
           ),
           
           "Size-based indicators (L50 - L90)" = fluidPage(
             fluidRow(column(12, p("What do L50 and L90 show?
L50 is the typical size (median length) in the catch; L90 is the size of larger fish (90th percentile). Rising L50/L90 suggests bigger fish are more common (often good); falling values suggest size truncation or many small recruits. The Kendall test checks whether values consistently go up (Positive) or down (Negative) over time and tells us if that trend is likely real/significant or just random noise (Not significant)."))),
             fluidRow(column(12, plotOutput("L50_plot", width = "100%"))),
             fluidRow(column(12, plotOutput("L90_plot", width = "100%")))
           ),
           
           "Length-Weight (L-W) relationship" = fluidPage(
             fluidRow(column(12, p("In fishery biology, the length-weight (LW) relationship describes how a fish's weight changes with its length. It's a crucial tool for understanding fish growth patterns, assessing their condition, and estimating biomass."))),
             fluidRow(column(12, uiOutput("species_filter_single"))),
             fluidRow(column(12, plotOutput("lw_plot", width = "100%"))),
             fluidRow(column(12, p("In the length–weight formula W = a·Lᵇ, the exponent b tells how weight scales with length.

b ≈ 3 (isometric): fish keep the same shape as they grow.

b > 3 (positive allometry): fish get relatively plumper as they grow (weight rises faster than length).

b < 3 (negative allometry): fish get relatively slimmer as they grow (weight rises slower than length).
                                   ")))
           ),
           
           "CPUE trend" = fluidPage(
             fluidRow(column(12, p("CPUE, or Catch Per Unit Effort, is a key metric in fisheries and conservation biology that measures the number of fish caught relative to a specific unit of fishing effort (in our case number of fisherman per number of fishing hours). It serves as an indirect indicator of fish population abundance and is used to monitor fish stock health and assess the effectiveness of fishing management strategies."))),
             fluidRow(column(12, plotOutput("cpue_plot", width = "100%"))),
             fluidRow(column(12, p("Increased CPUE: May indicate a recovering fish stock, allowing for increased fishing or potentially indicating more efficient fishing practices. 
Decreased CPUE: Could suggest a decline in fish population, potentially due to overfishing or other factors. ")))
           ),
           
           "Environmental Variables" = fluidPage(
             fluidRow(column(12, p("These plots show the environmental conditions collected during the survey over time."))),
             fluidRow(column(12, plotOutput("env_plot", width = "100%")))
           ),
           
           "Stock Assessment (AMSY)" = fluidPage(
             fluidRow(column(12, p(
               "The Abundance Maximum Sustainable Yield method (AMSY; ",
               tags$a(href = "https://doi.org/10.1093/icesjms/fsz230", "Froese et al. 2020", target = "_blank"),
               ") is a data-limited assessment model that uses trends in CPUE (or other relative abundance data) plus general knowledge of a species’ resilience to generate estimates of stock health. It tells us whether biomass is likely above or below levels needed for sustainable fishing, even when total catch data are absent. While AMSY is less precise than full data-rich assessments, it gives managers a useful starting point, especially in data-poor situations"
             ))),
             
             fluidRow(column(12, uiOutput("species_filter_single2"))), # Species
            # fluidRow(column(12, uiOutput("resilience_ui"))), # Resilience prior
             fluidRow(column(12, uiOutput("bkpr_ui"))), # stock size prior
             # Add the button that triggers the model
            fluidRow(
              column(
                12,
                div(
                  style = "text-align:center; margin-top:15px;",
                  actionButton(
                    inputId = "run_model",
                    label = "RUN model",
                    icon = icon("play"),
                    style = "padding: 12px 24px; font-size: 18px;"  # bigger button
                  )
                )
              )
            ),
            # fluidRow(column(12, plotOutput("amsyPlot1", width = "100%"))),
             fluidRow(column(12, plotOutput("amsyPlot2", height = "420px",width = "100%"))),
             fluidRow(column(12, p("• Relative stock size (B/Bmsy): how current biomass compares to the level that would produce maximum sustainable yield."))),
            fluidRow(column(12, p("• Relative fishing mortality (F/Fmsy): how current fishing mortality compares to the sustainable rate ")))
           ),
           
           "Environmental Influence Analysis" = fluidPage(
             fluidRow(column(12, p("Environmental-driver analysis helps us tell whether changes in CPUE are due to fihery or also changing conditions (like temperature or salinity). Here we use flexible Generalized Additive Models (GAMs) to show how CPUE varies with each driver. Total deviance explained says how much variation the model captures, and Variable Contributions rank which drivers matter the most."))),
             fluidRow(column(12, uiOutput("species_filter_single2"))),
             fluidRow(column(12, plotOutput("gam_plot", width = "100%"))),
             fluidRow(column(12, textOutput("gam_details"))),
             fluidRow(column(12, textOutput("gam_message")))
           )
    )
  })
  
  # Dynamically generate filters based on uploaded data
  output$fisherman_filter <- renderUI({
    req(fish_data())
    pickerInput("fisherman", "Select Fisherman:", 
                choices = unique(fish_data()$Fisherman), multiple = TRUE)
  })
  
  output$species_filter_multi <- renderUI({
    req(fish_data())
    pickerInput("species_multi", "Select Species (Multiple):", 
                choices = unique(fish_data()$Species), multiple = TRUE)
  })
  
  output$species_filter_single <- renderUI({
    req(fish_data())
    species_list <- unique(fish_data()$Species)
    pickerInput("species_single", "Species (all available data will be used):", 
                choices = species_list, selected = species_list[1], multiple = F)
  })
  
  output$species_filter_single2 <- renderUI({
    req(fish_data())
    species_list <- unique(fish_data()$Species)
    pickerInput("species_single", "Species (all available data will be used):", 
                choices = species_list, selected = species_list[1], multiple = F)
  })
  
  output$gear_filter <- renderUI({
    req(fish_data())
    pickerInput("gear", "Select Fishing Technique:", 
                choices = unique(fish_data()$FishingGear), multiple = TRUE)
  })
  
  output$station_filter <- renderUI({
    req(fish_data())
    pickerInput("station", "Select Station:", 
                choices = unique(fish_data()$Station), multiple = TRUE)
  })
  
  # Filtered data based on user inputs
  filtered_data_multi <- reactive({
    req(fish_data())
    data <- fish_data()
    # Ensure Date column is in Date format
    data <- data %>% mutate(Date = as.Date(Date))
    
    # Add a column for Abundance (if not present)
    if (!"Abundance" %in% colnames(data)) {
      if ("Count" %in% colnames(data)) {
        data <- data %>% rename(Abundance = Count)
      } else {
        # If no Abundance or Count column exists, assume each row is one fish
        data <- data %>% mutate(Abundance = 1)
      }
    }
    
    
    # Apply user filters
    if (!is.null(input$fisherman)) {
      data <- data %>% filter(Fisherman %in% input$fisherman)
    }
    if (!is.null(input$species_multi)) {
      data <- data %>% filter(Species %in% input$species_multi)
    }
    if (!is.null(input$gear)) {
      data <- data %>% filter(FishingGear %in% input$gear)
    }
    if (!is.null(input$station)) {
      data <- data %>% filter(Station %in% input$station)
    }
    if (!is.null(input$date_filter)) {
      data <- data %>% filter(Date >= input$date_filter[1] & Date <= input$date_filter[2])
    }
    
    data
  })
  
  # Filtered data for L-W Plot (single species)
  filtered_data_single  <- reactive({
    req(fish_data())
    data <- fish_data()
    # Ensure Date column is in Date format
    data <- data %>% mutate(Date = as.Date(Date))
    
    # Apply filters for single species
    if (!is.null(input$species_single)) {
      data <- data %>% filter(Species == input$species_single)
    }
    if (!is.null(input$date_filter)) {
      data <- data %>% filter(Date >= input$date_filter[1] & Date <= input$date_filter[2])
    }
    
    data
  })
  
  # Filtered environmental data
  filtered_env_data <- reactive({
    req(env_data())
    data <- env_data()
    
    # Apply date filter
    if (!is.null(input$date_filter)) {
      data <- data %>% filter(Date >= input$date_filter[1] & Date <= input$date_filter[2])
    }
    
    # Apply station filter
    if (!is.null(input$station)) {
      data <- data %>% filter(Station %in% input$station)
    }
    
    # Calculate average across stations for each Date
    data %>%
      group_by(Date) %>%
      summarise(
        Avg_Temp = mean(Temp, na.rm = TRUE),
        Avg_Salinity = mean(Salinity, na.rm = TRUE),
        Avg_Height = mean(Tidal_Height, na.rm = TRUE)
      )
  })
  
  ####################################################
  # Merge datasets on Date and Station for CPUE
  
  # CPUE Daily timeseries 
  cpue_station <- reactive({
    req(fish_data(), env_data())  # Ensure fish_data and env_data are available)
    data <- fish_data() 
    dataenv <- env_data() %>%
      group_by(Date, Station) %>%
      summarise(
        Temp = mean(Temp, na.rm = TRUE),  # Average temperature
        Salinity = mean(Salinity, na.rm = TRUE),  # Average salinity
        Tidal_Height = mean(Tidal_Height, na.rm = TRUE)  # Average tidal height
      ) %>%
      ungroup()
    data <- data %>% inner_join(dataenv, by = c("Date", "Station"))
    
    # Calculate average across stations for each Date
    all_combinations <- expand.grid(
      Date = unique(data$Date),
      Station = unique(data$Station),
      Species = unique(data$Species)
    )
    
    # Summarize the actual data
    summarized_data <- data %>%
      group_by(Date, Station, Species) %>%
      summarise(
        Total_Abundance = n(),  # Total number of fish caught
        Num_Fishermen = n_distinct(Fisherman),  # Number of unique fishermen
        fishing_hr = as.numeric(difftime(End, Start, units = "hours")) ,   # Fishing time
        Avg_Temp = mean(Temp, na.rm = TRUE),  # Average temperature
        Avg_Salinity = mean(Salinity, na.rm = TRUE),  # Average salinity
        Avg_Height = mean(Tidal_Height, na.rm = TRUE)  # Average tidal height
      ) %>%
      ungroup()
    
    # Summarize fishermen and environmental data at the station level
    station_summary <- data %>%
      group_by(Date, Station) %>%
      summarise(
        Num_Fishermen = n_distinct(Fisherman),  # Unique fishermen
        fishing_hr = mean(as.numeric(difftime(End, Start, units = "hours")) ), # Fishing time
        Avg_Temp = mean(Temp, na.rm = TRUE),
        Avg_Salinity = mean(Salinity, na.rm = TRUE),
        Avg_Height = mean(Tidal_Height, na.rm = TRUE)
      ) %>%
      ungroup()
    
    # Combine all combinations with the summarized catch data
    filled_data <- all_combinations %>%
      left_join(summarized_data, by = c("Date", "Station", "Species")) %>%
      left_join(station_summary, by = c("Date", "Station"), suffix = c("", "_station")) %>%
      mutate(
        Total_Abundance = coalesce(Total_Abundance, 0),  # Replace missing catches with 0
        Num_Fishermen = coalesce(Num_Fishermen_station, 0),      # Use station-level Num_Fishermen
        fishing_hr = coalesce(fishing_hr_station, 0),      # Use station-level fishing hr
        Avg_Temp = coalesce(Avg_Temp, Avg_Temp_station),  # Use station-level temperature
        Avg_Salinity = coalesce(Avg_Salinity, Avg_Salinity_station),
        Avg_Height = coalesce(Avg_Height, Avg_Height_station),
        CPUE = ifelse(Num_Fishermen > 0, Total_Abundance / (Num_Fishermen*fishing_hr), 0)  # Calculate CPUE
      ) %>%    select(-ends_with("_station"))   # Remove redundant station-level column
    # Return the completed dataset
    filled_data
    
  })
  
  cpue_monthly <- reactive({
    req(fish_data(), depth_data(), stratum_data() )  # Ensure fish_data and env_data are available)
    data <- fish_data() # %>% inner_join(env_data(), by = c("Date", "Station"))
    depth_station <- depth_data()
    stratum_info <- stratum_data()
    
    # extract month fom Date
    #data$Year_Month <- (format(data$Date, "%Y-%m"))
    Season =  quarter(data$Date)
    Year = year(data$Date) # Extract year
    data$Year_season <- paste0(Year,"-" ,Season)
    
    # Calculate average across stations for each Date
    all_combinations <- expand.grid(
      Year_season = unique(data$Year_season),
      Station = unique(data$Station),
      Species = unique(data$Species)
    )
    
    # Summarize the actual data
    summarized_data <- data %>%
      group_by(Year_season, Station, Species) %>%
      dplyr::summarise(
        Total_Abundance = n(),  # Total number of fish caught
        Num_Fishermen = n_distinct(Fisherman),  # Number of unique fishermen
        fishing_hr = as.numeric(difftime(End, Start, units = "hours"))    # Fishing time
      ) %>%
      ungroup()
    
    # Summarize fishermen and environmental data at the station level
    station_summary <- data %>%
      group_by(Year_season, Station) %>%
      summarise(
        Num_Fishermen = n_distinct(Fisherman),  # Unique fishermen
        fishing_hr = sum(unique(as.numeric(difftime(End, Start, units = "hours")) ) )   # Fishing time
      ) %>%
      ungroup()
    
    # Combine all combinations with the summarized catch data
    filled_data <- all_combinations %>%
      left_join(summarized_data, by = c("Year_season", "Station", "Species")) %>%
      left_join(station_summary, by = c("Year_season", "Station"), suffix = c("", "_station")) %>%
      mutate(
        Total_Abundance = coalesce(Total_Abundance, 0),  # Replace missing catches with 0
        Num_Fishermen = coalesce(Num_Fishermen_station, 0),      # Use station-level Num_Fishermen
        fishing_hr = coalesce(fishing_hr_station, 0),      # Use station-level fishing hr
        CPUE = ifelse(Num_Fishermen > 0, Total_Abundance / (Num_Fishermen*fishing_hr), 0)  # Calculate CPUE
      ) %>%
      select(-ends_with("_station"))  # Remove redundant station-level column
    # Return the completed dataset
    
    # read Stratum file
    stations_with_stratum <- depth_station %>%
      left_join(stratum_info, by = character()) %>%   # Cartesian join
      filter(Depth >= MinDepth & Depth <= MaxDepth) %>%
      select(Station, Depth, Stratum, Area, AreaWeight)
    
    # Weighted CPUE by DEPTH Stratum
    filled_data_monthly_stratum<- filled_data %>% left_join(stations_with_stratum)  %>% dplyr::group_by(Year_season, Stratum, Species) %>% dplyr::summarise( Total_CPUE_stratum = sum(CPUE*AreaWeight)  ) %>%
      ungroup() # Sum CPUE across stratum
    filled_data_monthly <- filled_data_monthly_stratum %>% dplyr::group_by(Year_season, Species) %>% dplyr::summarise( Total_CPUE = sum(Total_CPUE_stratum)  ) %>%
      ungroup() # Sum CPUE 
    filled_data_monthly
    
  })
  
  #############################################################
  #############################################################
  # AMSY PART 
  # Read the ID file once for UI defaults (adjust path if needed)
  cinfo_data <- reactive({
    read.csv("amsy_folder/ALL_IDnorway.csv", header = TRUE, stringsAsFactors = FALSE)
  })
  
  # Build the Resilience selectInput with the stock's default
  output$resilience_ui <- renderUI({
    req(cinfo_data(), input$species_single)
    df <- cinfo_data()
    current <- df$Resilience[df$Stock == input$species_single]
    # fallback if not found
    default_val <- if (length(current) && !is.na(current[1])) current[1] else "Medium"
    
    selectInput(
      inputId = "resilience",
      label   = "Resilience",
      choices = c("Very low","Low","Medium","High"),
      selected = default_val,
      width = "300px"
    )
  })
  
  # Build the Bk.pr selectInput with the stock's default from file
  output$bkpr_ui <- renderUI({
    req(cinfo_data(), input$species_single)
    df <- cinfo_data()
    current <- df$Bk.pr[df$Stock == input$species_single]
    default_val <- if (length(current) && !is.na(current[1])) current[1] else "About half"
    
    tagList(
      selectInput(
        inputId = "bk_pr",
        label   = "Relative stock size (biomass prior)",
        choices = c("Near unexploited","More than half","About half","Small","Very small"),
        selected = default_val,
        width = "300px"
      ),
      tags$small(
        "A prior for relative stock size can be derived from experts/fishermen who are asked how stock size was in a year of their choice compared to past stock size when there was little fishing of the species. For example, if the stock was only lightly fished in the beginning of the time series, it is reasonable to assume that stock size was more than half of the unexploited level in those years."
      )
    )
  })
  
  aAMSY <- eventReactive(input$run_model,{
    req(fish_data(), depth_data(), stratum_data() , input$species_single)  # Ensure fish_data and env_data are available)
    
    #  Show a progress bar while running
    withProgress(message = "Running AMSY…", value = 0, {
      incProgress(0.1, detail = "Preparing data")
      
      data <- fish_data() 
      depth_station <- depth_data()
      stratum_info <- stratum_data()
      
      # extract month from Date
      #data$Year_Month <- (format(data$Date, "%Y-%m"))
      Season =  quarter(data$Date)
      Year = year(data$Date) # Extract year
      data$Year_season <- paste0(Year,"-" ,Season)
      
      # Calculate average across stations for each Date
      all_combinations <- expand.grid(
        Year_season = unique(data$Year_season),
        Station = unique(data$Station),
        Species = unique(data$Species)
      )
      
      # Summarize the actual data
      summarized_data <- data %>%
        group_by(Year_season, Station, Species) %>%
        dplyr::summarise(
          Total_Abundance = n(),  # Total number of fish caught
          Num_Fishermen = n_distinct(Fisherman),  # Number of unique fishermen
          fishing_hr = as.numeric(difftime(End, Start, units = "hours"))    # Fishing time
        ) %>%
        ungroup()
      
      # Summarize fishermen and environmental data at the station level
      station_summary <- data %>%
        group_by(Year_season, Station) %>%
        summarise(
          Num_Fishermen = n_distinct(Fisherman),  # Unique fishermen
          fishing_hr = sum(unique(as.numeric(difftime(End, Start, units = "hours")) ) )   # Fishing time
        ) %>%
        ungroup()
      
      # Combine all combinations with the summarized catch data
      filled_data <- all_combinations %>%
        left_join(summarized_data, by = c("Year_season", "Station", "Species")) %>%
        left_join(station_summary, by = c("Year_season", "Station"), suffix = c("", "_station")) %>%
        mutate(
          Total_Abundance = coalesce(Total_Abundance, 0),  # Replace missing catches with 0
          Num_Fishermen = coalesce(Num_Fishermen_station, 0),      # Use station-level Num_Fishermen
          fishing_hr = coalesce(fishing_hr_station, 0),      # Use station-level fishing hr
          CPUE = ifelse(Num_Fishermen > 0, Total_Abundance / (Num_Fishermen*fishing_hr), 0)  # Calculate CPUE
        ) %>%
        select(-ends_with("_station"))  # Remove redundant station-level column
      # Return the completed dataset
      
      # read Stratum file
      stations_with_stratum <- depth_station %>%
        left_join(stratum_info, by = character()) %>%   # Cartesian join
        filter(Depth >= MinDepth & Depth <= MaxDepth) %>%
        select(Station, Depth, Stratum, Area, AreaWeight)
      
      # Weighted CPUE by DEPTH Stratum
      filled_data_monthly_stratum<- filled_data %>% left_join(stations_with_stratum)  %>% dplyr::group_by(Year_season, Stratum, Species) %>% dplyr::summarise( Total_CPUE_stratum = sum(CPUE*AreaWeight)  ) %>%
        ungroup() # Sum CPUE across stratum
      filled_data_monthly <- filled_data_monthly_stratum %>% dplyr::group_by(Year_season, Species) %>% dplyr::summarise( Total_CPUE = sum(Total_CPUE_stratum)  ) %>%
        ungroup() # Sum CPUE 
      
      cpue_db <- filled_data_monthly %>%
        # Ensure Year_season is ordered
        arrange(Year_season) %>%
        
        # Create sequential year index (1 to N) based on Year_season
        mutate(
          Year = as.integer(factor(Year_season, levels = unique(Year_season))),
          Stock = Species,
          CPUE = Total_CPUE,
          Catch = NA
        ) %>%
        select(Stock, Year, Catch, CPUE, Year_season)
      
      cpue_amsy <- cpue_db %>% select(-last_col())
      
      incProgress(0.35, detail = "Running Monte Carlo filtering")
      
      # write.csv(cpue_amsy, "amsy_folder/Cpue_amsy.csv", row.names = FALSE)
      
      # AMSY MODEL   ####################
      # source("amsy_model.R", local = F) 
      ###################################
      ##---------------------------------------------------------------------------------------------
      ## AMSY
      ## Original code written by Rainer Froese in January - April 2019
      ## Additons by Henning Winker:
      ## 1. Implemented process error on process equation
      ## 2. Process error is implemented as sigme.R = c(0.05,0.07,0.1,0.15) for Very low, Low, Medium, High.
      ## 3. Schaefer function now greps sigma.R as defined 
      ## 4. Observation error implemented as CV (log.sd of CPUE)
      ## 5. Implemented Kobe prototype with terminal F/Fmsy taken as mean of previous 3 yrs.
      ## 6. Added save.plot option from CMSY
      ## 7. Added automatic package installer
      ## Additions by Gianpaolo Coro:
      ## 1. improved estimate of prior kq  
      ## 2. retrospective analysis
      ## Addition by RF: MVN
      ##---------------------------------------------------------------------------------------------
      
      #-----------------------------------------
      # Some general settings ----
      #-----------------------------------------
      # set.seed(999) # use for comparing results between runs
      #rm(list=ls(all=FALSE)) # clear previous variables etc
      options(digits=3) # displays all numbers with three significant digits as default
      #graphics.off() # close graphics windows from previous sessions
      #setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # set working directory to source file location
      
      #-----------------------------------------
      # Required settings, File names 
      #-----------------------------------------
      id_file     <- "amsy_folder/ALL_IDnorway.csv" #   name of file containing stock-specific info and settings for the analysis
      outfile     <- paste("Out_",format(Sys.Date(),format="%B%d%Y_"),id_file,sep="") # default name for output file
      
      #----------------------------------------
      # Select stock to be analyzed ----
      #----------------------------------------
      stocks      <-    input$species_single
      # If the input files contain more than one stock, specify below the stock to be analyzed
      # If the line below is commented out (#), all stocks in the input file will be analyzed
      # stocks <- "DIPL.ANN"   #"HL_VL"   #c("anb-78ab")  # c("HH_VL","HL_VL","HLH_VL","LH_VL","LHL_VL","LL_VL") # "anb-78ab"  #c("HL_H","HL_M","HL_L","HL_VL")  #"HH_L"  #"LHL_L"  #"Micr_pou_AD" #"Myxine glutinosa"   #"Eut_gurn_Balt"  # "rjc.27.3a47d"  #"PNSK"   # "Ille_coi_AD" # "WSTM"
      
      # Read data
      cinfo       <- read.csv(id_file, header=T, dec=".",  stringsAsFactors = FALSE)
      cat("File", id_file, "read successfully","\n")
      
      #-----------------------------------------
      # General settings for the analysis ----
      #-----------------------------------------
      smooth.cpue  <- F # set to TRUE to apply ksmooth with minimum bandwidth of 3, increased at low r
      filter       <- TRUE # set to TRUE for Monte Carlo filtering; if FALSE, incease max.viable to 20000
      cor.log.rk   <- -0.607 #-0.871 # empirical value of log r-k correlation in 140 stocks analyzed with BSM
      sigma.r      <- c(0.05,0.07,0.1,0.15) # very low, low, medium, high # overall process error for productivity or r
      sigma.cpue   <- 0.1 # observation error for cpue 
      n.p          <- 50000 # number of r-kq pairs to be analysed; will be doubled if too few viable pairs are found
      n.trial      <- 30 # times each year is calculated with new random error terms for r and cpue
      min.viable   <- 20 # minimum number of viable r-kq pairs to be accepted for analysis
      max.viable   <- 5000 # maximum number of viable r-kq pairs to reduce processing time; set to 20000 if filter==FALSE
      creep.graph  <- T # plot graph for effort creep correction, if used
      do.plots     <- T # retrospective analysis does not work if FALSE
      write.output <- T # set to TRUE if table with results in output file is wanted 
      kobe.plot    <- T # HW set to TRUE so produce additional kobe status plot 
      save.plots   <- F # set to TRUE to save graphs to JPEG files
      close.plots  <- T # set to TRUE to close on-screen plots, to avoid "too many open devices" error in batch-processing;
      retros       <- F # retrospective analysis, requires do.plots <- TRUE
      
      # Additional settings
      save_extra_info=T
      need_raising='T'
      area=91300 # area of the basin
      save_trajectories=T
      #----------------------------------------------
      #  FUNCTIONS ----
      #----------------------------------------------
      # Monte Carlo filtering with Schaefer Function ----
      #----------------------------------------------
      
      SchaeferCPUE<-function(yr, cpue, ri, kqi, sigR, filter){
        # create matrix for results
        mdat  <- matrix(ncol = (2*nyr+1))
        colnames(mdat) <- c("rv","kqv",paste("c",yr[1:(nyr-1)],sep=""),paste("b",yr[1:nyr],sep=""))
        
        for(i in 1:length(ri)) { # for all submitted r-kq pairs
          
          for(trial in 1:n.trial) { # rerun every r-kq pair several times because error terms per year are random
            # max one succesful run across all years is returned per trial
            cqt         <- vector()
            cpuet       <- vector()
            FFmsy       <- vector()
            break.flag  <- FALSE
            
            for(t in 1:(nyr-1))  {  # for all years except the last one, for which catch cannot be calculated
              # assign random error terms to surplus production and to cpue  
              err      <- exp(rnorm(1,0,sigR)) # set annual error for productivity  
              cpuet[t] <- cpue[t]*exp(rnorm(1,0,sigma.cpue)) # assign error to cpue
              if(cpuet[t]<=0) {cpuet[t] <- 0.01*kqi[i]} # make sure cpuet is not zero or negative
              
              # calculate catch
              if(cpuet[t]/kqi[i] >= 0.25) {
                cqt[t] <- (cpuet[t] + cpuet[t] * ri[i] * (1-cpuet[t]/kqi[i]))*err - cpue[t+1]  } else {
                  cqt[t] <- (cpuet[t] + cpuet[t] * ri[i] * (1-cpuet[t]/kqi[i])*(4*cpuet[t]/kqi[i]))*err - cpue[t+1] } # reduce r linearly below 0.25 kq
              
              # use moving average to smooth hectic catch predictions
              if(t == 2) {cqt[t] <- mean(c(cqt[t-1],cqt[t])) }
              if(t > 2) {cqt[t] <- mean(c(cqt[t-2],cqt[t-1],cqt[t])) }
              
              # calculate MSYq and F/Fmsy
              MSYq     <- ri[i]*kqi[i]/4 
              FFmsy[t] <- 2*cqt[t]/(ri[i]*cpue[t])
              
              if(filter==TRUE) {
                ## Test compatibility of r-kq pairs with general prior popdyn knowledge
                ## If one test fails, break the loop and go to the next trial  
                # (1) Exclude r-kq pair if catch is negative (cqt[t] < 0) 
                mult.kqi    <- ifelse(res=="Very low",-0.06,ifelse(res=="Low",-0.02,0)) #relax rule for Very low and Low resilience
                if(cqt[t] < mult.kqi*kqi[i]) {break.flag<-TRUE;break}
                
                # (2) Exclude r-kq pair if catch exceeds biomass (cqt[t] > cpue[t])
                # if lowest cpue is close to zero, skip this test
                if(min(cpue.raw) > 0.1*max.cpue) {
                  # in highly productive species, catch may exceed average annual biomass
                  mult.cpue  <- ifelse(res=="High",1.4,ifelse(res=="Medium",1,ifelse(res=="Low",0.5,0.25))) 
                  if(cqt[t] > (mult.cpue*cpuet[t])) {break.flag<-TRUE;break} }
                
                # (3) Exclude r-kq pair if catch exceeds MSY   
                # some overshooting of MSY is possible
                mult.msy <- ifelse(res=="Very low",10,ifelse(res=="Low",5,ifelse(res=="Medium",3,2))) 
                if(cqt[t] > mult.msy*MSYq)        {break.flag<-TRUE;break} 
                
                # (4) Exclude r-k pairs if F/Fmsy is highly unrealistic (negative or much too high)
                FFlow  <- ifelse(res=="Very low",-25,-3) 
                FFhi   <- ifelse(res=="Very low",12,5)   
                if(t > 1 && (FFmsy[t-1] < FFlow || FFmsy[t-1] > FFhi)) {break.flag<-TRUE;break}
                
                # (5) if relative cpue in the year of the B/k prior is outside of the prior range, discard trial
                #  relax rule if lower B/k prior range is <= 0.01    
                if(prior.Bk[1] <= 0.01) { prior.Bk[1] <- 0.0001}
                if(t==Bk.yr.i && (cpuet[Bk.yr.i]/kqi[i] < prior.Bk[1] || cpuet[Bk.yr.i]/kqi[i] > prior.Bk[2])) {break.flag<-TRUE;break }
                
              } # end of condition for filtering
              
            } # end of t-loop through years
            
            # if t-loop was broken and flag==TRUE do not test further, do not plot points, do not store results 
            if(break.flag==TRUE) { next }
            
            # assign error to last cpue and repeat filter (7) if applicable to last year
            cpuet[nyr] <- cpue[nyr]*exp(rnorm(1,0,sigma.cpue)) 
            if(cpuet[nyr]<=0) {cpuet[nyr] <- 0.01*kqi[i]} # make sure cpuet is not zero or negative
            if(filter==TRUE && Bk.yr.i==nyr && (cpuet[nyr]/kqi[i] < prior.Bk[1] || cpuet[nyr]/kqi[i] > prior.Bk[2])) { next }   
            
            # If all tests are passed, add viable r-kq pair and predicted catch to matrix
            mdat     <- rbind(mdat,c(ri[i],kqi[i],cqt[1:(nyr-1)],cpuet[1:(nyr)]))
            
            # plot viable r-kq pairs
            if(do.plots==T) { }
            
          } # end of trial-loop for trials per r-kq pair
          if(length(mdat[,1])>max.viable) { break} # end searching for viable pairs if n > max.viable
        } # end of i-loop through r-kq pairs
        
        mdat <- na.omit(mdat)
        return(mdat)
        
      } # end of SchaeferCPUE function
      
      #-------------------------------------------------------------
      # Function to create multivariate-normal distribution for r-k 
      #-------------------------------------------------------------
      mvn   <- function(n,mean.log.r,sd.log.r,mean.log.kq,sd.log.kq) {
        cov.log.rk <- cor.log.rk*sd.log.r*sd.log.kq # covariance with empirical correlation and prior variances  covar.log.rk = matrix(NA, ncol=2,nrow=2)   # contract covariance matrix
        covar.log.rk      <- matrix(NA, ncol=2,nrow=2) # covariance matrix
        covar.log.rk[1,1] <- sd.log.r^2                # position [1,1] is variance of log.r
        covar.log.rk[2,2] <- sd.log.kq^2               # position [2,2] is variance of log.k
        covar.log.rk[1,2] = covar.log.rk[2,1] = cov.log.rk     # positions [1,2] and [2,1] are correlations
        mu.log.rk  <- (c(mean.log.r,mean.log.kq))      # vector of log.means
        mvn.log.rk <- rmvnorm(n,mean=mu.log.rk,sigma=covar.log.rk,method="svd") 
        return(mvn.log.rk)
      }
      
      
      
      # Trajectories extraction ####
      trajectories_fun=function(Bdat, Fdat, Xstock){
        
        Btrajectory=data.frame(Bdat)
        colnames(Btrajectory)="BBmsy"
        Btrajectory$year=rownames(Btrajectory)
        
        Ftrajectory=data.frame(Fdat)
        Ftrajectory$year=rownames(Ftrajectory)
        
        s_trajectories=merge(Btrajectory, Ftrajectory, by="year", all.x = T)
        s_trajectories$species=stock
        return(s_trajectories)
        #write.csv(s_trajectories,paste0("Stock_trajectories/Stock_trajectories_",Xstock, ".csv"))
        
      }  
      
      # Parameters extraction ####
      parameters_fun=function(r_dat, b_dat, cpue_dat, k_dat, f_dat, Xstock, xarea){
        
        s_parameters=data.frame(r=r_dat,  
                                Fmsy=r_dat/2, 
                                Bbmsy=b_dat ,
                                B=cpue_dat, 
                                Bmsy=(k_dat/2), 
                                Fcur = f_dat)
        s_parameters$species=Xstock
        
        if(need_raising=="T"){
          s_parameters$B=raising(s_parameters$B, area)
          s_parameters$Bmsy=raising(s_parameters$Bmsy, area)
        }
        return(s_parameters)
        #write.csv(s_parameters,paste0("Stock_parameters/Stock_parameters_",stock,".csv"))
        
      }
      
      
      # rise kg/km2 to absolute tonnes ####
      raising=function(xb, xkm){
        v=(xb*xkm)/100
        return(v)} 
      
      #---------------------------------------------
      # END OF FUNCTIONS
      #---------------------------------------------
      
      #--------------------------------------------
      # Create table for output to csv file
      #--------------------------------------------
      #if(save_extra_info==T){source('supp_funs.R')}
      if(save_extra_info==T){}
      
      if(write.output==T && substr(id_file,1,3)=="Sim"){ # output for simulated data
        outheaders = data.frame("Stock","r.true", "r.est","r.lcl","r.ucl",
                                "kq.true","kq.est","kq.lcl","kq.ucl",
                                "MSYq.true","MSYq.est","MSYq.lcl","MSYq.ucl",
                                "FFmsy.true","FFmsy.est","FFmsy.lcl","FFmsy.ucl",
                                "BBmsy.true","BBmsy.est","BBmsy.lcl","BBmsy.ucl")
        #write.table(outheaders,file=outfile, append = T, sep=",",row.names=F,col.names=F) 
      }
      
      
      if(write.output==TRUE && is.null(cinfo$MSY.BSM)==F) { # assuming all BSM fields are available
        outheaders = data.frame("Stock","r.BSM","lcl","ucl","r.est","lcl","ucl","k.BSM","lcl","ucl","k.est","lcl","ucl",
                                "BBmsy.BSM","lcl","ucl","BBmsy.est","lcl","ucl",
                                "FFmsy.BSM","lcl","ucl",
                                "FFmsy.est","lcl","ucl") 
        #write.table(outheaders,file=outfile, append = T, sep=",",row.names=F,col.names=F)
      }
      
      if(write.output==T && substr(id_file,1,3)!="Sim" && is.null(cinfo$MSY.BSM)==T){
        outheaders = data.frame("Stock","Fmsy.est","Fmsy.lcl","Fmsy.ucl",
                                "FFmsy.est","FFmsy.lcl","FFmsy.ucl",
                                "BBmsy.est","BBmsy.lcl","BBmsy.ucl")
        #write.table(outheaders,file=outfile, append = T, sep=",",row.names=F,col.names=F)
      }
      
      #-----------------------------------------
      # Start output to screen
      #-----------------------------------------
      #cat("------------------------------------------------------------\n")
      #cat("AMSY Analysis,", date(),"\n")
      
      #---------------------------------
      # Analyze stock(s)
      #---------------------------------
      if(is.na(stocks[1])==TRUE){
        stocks         <- as.character(cinfo$Stock) # Analyze stocks in sequence of ID file
        # stocks          <- cinfo$Stock[cinfo$Stock >= "fle-2425"] # Analyze stocks in sequence of ID file
        # stocks         <- sort(as.character(cinfo$Stock)) # Analyze stocks in alphabetic order
      }
      # analyze one stock after the other
      for(stock in stocks) {
        
        #retrospective analysis
        if(retros==T && (cinfo$EndYear[cinfo$Stock==stock]-cinfo$Bk.yr[cinfo$Stock==stock])<3) {
          retros.nyears<-0 #retrospective analysis
          cat("Warning: Retrospective analysis not meaningful and omitted if B/k prior is in the final year(s)\n") } else {
            retros.nyears<-ifelse(retros==T,3,0) #retrospective analysis
          }
        
        FFmsy.retrospective<-list() #retrospective analysis
        BBmsy.retrospective<-list() #retrospective analysis
        years.retrospective<-list() #retrospective analysis
        
        for (retrosp.step in 0:retros.nyears){ #retrospective analysis
          
          cat("------------------------------------------------------------\n")
          cat("Stock ",bold(stock),", ", bold(italic(as.character(cinfo$ScientificName[cinfo$Stock==stock]))),", ",
              as.character(cinfo$EnglishName[cinfo$Stock==stock]),sep="","\n")
          # read data for stock
          cpue_file    <- cinfo$CPUE_File[cinfo$Stock==stock]
          # cdat         <- read.csv( paste0("amsy_folder/",  cpue_file), header=T, dec=".", stringsAsFactors = FALSE)
          cdat         <-  cpue_amsy
          
          # assign data from cinfo to vectors
          n            <- n.p
          #res          <- as.character(cinfo$Resilience[cinfo$Stock==stock])
          # prefer user selection, fallback to file value
          res_from_file <- as.character(cinfo$Resilience[cinfo$Stock == stock])
          res <- if (!is.null(input$resilience) && nzchar(input$resilience)) input$resilience else res_from_file
          res.i <- match(res, c("Very low","Low","Medium","High"))
          if (is.na(res.i)) stop("Resilience must be one of: Very low, Low, Medium, High")
          #res.i        <- which(c("Very low","Low","Medium","High")%in%res) # determines process error strength
          if(length(res.i)==0) {stop("Spelling error in resilience in ID file\n")}
          start.yr     <- as.numeric(cinfo$StartYear[cinfo$Stock==stock])
          end.yr       <- as.numeric(cinfo$EndYear[cinfo$Stock==stock])
          end.yr 	     <- end.yr-retrosp.step #retrospective analysis
          r.low        <- as.numeric(cinfo$r.low[cinfo$Stock==stock])
          r.hi         <- as.numeric(cinfo$r.hi[cinfo$Stock==stock])
          user.log.r   <- ifelse(is.na(r.low)==F & is.na(r.hi)==F,TRUE,FALSE)     
          Bk.yr        <- as.numeric(cinfo$Bk.yr[cinfo$Stock==stock])
          # --- OVERRIDE with user choice if provided ---
          if (!is.null(input$bk_pr) && nzchar(input$bk_pr)) {
            Bk.pr     <- input$bk_pr
            Bk.pr.low <- NA_real_
            Bk.pr.hi  <- NA_real_
          }
          stopifnot(Bk.pr %in% c("Near unexploited","More than half","About half","Small","Very small"))
          #Bk.pr        <- as.character(cinfo$Bk.pr[cinfo$Stock==stock])
          #Bk.pr.low    <- as.numeric(cinfo$Bk.pr.low[cinfo$Stock==stock])
          #Bk.pr.hi     <- as.numeric(cinfo$Bk.pr.hi[cinfo$Stock==stock])
          e.creep      <- as.numeric(cinfo$e.creep[cinfo$Stock==stock])
          comment      <- as.character(cinfo$Comment[cinfo$Stock==stock])
          Fmsy.ass     <- as.numeric(cinfo$Fmsy.ass[cinfo$Stock==stock])
          Bmsy.ass     <- as.numeric(cinfo$Bmsy.ass[cinfo$Stock==stock])
          source       <- as.character(cinfo$Source[cinfo$Stock==stock])
          
          
          # check for common errors
          if(length(r.low)==0){
            cat("ERROR: Could not find the stock in the ID input file - check that the stock names match in ID and CPUE files and that commas are used (not semi-colon)")
            return (NA) }
          if(length(cdat$Year[cdat$Stock==stock])==0){
            cat("ERROR: Could not find the stock in the CPUE file - check that the stock names match in ID and CPUE files and that commas are used (not semi-colon)")
            return (NA) }
          if(start.yr < cdat$Year[cdat$Stock==stock][1]){
            cat("ERROR: start year in ID file before first year in CPUE file\n")
            return (NA)}
          if(!(Bk.pr %in% c("Near unexploited","More than half","About half","Small","Very small"))){
            cat("ERROR: Prior for stock size not in: Near unexploited, More than half, About half, Small, Very small\n")
            return (NA)}
          if(Bk.yr < start.yr || Bk.yr > end.yr){
            cat("ERROR: Year for B/k prior outside range of years\n")
            return (NA)}
          
          #----------------------------------------------------
          # Determine initial ranges for r
          #----------------------------------------------------
          # initial range of r from input file
          if(is.na(r.low)==F & is.na(r.hi)==F) {
            prior.r <- c(r.low,r.hi)
          } else {
            # initial range of r based on resilience
            if(res == "High") {
              prior.r <- c(0.6,1.5)} else if(res == "Medium") {
                prior.r <- c(0.2,0.8)}    else if(res == "Low") {
                  prior.r <- c(0.05,0.5)}  else { # i.e. res== "Very low"
                    prior.r <- c(0.015,0.1)} 
          }
          
          #--------------------------------------
          # extract data on stock
          #--------------------------------------
          yr           <- as.numeric(cdat$Year[cdat$Stock==stock & cdat$Year >= start.yr & cdat$Year <= end.yr])
          nyr          <- length(yr) # number of years in the time series
          Bk.yr.i      <- which(yr==Bk.yr)
          cpue.raw     <- as.numeric(cdat$CPUE[cdat$Stock==stock & cdat$Year >= start.yr & cdat$Year <= end.yr])
          
          # get catch from full assessments or simulations if available, for comparison
          if(is.null(cdat$Catch[cdat$Stock==stock][1])==F && is.na(cdat$Catch[cdat$Stock==stock][1])==F){
            C.ass <- cdat$Catch[cdat$Stock==stock & cdat$Year >= start.yr & cdat$Year <= end.yr] } else {C.ass <- NA}
          
          # apply correction for effort-creep to commercial(!) CPUE if indicated by user
          if(is.na(e.creep)==FALSE) {
            cpue.cor         <- cpue.raw
            for(i in 1:(length(cpue.raw)-1)) {
              cpue.cor[i+1]  <- cpue.raw[i+1]*(1-e.creep/100)^i # equation for decay in %; first cpue without correction
            }
            if(creep.graph==TRUE) {
              windows(8,6)
              plot(x=yr,y=cpue.raw,ylim=c(0,max(cpue.raw)),type="l",bty="l",xlab="Year",ylab="CPUE")
              lines(x=yr,y=cpue.cor,col="red")
              text(x=yr[length(yr)/2],y=max(cpue.raw),paste(stock," CPUE corrected for effort creep of ",e.creep," %",sep=""),col="red")
            }
            cpue.raw <- cpue.cor
          }
          
          d.cpue.raw   <- max(diff(cpue.raw)/cpue.raw[1:(nyr-1)])
          if(smooth.cpue==T||d.cpue.raw > 1.5) {
            smooth.flag <- TRUE
            bw          <- log(2)/exp(mean(log(prior.r))) # use population doubling time as bandwidth
            bw          <- ifelse(bw < 3,3,bw) # enforce minimum bandwidth of 3
            cpue        <- ksmooth(x=yr,y=cpue.raw,kernel="normal",n.points=length(yr),bandwidth=bw)$y  } else {
              smooth.flag <- FALSE
              cpue <- cpue.raw }
          # use median of 3 largest cpue as max cpue
          max.cpue     <- sort(cpue)[length(cpue)-1] 
          min.cpue     <- min(cpue)
          if(length(Fmsy.ass)>0 && is.null(Fmsy.ass)==F && is.na(Fmsy.ass)==F && is.null(cdat$F[1])==F) {
            FFmsy.ass     <- as.numeric(cdat$F[cdat$Stock==stock & cdat$Year >= start.yr & cdat$Year <= end.yr]/Fmsy.ass)
            if(is.null(cdat$FLow[1])==F) {FFmsy.ass.lcl <- as.numeric(cdat$FLow[cdat$Stock==stock & cdat$Year >= start.yr & cdat$Year <= end.yr]/Fmsy.ass) }
            if(is.null(cdat$FHi[1])==F) {FFmsy.ass.ucl <- as.numeric(cdat$FHi[cdat$Stock==stock & cdat$Year >= start.yr & cdat$Year <= end.yr]/Fmsy.ass) }
          } else {Fmsy.ass <- NA; FFmsy.ass <- NA;FFmsy.ass.lcl <- NA;FFmsy.ass.ucl <- NA}
          
          if(length(Bmsy.ass)>0 && is.null(Bmsy.ass)==F && is.na(Bmsy.ass)==F && is.null(cdat$B[1])==F) {
            BBmsy.ass     <- as.numeric(cdat$B[cdat$Stock==stock & cdat$Year >= start.yr & cdat$Year <= end.yr]/Bmsy.ass)
            if(is.null(cdat$BLow[1])==F) {BBmsy.ass.ucl <- as.numeric(cdat$BLow[cdat$Stock==stock & cdat$Year >= start.yr & cdat$Year <= end.yr]/Bmsy.ass) }
            if(is.null(cdat$BHi[1])==F) {BBmsy.ass.lcl <- as.numeric(cdat$BHi[cdat$Stock==stock & cdat$Year >= start.yr & cdat$Year <= end.yr]/Bmsy.ass) }
          } else {Bmsy.ass <- NA; BBmsy.ass <- NA;BBmsy.ass.lcl <- NA;BBmsy.ass.ucl <- NA}
          
          if(length(yr)==0){
            cat("ERROR: Could not find the stock in the CPUE input files - Please check that the stock ID is written correctly")
            return (NA) }
          # if(length(yr) != (end.yr-start.yr+1)) {
          #   cat("ERROR: indicated year range is of different length than years in CPUE file\n")
          #   return (NA)}
          
          #----------------------------------------------------
          # Determine ranges for relative biomass
          #----------------------------------------------------
          # initial range of B/k from input file
          # NEW: always use the categorical mapping when user picks Bk.pr
          if (!is.na(Bk.pr.low) && !is.na(Bk.pr.hi) && is.null(input$bk_pr)) {
            # use file's numeric bounds only if user did NOT override
            prior.Bk <- c(Bk.pr.low, Bk.pr.hi)
          } else {
            prior.Bk <- switch(
              Bk.pr,
              "Near unexploited" = c(0.75, 1.0),
              "More than half"   = c(0.5,  0.85),
              "About half"       = c(0.35, 0.65),
              "Small"            = c(0.15, 0.4),
              "Very small"       = c(0.01, 0.2)
            )
          }
          # us relative range of B/k as relative range for kq
          mean.prior.Bk   <- mean(prior.Bk)
          rr.prior.Bk     <- (mean.prior.Bk-prior.Bk[1])/mean.prior.Bk 
          
          prior.kq.low.1  <- (1-rr.prior.Bk)*cpue[Bk.yr.i]/mean.prior.Bk
          prior.kq.hi.1   <- (1+rr.prior.Bk)*cpue[Bk.yr.i]/mean.prior.Bk
          
          # kq must be > max cpue unless near unexploited
          prior.kq.low.2  <- ifelse(prior.kq.low.1 < max.cpue,ifelse(mean.prior.Bk >= 0.85,0.9*max.cpue,max.cpue),prior.kq.low.1) 
          
          # increase lower kq prior if cpue is small and flat
          if((max(cpue)/min(cpue))<2) {
            prior.kq.low <- ifelse(mean.prior.Bk < 0.3,2*prior.kq.low.2,
                                   ifelse(mean.prior.Bk < 0.6,1.5*prior.kq.low.2,prior.kq.low.2))
          } else {prior.kq.low <- prior.kq.low.2 }
          
          # kq.hi at least 30-50% larger than kq.low, depending on Bk prior  
          if(mean.prior.Bk >= 0.6) {
            prior.kq.hi.2  <- ifelse(prior.kq.hi.1 < (1.3*prior.kq.low),1.3*prior.kq.low,prior.kq.hi.1) } else {
              prior.kq.hi.2  <- ifelse(prior.kq.hi.1 < (1.5*prior.kq.low),1.5*prior.kq.low,prior.kq.hi.1) }
          
          # if upper prior kq is too hi, limit to 3 times lower range 
          prior.kq.hi   <- ifelse(prior.kq.hi.2 > (3*prior.kq.low),3*prior.kq.low,prior.kq.hi.2)
          
          prior.kq           <- c(prior.kq.low,prior.kq.hi)
          
          #------------------------------------------------------------------
          # Sampling of r-k space 
          #------------------------------------------------------------------
          # turn numerical ranges into log-normal distributions 
          
          mean.log.r=mean(log(prior.r))
          sd.log.r=(log(prior.r[2])-log(prior.r[1]))/4  # assume range covers 4 SD
          
          mean.log.kq <- mean(log(prior.kq))
          sd.log.kq   <- (log(prior.kq[2])-log(prior.kq[1]))/4 # assume range covers 4 SD
          
          mvn.log.rk <- mvn(n=n,mean.log.r=mean.log.r,sd.log.r=sd.log.r,mean.log.kq=mean.log.kq,sd.log.kq=sd.log.kq)
          ri1    <- exp(mvn.log.rk[,1])
          kqi1   <- exp(mvn.log.rk[,2])
          
          #------------------------------------------------------------------
          # print prior info on screen
          #------------------------------------------------------------------
          cat("CPUE data for years ",yr[1]," - ",yr[nyr],", CPUE range ",min.cpue," - ",max(cpue),", smooth = ",smooth.flag,sep="","\n")
          cat("Prior for r                   = ",res,", ", r.low," - ",r.hi,sep="","\n")
          if(is.na(r.low)==T) {
            cat("Used prior range for r        = ", prior.r[1]," - ",prior.r[2],sep="","\n") } else {
              cat("Used prior range for r        = ", quantile(ri1,0.01)," - ",quantile(ri1,0.99),sep="","\n") }
          cat("Prior for ",Bk.yr," stock status   = ", Bk.pr,", ",Bk.pr.low," - ",Bk.pr.hi,sep="","\n") 
          cat("Used ",Bk.yr," prior B/B0 range    = ",prior.Bk[1]," - ",prior.Bk[2],", prior B/Bmsy = ",2*prior.Bk[1]," - ",2*prior.Bk[2],sep="","\n")
          cat("Used prior range for kq       = ",prior.kq[1]," - ",prior.kq[2]," [original range = ",prior.kq.low.1," - ",prior.kq.hi.1,"]\n",sep="") 
          if(is.na(Fmsy.ass)==F) {cat("Assessment Fmsy               =",Fmsy.ass,"\n")}
          if(is.na(FFmsy.ass[1])==F) {cat("Assessment F/Fmsy             =",FFmsy.ass[nyr-1],ifelse(is.na(FFmsy.ass.lcl[1])==F,
                                                                                                    paste(",",format(FFmsy.ass.lcl[nyr-1],digits = 2),"-",format(FFmsy.ass.ucl[nyr-1],digits=2),"")),
                                          "(",yr[nyr-1],")\n")}
          if(is.na(Bmsy.ass)==F) {cat("Assessment proxy Bmsy         =",Bmsy.ass,"\n")}
          if(is.na(BBmsy.ass[1])==F) {cat("Assessment proxy B/Bmsy       =",BBmsy.ass[nyr],ifelse(is.na(BBmsy.ass.lcl[1])==F,
                                                                                                  paste(",",format(BBmsy.ass.lcl[nyr],digits = 2),"-",format(BBmsy.ass.ucl[nyr],digits=2),"")),
                                          "(",yr[nyr],")\n")
            cat("Source:",source,"\n")}
          
          if(is.null(cinfo$MSY.BSM[cinfo$Stock==stock])==F && is.na(cinfo$MSY.BSM[cinfo$Stock==stock])==F) { # assume all info from BSM analysis is available
            cat("BSM r                         =", cinfo$r.BSM[cinfo$Stock==stock],",",cinfo$r.BSM.lcl[cinfo$Stock==stock],
                "-",cinfo$r.BSM.ucl[cinfo$Stock==stock],"\n")
            cat("BSM k                         =", cinfo$k.BSM[cinfo$Stock==stock]*1000,",",cinfo$k.BSM.lcl[cinfo$Stock==stock]*1000,
                "-",cinfo$k.BSM.ucl[cinfo$Stock==stock]*1000,"\n")
            cat("BSM MSY                       =", cinfo$MSY.BSM[cinfo$Stock==stock]*1000,",",cinfo$MSY.BSM.lcl[cinfo$Stock==stock]*1000,
                "-",cinfo$MSY.BSM.ucl[cinfo$Stock==stock]*1000, "\n")
            cat("BSM last B/Bmsy               =", cinfo$B_Bmsy[cinfo$Stock==stock],",",cinfo$B_Bmsy.lcl[cinfo$Stock==stock],
                "-",cinfo$B_Bmsy.ucl[cinfo$Stock==stock], "\n")
            cat("BSM last F/Fmsy               =", cinfo$F_Fmsy[cinfo$Stock==stock],",",cinfo$F_Fmsy.lcl[cinfo$Stock==stock],
                "-",cinfo$F_Fmsy.ucl[cinfo$Stock==stock], "\n")
          }
          cat("Comment:",comment,"\n")
          if(is.na(source)==F) {cat("Source:",source,"\n")}
          cat("\n")
          #-----------------------------------------------------------------
          # Plot CPUE data and prior CPUE_msy
          #-----------------------------------------------------------------
          if(close.plots==T) {graphics.off()} # close previous plots, e.g. in batch processing
          if(do.plots==T) {
            
            # check for operating system, open separate window for graphs if Windows
            #if(grepl("win",tolower(Sys.info()['sysname']))) {windows(14,9)}
            
          } # end of do.plots loop
          
          #---------------------------------------------------------------------
          # Call AMSY-Schaefer function to filter r-kq space for viable r-kq pairs
          #---------------------------------------------------------------------
          cat("Monte Carlo filtering of r-kq space with",n,"points and",n.trial,"error patterns. \n")
          MCA1 <-  SchaeferCPUE(yr=yr, cpue=cpue, ri=ri1, kqi=kqi1, sigR=sigma.r[res.i], filter=filter) #><> correct PE input
          
          n.viable <- length(MCA1[,"rv"])
          cat("Viable r-kq pairs =",n.viable,"\n") 
          
          if((n.viable)<min.viable) {
            cat("Too few r-kq pairs after filtering, repeating analysis with 2 times more pairs, extended prior ranges, and increased smoothing:\n")
            mvn.log.rk2 <- mvn(n=2*n,mean.log.r=mean.log.r,sd.log.r=1.2*sd.log.r,mean.log.kq=mean.log.kq,sd.log.kq=1.2*sd.log.kq)
            ri2  <- exp(mvn.log.rk2[,1])
            kqi2 <- exp(mvn.log.rk2[,2])
            cpue <- ksmooth(x=yr,y=cpue.raw,kernel="normal",n.points=length(yr),bandwidth=5)$y
            
            MCA2 <-  SchaeferCPUE(yr=yr, cpue=cpue, ri=ri2, kqi=kqi2, sigR=sigma.r[res.i], filter=filter) 
            MCA  <- rbind(MCA1,MCA2)
            n.viable <- length(MCA[,"rv"])
            cat("Viable r-kq pairs =",n.viable,"\n") } else {MCA <- MCA1}
          
          if((n.viable)<10) {
            cat("Too few r-kq pairs after filtering, doing analysis without filters:\n")
            MCA <-  SchaeferCPUE(yr=yr, cpue=cpue, ri=ri2, kqi=kqi2, sigR=sigma.r[res.i], filter=FALSE) 
          }
          
          rv       <- MCA[,"rv"]
          kqv      <- MCA[,"kqv"]
          
          MSYqv     <- rv * kqv / 4
          MSYq.est  <- median(MSYqv)    
          MSYq.lcl  <- as.numeric(quantile(MSYqv,0.025))      
          MSYq.ucl  <- as.numeric(quantile(MSYqv,0.975))      
          
          n.v        <- length(MSYqv)
          
          kqv.est    <- median(kqv)
          kqv.lcl  <- as.numeric(quantile(kqv,0.025))
          kqv.ucl  <- as.numeric(quantile(kqv,0.975))
          
          rv.est     <- 4*MSYq.est/kqv.est    # rv corresponding to median(kqv)
          rv.lcl   <- as.numeric(quantile(rv,0.025))
          rv.ucl   <- as.numeric(quantile(rv,0.975))
          
          cqt.sel           <- matrix(nrow=length(rv),ncol=nyr-1)
          colnames(cqt.sel) <- c(yr[1:nyr-1])
          for(j in 1:(nyr-1)) {
            cqt.sel[,j]     <- MCA[,j+2]}
          cqt.median        <- apply(cqt.sel,2,median)
          cqt.lcl           <- apply(cqt.sel,2,quantile,probs=0.025)
          cqt.ucl           <- apply(cqt.sel,2,quantile,probs=0.975)
          
          cpuet.sel           <- matrix(nrow=length(rv),ncol=nyr)
          colnames(cpuet.sel) <- c(yr[1:nyr])
          for(j in 1:(nyr)) {
            cpuet.sel[,j]     <- MCA[,j+2+nyr-1]}
          cpuet.median        <- apply(cpuet.sel,2,median)
          cpuet.lcl           <- apply(cpuet.sel,2,quantile,probs=0.025)
          cpuet.ucl           <- apply(cpuet.sel,2,quantile,probs=0.975)
          
          BBmsy.end       <- cpuet.median[nyr]/(kqv.est/2)
          BBmsy.end.lcl   <- cpuet.lcl[nyr]/(kqv.est/2)
          BBmsy.end.ucl   <- cpuet.ucl[nyr]/(kqv.est/2)
          
          
          Ft            <- cqt.median[1:(nyr-1)]/cpuet.median[1:(nyr-1)]
          FFmsy         <- Ft/(0.5*rv.est)
          FFmsy.end     <- FFmsy[nyr-1]
          Ft.lcl        <- cqt.lcl[1:(nyr-1)]/cpuet.median[1:(nyr-1)]
          FFmsy.lcl     <- Ft.lcl/(0.5*rv.est)
          FFmsy.end.lcl <- FFmsy.lcl[nyr-1]
          Ft.ucl        <- cqt.ucl[1:(nyr-1)]/cpuet.median[1:(nyr-1)]
          FFmsy.ucl     <- Ft.ucl/(0.5*rv.est)
          FFmsy.end.ucl <- FFmsy.ucl[nyr-1]
          
          if(substr(id_file,1,3)=="Sim"){ # if dealing with simulated data, get the "true" values
            MSYq.true      <- cinfo$true.MSYq[cinfo$Stock==stock]
            r.true         <- as.numeric(cinfo$true.r[cinfo$Stock==stock])
            kq.true        <- as.numeric(cinfo$true.kq[cinfo$Stock==stock])
            cqt.true       <- as.numeric(cdat$Catch[cdat$Stock==stock & cdat$Year >= start.yr & cdat$Year <= end.yr])
            Ft.true        <- cqt.true/cpuet.median
            FFmsy.true     <- Ft.true/(0.5*cinfo$true.r[cinfo$Stock==stock])
            FFmsy.end.true <- FFmsy.true[nyr-1]
            BBmsy.end.true <- cinfo$true.Bk.end[cinfo$Stock==stock]*2
          } else {FFmsy.true <- NA; FFmsy.end.true <- NA; cqt.true <- NA}
          
          cat("\n Results:",
              "\n viable r-kq pairs   = ",n.v,
              "\n median kq           = ",kqv.est,", ",kqv.lcl," - ",kqv.ucl,
              "\n median MSYq         = ",MSYq.est,", ",MSYq.lcl," - ",MSYq.ucl,
              "\n r (4 MSYq/kq)       = ",rv.est,", ",rv.lcl," - ",rv.ucl,
              "\n Fmsy (r/2)          = ",rv.est/2,", ",rv.lcl/2," - ",rv.ucl/2,
              "\n F/Fmsy              = ",FFmsy.end,", ",FFmsy.end.lcl," - ",FFmsy.end.ucl," (",yr[nyr-1],")",
              ifelse(substr(id_file,1,3)=="Sim",paste(", true:",format(FFmsy.end.true,digits = 3)),""),
              "\n B/Bmsy              = ",BBmsy.end,", ",BBmsy.end.lcl," - ",BBmsy.end.ucl," (",yr[nyr],")",
              ifelse(substr(id_file,1,3)=="Sim",paste(", true:",format(BBmsy.end.true,digits = 3)),""),
              "\n",sep="")
          
          # -----------------------------------------
          # Plot results 
          # -----------------------------------------
          if(do.plots==T) {
            
            
            # (d) Pred. rel. catch plot 
            #--------------------
            # get data from full assessments if available
            C_MSY.ass <- NA
            if(is.na(C.ass[1])==F && is.na(Fmsy.ass)==F && is.na(Bmsy.ass)==F) {
              MSY.ass   <- Fmsy.ass*Bmsy.ass
              C_MSY.ass <- C.ass/MSY.ass
            }
            # get data from BSM if available
            if(is.null(cinfo$MSY.BSM[cinfo$Stock==stock])==F) { 
              C_MSY.ass <- C.ass/(cinfo$MSY.BSM[cinfo$Stock==stock]*1000)
            }
            
            # determine height of y-axis in plot
            max.y  <- max(c(cqt.ucl/MSYq.est,1.1,cqt.true/MSYq.est,C_MSY.ass), na.rm=T)
            # Main plot of relative CMSY catch up to last year because the schaefer equation is not reliable in nyr
            
            # plot true catch * q from simulations
            if(substr(id_file,1,3)=="Sim") {
              cqt.true <- as.numeric(cdat$Catch[cdat$Stock==stock & cdat$Year >= start.yr & cdat$Year <= end.yr])
            } 
            
            
            # plot (e): F/Fmsy
            #---------------
            max.y <- max(c(1.2,FFmsy.ucl,FFmsy.true,FFmsy.ass.ucl),na.rm=T)
            
            
            # plot (f): B/Bmsy
            #---------------
            Bkt        <- cpuet.median/(0.5*kqv.est)
            Bmsy.true  <- cinfo$true.kq[cinfo$Stock==stock]/kqv.est
            if(is.na(C.ass[1])==F && is.null(cinfo$r.BSM[cinfo$Stock==stock])==F && substr(id_file,1,4)=="CMSY") {
              BBmsy.BSM  <- cpue.raw / (cinfo$k.BSM[cinfo$Stock==stock]*1000 / 2) } else { BBmsy.BSM <- NA }
            max.y      <- max(c(Bkt, kqv.ucl/kqv.est,Bmsy.true,cpue.raw/(kqv.est/2),cpuet.ucl/(0.5*kqv.est),BBmsy.ass.ucl,BBmsy.BSM),na.rm=T)
            
            
            
            if (save.plots==TRUE & do.plots==TRUE) {
              jpgfile<-paste(stock,"_AMSY.jpg",sep="")
              dev.copy(jpeg,jpgfile,
                       width = 1024, 
                       height = 768, 
                       units = "px", 
                       pointsize = 18,
                       quality = 95,
                       res=80,
                       antialias="cleartype")
              dev.off()
            }
            
            if(kobe.plot==T){
              kobe_plot_gg <- function(cpuet.sel,
                                       cqt.sel,
                                       kqv.est,
                                       rv.est,
                                       harvest.label = "Fmsy",
                                       Bkt,
                                       FFmsy,
                                       yr,
                                       start.yr,
                                       end.yr,
                                       nyr = length(yr),
                                       n_sims = 10000,
                                       ci.levels = c(0.50, 0.80, 0.95),
                                       grid_n = 151,
                                       point_size = 2.6,
                                       seed = NULL) {
                
                pkgs <- c("mvtnorm","MASS","ggplot2","dplyr","tidyr")
                miss <- pkgs[!sapply(pkgs, requireNamespace, quietly = TRUE)]
                if (length(miss)) stop("Missing packages: ", paste(miss, collapse = ", "))
                
                if (!is.null(seed)) set.seed(seed)
                
                bbmsy <- (cpuet.sel[, nyr] / (0.5 * kqv.est))
                ffmsy <- ((apply(cqt.sel[, (nyr-4):(nyr-1), drop = FALSE], 1, median) / cpuet.sel[, nyr]) / (0.5 * rv.est))
                
                ok <- ffmsy > 0 & bbmsy > 0
                if (!any(ok)) stop("No positive BBMSY and FFMSY pairs found for log transform.")
                
                log.bbmsy <- log(bbmsy[ok])
                log.ffmsy <- log(ffmsy[ok])
                
                mu.kobe  <- c(median(log.ffmsy, na.rm = TRUE), median(log.bbmsy, na.rm = TRUE))
                cov.kobe <- cov(cbind(log.ffmsy, log.bbmsy), use = "complete.obs")
                
                log.kobe.mvn <- mvtnorm::rmvnorm(n_sims, mean = mu.kobe, sigma = cov.kobe)
                x_FFmsy <- exp(log.kobe.mvn[, 1])
                y_BBmsy <- exp(log.kobe.mvn[, 2])
                
                max_y <- max(c(2, quantile(x_FFmsy, 0.96, na.rm = TRUE)), na.rm = TRUE)
                max_x <- max(c(2, quantile(y_BBmsy, 0.999, na.rm = TRUE)), na.rm = TRUE)
                
                kde <- MASS::kde2d( x_FFmsy,y_BBmsy, n = grid_n, lims = c(0, max_x, 0, max_y))
                df_kde <- tidyr::expand_grid(x = kde$x, y = kde$y)
                df_kde$z <- as.vector(kde$z)
                
                get_hdr_levels <- function(kde, probs) {
                  dx <- mean(diff(kde$x))
                  dy <- mean(diff(kde$y))
                  z_vec <- as.vector(kde$z)
                  o <- order(z_vec, decreasing = TRUE)
                  mass <- cumsum(z_vec[o]) * dx * dy
                  sapply(probs, function(p) {
                    idx <- which(mass >= p)[1]
                    z_vec[o[idx]]
                  })
                }
                probs <- sort(ci.levels)
                thr   <- get_hdr_levels(kde, probs)
                breaks <- c( thr[3], thr[2], thr[1], Inf)
                labels <- c( "95% C.I.", "80% C.I.", "50% C.I.")
                
                df_traj <- data.frame(
                  B = Bkt,
                  F = c(FFmsy, median(x_FFmsy, na.rm = TRUE))[seq_along(Bkt)]
                )
                
                nB <- length(Bkt)
                nF <- length(FFmsy)
                F_traj <- c(FFmsy, median(x_FFmsy, na.rm = TRUE))
                F_traj <- F_traj[seq_len(nB)]
                df_traj <- data.frame(B = Bkt, F = F_traj)
                df_traj <- df_traj[is.finite(df_traj$B) & is.finite(df_traj$F), , drop = FALSE]
                
                # adjust limits to include trajectory
                max_x <- max(max_x, max(df_traj$B, na.rm = TRUE))
                max_y <- max(max_y, max(df_traj$F, na.rm = TRUE))
                
                Pr.green  <- mean(y_BBmsy > 1 & x_FFmsy < 1, na.rm = TRUE) * 100
                Pr.red    <- mean(y_BBmsy < 1 & x_FFmsy > 1, na.rm = TRUE) * 100
                Pr.yellow <- mean(y_BBmsy < 1 & x_FFmsy < 1, na.rm = TRUE) * 100
                Pr.orange <- mean(y_BBmsy > 1 & x_FFmsy > 1, na.rm = TRUE) * 100
                
                probs_df <- data.frame(
                  label = c(
                    sprintf("Green: %.1f%%", Pr.green),
                    sprintf("Yellow: %.1f%%", Pr.yellow),
                    sprintf("Orange: %.1f%%", Pr.orange),
                    sprintf("Red: %.1f%%", Pr.red)
                  ),
                  x = c(max_x * 0.75, max_x * 0.75, max_x * 0.75, max_x * 0.75),
                  y = c(max_y * 0.25, max_y * 0.20, max_y * 0.15, max_y * 0.10)
                )
                
                ylab_txt <- if (harvest.label == "Fmsy") expression(F/F[MSY]) else expression(H/H[MSY])
                
                p <- ggplot2::ggplot() +
                  annotate("rect", xmin = 1, xmax = max_x, ymin = 0, ymax = 1, fill = "green", alpha = 0.80) +
                  annotate("rect", xmin = 0, xmax = 1,     ymin = 0, ymax = 1, fill = "yellow", alpha = 0.80) +
                  annotate("rect", xmin = 1, xmax = max_x, ymin = 1, ymax = max_y, fill = "orange", alpha = 0.80) +
                  annotate("rect", xmin = 0, xmax = 1,     ymin = 1, ymax = max_y, fill = "red", alpha = 0.80) +
                  geom_contour_filled(data = df_kde, aes(x = x, y = y, z = z),
                                      breaks = breaks, alpha = 0.6) +
                  geom_hline(yintercept = 1, linetype = 3) +
                  geom_vline(xintercept = 1, linetype = 3) +
                  geom_path(data = df_traj, aes(B, F), na.rm = TRUE) +
                  geom_point(data = df_traj, aes(B, F), size = 1.8, na.rm = TRUE) +
                  geom_point(data = df_traj[1, , drop = FALSE], aes(B, F), shape = 22, fill = "white", size = point_size, na.rm = TRUE) +
                  geom_point(data = df_traj[length(Bkt), , drop = FALSE], aes(B, F), shape = 24, fill = "white", size = point_size, na.rm = TRUE) +
                  geom_text(data = probs_df, aes(x, y, label = label), inherit.aes = FALSE, hjust = 0, size = 4) +
                  scale_x_continuous(limits = c(0, max_x), expand = c(0, 0), name = expression(B/B[MSY])) +
                  scale_y_continuous(limits = c(0, max_y), expand = c(0, 0), name = ylab_txt) +
                  scale_fill_manual(values = c("grey70","grey50","grey30"), labels = labels, drop = FALSE) +
                  coord_cartesian(xlim = c(0, max_x), ylim = c(0, max_y), clip = "off") + 
                  theme_bw(base_size = 12) +  
                  ggtitle(stock)+
                  theme(
                    plot.title = element_text(hjust = 0.5),
                    legend.position = c(1, 1),
                    legend.justification = c(1, 1),
                    legend.background = ggplot2::element_rect(fill = ggplot2::alpha("white", 0.6), colour = NA)
                  ) 
              }
              
            } # End Kobe
            
            if (save.plots==TRUE & kobe.plot==TRUE) 
            {
              jpgfile<-paste(stock,"_KOBE.jpg",sep="")
              dev.copy(jpeg,jpgfile,
                       width = 1024*0.7, 
                       height = 1024*0.7, 
                       units = "px", 
                       pointsize = 18,
                       quality = 95,
                       res=80,
                       antialias="cleartype")
              dev.off()
            }  
            
            FFmsy.retrospective[[retrosp.step+1]]<-FFmsy #retrospective analysis
            BBmsy.retrospective[[retrosp.step+1]]<-Bkt #retrospective analysis
            years.retrospective[[retrosp.step+1]]<-yr #retrospective analysis
            
          } # end of do.plots loop
        } #retrospective analysis - end loop
        
        #retrospective analysis plots
        if (retros.nyears>0 && do.plots==T){
          
          if(grepl("win",tolower(Sys.info()['sysname']))) {windows(12,7)}
          par(mfrow=c(1,2))  
          
          allyears<-years.retrospective[[1]]
          nyrtotal<-length(allyears)
          plot(x=allyears[1:(nyrtotal-1)],y=FFmsy.retrospective[[1]], main=as.character(stock), ylim=c(0,max(FFmsy.retrospective[[1]],na.rm=T)), lwd=2, xlab="Year", ylab="F/Fmsy", type="l", bty="l",  cex.main = 1.5, cex.lab = 1.5, cex.axis = 1.5)
          lines(x=allyears[1:(nyrtotal-2)],y=FFmsy.retrospective[[2]], type = "o", pch=15, col="red")
          lines(x=allyears[1:(nyrtotal-3)],y=FFmsy.retrospective[[3]], type = "o", pch=16, col="green") 
          lines(x=allyears[1:(nyrtotal-4)],y=FFmsy.retrospective[[4]], type = "o", pch=17, col="blue") 
          legend("bottomleft", legend = c("Reference",allyears[nyrtotal-2],allyears[nyrtotal-3],allyears[nyrtotal-4]), 
                 col=c("black","red", "green", "blue"), lty=1, pch=c(-1,15,16,17))
          
          plot(x=allyears[1:(nyrtotal)],y=BBmsy.retrospective[[1]],main=as.character(stock), ylim=c(0,max(BBmsy.retrospective[[1]],na.rm=T)), lwd=2, xlab="Year", ylab="B/Bmsy", type="l", bty="l",cex.main = 1.5, cex.lab = 1.5, cex.axis = 1.5)
          lines(x=allyears[1:(nyrtotal-1)],y=BBmsy.retrospective[[2]], type = "o", pch=15, col="red")
          lines(x=allyears[1:(nyrtotal-2)],y=BBmsy.retrospective[[3]], type = "o", pch=16, col="green") 
          lines(x=allyears[1:(nyrtotal-3)],y=BBmsy.retrospective[[4]], type = "o", pch=17, col="blue") 
          legend("bottomleft", legend = c("Reference",allyears[nyrtotal-1],allyears[nyrtotal-2],allyears[nyrtotal-3]), 
                 col=c("black","red", "green", "blue"), lty=1, pch=c(-1,15,16,17))
        } #retrospective analysis plots - end
        
        
        #---------------------------------------	
        # write results to file
        #---------------------------------------	
        if(write.output==TRUE && substr(id_file,1,3)=="Sim") {
          output = data.frame(as.character(stock), 
                              r.true,rv.est,rv.lcl,rv.ucl,
                              kq.true,kqv.est,kqv.lcl,kqv.ucl,
                              cinfo$true.MSYq[cinfo$Stock==stock],
                              MSYq.est,MSYq.lcl,MSYq.ucl,
                              FFmsy.end.true,
                              FFmsy.end,FFmsy.end.lcl,FFmsy.end.ucl,
                              BBmsy.end.true,
                              BBmsy.end,BBmsy.end.lcl,BBmsy.end.ucl )
          
          #  write.table(output, file=outfile, append = T, sep = ",", 
          #      dec = ".", row.names = FALSE, col.names = FALSE)
        } 
        if(write.output==TRUE && is.null(cinfo$MSY.BSM[cinfo$Stock==stock])==F) { # assuming all BSM fields are available
          output = data.frame(as.character(stock), 
                              cinfo$r.BSM[cinfo$Stock==stock],cinfo$r.BSM.lcl[cinfo$Stock==stock],cinfo$r.BSM.ucl[cinfo$Stock==stock],
                              rv.est,rv.lcl,rv.ucl,
                              cinfo$k.BSM[cinfo$Stock==stock]*1000,cinfo$k.BSM.lcl[cinfo$Stock==stock]*1000,cinfo$k.BSM.ucl[cinfo$Stock==stock]*1000,
                              kqv.est,kqv.lcl,kqv.ucl,
                              cinfo$B_Bmsy[cinfo$Stock==stock],cinfo$B_Bmsy.lcl[cinfo$Stock==stock],cinfo$B_Bmsy.ucl[cinfo$Stock==stock],
                              BBmsy.end,BBmsy.end.lcl,BBmsy.end.ucl,
                              cinfo$F_Fmsy[cinfo$Stock==stock],cinfo$F_Fmsy.lcl[cinfo$Stock==stock],cinfo$F_Fmsy.ucl[cinfo$Stock==stock],
                              FFmsy[nyr-1],FFmsy.lcl[nyr-1],FFmsy.ucl[nyr-1])
          
          #write.table(output, file=outfile, append = T, sep = ",",dec = ".", row.names = FALSE, col.names = FALSE)
        } # end of BSM option to write results to file
        
        
        if(write.output==T && substr(id_file,1,3)!="Sim" && is.null(cinfo$MSY.BSM)==T){ # regular assessment
          output = data.frame(as.character(stock),rv.est/2,rv.lcl/2,rv.ucl/2,
                              kqv.est,kqv.lcl,kqv.ucl,
                              FFmsy[nyr-1],FFmsy.lcl[nyr-1],FFmsy.ucl[nyr-1],
                              cpue[nyr]/kqv.est,cpue[nyr]/kqv.ucl,cpue[nyr]/kqv.lcl)
          #write.table(output, file=outfile, append = T, sep = ",", 
          # dec = ".", row.names = FALSE, col.names = FALSE)
        } # end of regular output
        
        # Addition to extract additional results
        
        my_trj_result <- trajectories_fun(Bdat = BBmsy.retrospective,
                                          Fdat = FFmsy,
                                          Xstock = stock)
        
        #my_trj_result <- my_trj_result %>% left_join(cpue_db)
        
        # trajectories plot
        # amsy_plot <-ggpubr::ggarrange(
        #    ggplot(data=my_trj_result)+
        #      geom_line(aes(x=as.numeric(year), y=BBmsy, color=species))+
        #      ylab(expression('B/B'[MSY]))+
        #      geom_hline(yintercept = 1, linetype=2)+
        #      theme_bw()
        #    ,
        #    ggplot(data=my_trj_result)+
        #      geom_line(aes(x=as.numeric(year), y=Fdat, color=species))+
        #      ylab(expression('F/F'[MSY]))+
        #      theme_bw()+
        #      geom_hline(yintercept = 1, linetype=2),
        #    common.legend = T
        #    ,nrow=2)
        # print(amsy_plot)
        #	return(PLOT)
        
      } # end loop on stocks
      
      kobe <- kobe_plot_gg(cpuet.sel = cpuet.sel,
                           cqt.sel   = cqt.sel,
                           kqv.est   = kqv.est,
                           rv.est    = rv.est,
                           harvest.label = "Fmsy",
                           Bkt = Bkt,
                           FFmsy = FFmsy,
                           yr = yr,
                           start.yr = start.yr,
                           end.yr = end.yr,
                           #   filename = "kobe_plot_gg.png",
                           seed = 42) 
      
      incProgress(0.9, detail = "Finalizing")
      # Return what the outputs need:
      return(kobe)
      
    })
    
  }, ignoreInit = T)  
 
  
  
  #######################################################  
  #######################################################
  #######################################################
  # Compute the longest fish
  longest_fish <- reactive({
    req(filtered_data_multi())
    data <- filtered_data_multi()
    
    # Find the row with the longest fish
    if (nrow(data) > 0) {
      longest <- data[which.max(data$Length), ]
      paste0("The longest fish caught is ", round(longest$Length, 2), 
             " cm, caught by ", longest$Fisherman, 
             " (Species: ", longest$Species, ").")
    } else {
      "No data available for the selected filters."
    }
  })
  
  # Display the longest fish information
  output$longest_fish_info <- renderText({
    longest_fish()
  })
  
  ####################################################
  # Generate the LFD plot
  output$lfd_plot <- renderPlot({
    req(filtered_data_multi())
    data <- filtered_data_multi()
    
    ggplot(data, aes(x = Length, fill = Species, color = Species)) +
      geom_histogram(binwidth=0.5, alpha = 0.5, position = "identity") +
      theme_bw() +
      labs(title = "Length Frequency Distribution",
           x = "Length (cm)", y = "Frequency",
           fill = "Species")  +
      scale_x_continuous(breaks = seq(floor(min(data$Length, na.rm = T)), ceiling(max(data$Length, na.rm = T)), by = 2))+   # 1 cm tick marks
      theme(legend.position = "bottom",axis.text.x = element_text(angle = 90, hjust = 1))
  })
  ###############################################  
  
  # --- shared prep so we don't repeat heavy work ---
  l5090_prep <- reactive({
    req(fish_data())
    data <- fish_data()
    
    dataL50 <- data %>%
      mutate(
        Season      = quarter(Date),
        Year        = year(Date),
        Year_season = sprintf("%d-Q%d", Year, Season)
      ) %>%
      group_by(Year, Season, Year_season, Species) %>%
      summarise(
        LMean = as.numeric(mean(Length, na.rm = TRUE)),
        L50 = as.numeric(quantile(Length, 0.5, na.rm = TRUE)),
        L90 = as.numeric(quantile(Length, 0.9, na.rm = TRUE)),
        .groups = "drop"
      ) %>%
      mutate(QuarterDate = as.Date(sprintf("%d-%02d-01", Year, (Season - 1) * 3 + 1)))
    
    trend_fun <- function(df, yvar, alpha = 0.05) {
      df %>%
        group_by(Species) %>%
        summarise(
          tau = {
            x <- as.numeric(QuarterDate); y <- .data[[yvar]]
            keep <- is.finite(x) & is.finite(y)
            if (sum(keep) >= 3 && dplyr::n_distinct(x[keep]) >= 2)
              suppressWarnings(cor.test(x[keep], y[keep], method = "kendall")$estimate[[1]])
            else NA_real_
          },
          p = {
            x <- as.numeric(QuarterDate); y <- .data[[yvar]]
            keep <- is.finite(x) & is.finite(y)
            if (sum(keep) >= 3 && dplyr::n_distinct(x[keep]) >= 2)
              suppressWarnings(cor.test(x[keep], y[keep], method = "kendall")$p.value)
            else NA_real_
          },
          .groups = "drop"
        ) %>%
        mutate(color = dplyr::case_when(
          !is.na(p) & p < 0.05 & tau > 0 ~ "blue",
          !is.na(p) & p < 0.05 & tau < 0 ~ "red",
          TRUE ~ "white"  # your choice for NS
        ))
    }
    
    trend50 <- trend_fun(dataL50, "L50") %>% rename(color50 = color)
    trend90 <- trend_fun(dataL50, "L90") %>% rename(color90 = color)
    
    d50 <- dataL50 %>% left_join(trend50 %>% select(Species, color50), by = "Species")
    d90 <- dataL50 %>% left_join(trend90 %>% select(Species, color90), by = "Species")
    
    bg50 <- d50 %>% distinct(Species, color50) %>% rename(bg_color = color50)
    bg90 <- d90 %>% distinct(Species, color90) %>% rename(bg_color = color90)
    
    species_levels <- sort(unique(c(as.character(d50$Species), as.character(d90$Species))))
    
    list(d50 = d50, d90 = d90, bg50 = bg50, bg90 = bg90, species_levels = species_levels)
  })
  
  # --- L50 plot ---
  output$L50_plot <- renderPlot({
    x <- l5090_prep(); d50 <- x$d50; bg50 <- x$bg50; species_levels <- x$species_levels
    
    ggplot(d50, aes(QuarterDate, L50)) +
      geom_rect(
        data = bg50,
        aes(fill = bg_color),
        xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf,
        inherit.aes = FALSE, alpha = 0.18
      ) +
      geom_line(aes(color = Species, group = Species), linewidth = 0.8, na.rm = TRUE, show.legend = FALSE) +
      facet_wrap(~ Species, scales = "free_y", ncol = 2) +
      scale_fill_identity(
        name = "Kendall trend",
        breaks = c("blue","red","white"),
        labels = c("Positive (p<0.05)", "Negative (p<0.05)", "Not significant"),
        guide = "legend"
      ) +
      guides(fill = guide_legend(override.aes = list(colour = "black", linewidth = 0.6))) +
      scale_color_hue(limits = species_levels, guide = "none") +
      scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
      labs(x = "Year", y = "Length (cm)", title = "L50 by Season") +
      theme_bw() +
      theme(legend.position = "bottom", strip.text = element_text(face = "bold"))
  }, execOnResize = TRUE)
  
  # --- L90 plot ---
  output$L90_plot <- renderPlot({
    x <- l5090_prep(); d90 <- x$d90; bg90 <- x$bg90; species_levels <- x$species_levels
    
    ggplot(d90, aes(QuarterDate, L90)) +
      geom_rect(
        data = bg90,
        aes(fill = bg_color),
        xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf,
        inherit.aes = FALSE, alpha = 0.18
      ) +
      geom_line(aes(color = Species, group = Species), linewidth = 0.8, na.rm = TRUE, show.legend = FALSE) +
      facet_wrap(~ Species, scales = "free_y", ncol = 2) +
      scale_fill_identity(
        name = "Kendall trend",
        breaks = c("blue","red","white"),
        labels = c("Positive (p<0.05)", "Negative (p<0.05)", "Not significant"),
        guide = "legend"
      ) +
      guides(fill = guide_legend(override.aes = list(colour = "black", linewidth = 0.6))) +
      scale_color_hue(limits = species_levels, guide = "none") +
      scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
      labs(x = "Year", y = "Length (cm)", title = "L90 by Season") +
      theme_bw() +
      theme(legend.position = "bottom", strip.text = element_text(face = "bold"))
  }, execOnResize = TRUE)
  
  # If these live in initially hidden tabs, keep rendering enabled:
  outputOptions(output, "L50_plot", suspendWhenHidden = FALSE)
  outputOptions(output, "L90_plot", suspendWhenHidden = FALSE)
  
  
  ###############################################
  # Sampling area Map
  output$samplingmap <- renderImage({
    filename <-  file.path("map.png")
    list(src = filename,
         width = "100%"
        # width = 1300,
        # height = 630,
        # alt = "z"
    )
  }, deleteFile = F)
  
  ############################################## 
  # Generate the Abundance-CATCH plot
  output$abundance_plot <- renderPlot({
    req(filtered_data_multi())
    data <- filtered_data_multi()
    # Summarize abundance
    if ("Count" %in% names(data)) {
      # If 'Count' column exists, sum the counts
      data <- data %>%
        group_by(Date, Species) %>%
        summarise(Abundance = sum(Count, na.rm = TRUE), .groups = "drop")
    } else {
      # If 'Count' column does not exist, count rows
      data <- data %>%
        group_by(Date, Species) %>%
        summarise(Abundance = n(), .groups = "drop")
    }
    
    ggplot(data, aes(x = Date, y = Abundance, fill = Species,  color = Species)) +
      geom_bar(stat = "identity", position = "identity", alpha = 0.5)+
    #  geom_smooth(aes(color = Species, group = Species),method = "gam",formula = y ~ s(x), se = FALSE, size = 0.8, linetype="twodash") +  # GAM trend lines using dynamik k
      theme_bw() +
      labs(title = "Catch Over Time",
           x = "Date", y = "Number of Fish",
           fill = "Species") +
      facet_wrap(~ Species, scales = "free_y", ncol = 2) +
      theme(legend.position = "",strip.text = element_text(face = "bold"),axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  ######################################################################    
  # Generate the L-W graph with power function and formula
  output$lw_plot <- renderPlot({
    req(filtered_data_single())
    data <- filtered_data_single()
    
    # Filter out rows with zero or missing Length or Weight to avoid log issues
    data <- data %>% filter(Length > 0, Weight > 0)
    
    # Log-transform the data for fitting
    data <- data %>%
      mutate(log_Length = log(Length),
             log_Weight = log(Weight))
    
    # Fit a linear model to the log-transformed data
    fit <- lm(log_Weight ~ log_Length, data = data)
    
    # Extract coefficients
    intercept <- coef(fit)[1]
    slope <- coef(fit)[2]
    formula_label <- paste0("W = exp(", round(intercept, 3), ") * L^", round(slope, 3))
    
    # Plot
    ggplot(data, aes(x = Length, y = Weight)) +
      geom_point(alpha = 0.3, size = 2, color = "black") +  # Scatter plot
      stat_function(
        fun = function(x) exp(intercept) * x^slope,  # Power function
        color = "red", linetype = "twodash", size = 1
      ) +
      theme_bw() +
      labs(
        title = "Length-Weight Relationship",
        subtitle = formula_label,
        x = "Length (cm)", y = "Weight (g)",
        color = "Species"
      ) +
      theme(legend.position = "bottom")
  })
  
  ##############################################################  
  # Generate Catch & Release timeseries plot
  output$cr_plot <- renderPlot({
    req(filtered_data_multi())
    data <- filtered_data_multi()
    
    # Replace NA with "Retained" for clarity
    data$CandR <- ifelse(is.na(data$CandR), "Retained", "C&R")
    
    ggplot(data, aes(x = Date, fill =CandR)) +
      geom_bar(position = "stack", alpha=0.7) +
      facet_wrap(~ Species, scales = "free_y", ncol = 2) +
      theme_bw() +
      labs(
        title = "Catch & Release vs Retained Catch",
        x = "Date",
        y = "Number of Fish",
        fill = "Catch Status"
      ) +
      theme(legend.position = "bottom", strip.text = element_text(face = "bold"),axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  
  ####################################################  
  # Top 10 Longest Fish Table
  output$longest_fish_table <- renderTable({
    req(filtered_data_multi())
    data <- filtered_data_multi()
    top_longest <- data %>%
      arrange(desc(Length)) %>%
      head(10) %>%
      mutate(Medal = case_when(
        row_number() == 1 ~ "🥇",
        row_number() == 2 ~ "🥈",
        row_number() == 3 ~ "🥉",
        TRUE ~ ""
      ))
    top_longest %>%
      select(Medal, Fisherman, Length, Species, FishingGear)
  }, striped = TRUE, hover = TRUE)
  
  # Top 10 Heaviest Fish Table
  output$heaviest_fish_table <- renderTable({
    req(filtered_data_multi())
    data <- filtered_data_multi()
    top_heaviest <- data %>%
      arrange(desc(Weight)) %>%
      head(10) %>%
      mutate(Medal = case_when(
        row_number() == 1 ~ "🥇",
        row_number() == 2 ~ "🥈",
        row_number() == 3 ~ "🥉",
        TRUE ~ ""
      ))
    top_heaviest %>%
      select(Medal, Fisherman, Weight, Species, FishingGear)
  }, striped = TRUE, hover = TRUE)
  #######################################
  # Biodiversity Prize
  output$biodiversity_prize <- renderText({
    req(filtered_data_multi())
    data <- filtered_data_multi()
    
    if (nrow(data) == 0) {
      return("No data available to determine the Biodiversity Prize winners.")
    }
    
    biodiversity <- data %>%
      group_by(Fisherman) %>%
      summarise(Species_Count = n_distinct(Species)) %>%
      arrange(desc(Species_Count))
    
    max_species <- max(biodiversity$Species_Count)
    winners <- biodiversity %>%
      filter(Species_Count == max_species)
    
    winner_names <- paste(winners$Fisherman, collapse = ", ")
    
    paste0(
      winner_names,
      " explored the ocean’s wonders and caught ",
      max_species,
      " species! You’re the ambassadors of biodiversity!"
    )
  })
  ################################################# 
  # Conservation Champion Prize
  output$conservation_prize <- renderText({
    req(filtered_data_multi())  # Use filtered data with C&R
    data <- filtered_data_multi()
    
    # Count releases by fisherman
    conservation_data <- data %>%
      filter(CandR == "Y") %>%
      group_by(Fisherman) %>%
      summarise(Released_Count = n()) %>%
      arrange(desc(Released_Count))
    
    # Identify the winner
    if (nrow(conservation_data) > 0) {
      winner <- conservation_data[1, ]
      paste0("Hats off to ", winner$Fisherman, 
             "! Leading the way with ", winner$Released_Count, " releases — keeping the future of fishing alive, one fish at a time!")
    } else {
      "No Catch & Release data available to determine the winner."
    }
  })
  
  #####################################################################  
  # Create the Environmental Variables Plot 
  
  output$env_plot <- renderPlot({
    req(filtered_env_data())
    data <- filtered_env_data()
    
    # Temperature Plot
    temp_plot <- ggplot(data, aes(x = Date, y = Avg_Temp)) +
      geom_line(color = "red", size = 0.8, linetype= "solid") +
      theme_bw() +
      labs(
        title = "Average Temperature Over Time",
        x = "Date",
        y = "Temperature (°C)"
      ) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    # Salinity Plot
    salinity_plot <- ggplot(data, aes(x = Date, y = Avg_Salinity)) +
      geom_line(color = "green", size = 0.8, linetype= "solid") +
      theme_bw() +
      labs(
        title = "Average Salinity Over Time",
        x = "Date",
        y = "Salinity (psu)"
      ) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    # Tidal Wave Height Plot
    tidal_plot <- ggplot(data, aes(x = Date, y = Avg_Height)) +
      geom_line(color = "blue", size = 0.8, linetype= "solid") +
      theme_bw() +
      labs(
        title = "Average Tidal Wave Height Over Time",
        x = "Date",
        y = "Tidal Wave Height (m)"
      ) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    # Combine the plots using patchwork
    temp_plot / salinity_plot / tidal_plot
  })
  
  ##########################################################
  # CPUE Season plot 
  output$cpue_plot<- renderPlot({
    req(cpue_monthly())
    data <- cpue_monthly()
    
    
    ggplot(data, aes(x = Year_season, y = Total_CPUE, fill = Species,  color = Species)) +
      geom_bar(stat = "identity", position = "identity", alpha = 0.5)+
      geom_smooth(aes(color = Species, group = Species),method = "gam",formula = y ~ s(x), se = FALSE, size = 0.8, linetype="twodash") +  # GAM trend lines using dynamik k
      theme_bw() + 
      labs(title = "Mean CPUE by Season (Catch/Fisherman/hr)",
           x = "Date", y = "CPUE",
           fill = "Species") +
      facet_wrap(~ Species, scales = "free_y", ncol = 2) +
      theme(legend.position = "",strip.text = element_text(face = "bold"),axis.text.x = element_text(angle = 90, hjust = 1))
    
  })
  
  ###########################
  # Render AMSY plots
  #output$amsyPlot1 <- renderPlot({
  #  req(aAMSY())  # traj
  #  amsy_plot
  #})
  output$amsyPlot2 <- renderPlot({
    req(aAMSY())  # kobeplot
  },execOnResize = TRUE)
  
  outputOptions(output, "amsyPlot2", suspendWhenHidden = FALSE)
  ################################
  
  
  # Render GAM plots CPUE vs ENV
  output$gam_plot <- renderPlot({
    req(cpue_station(), input$species_single)
    
    # Filter data for the selected species
    data <- cpue_station() %>%
      filter(Species == input$species_single)
    
    data <- data %>% dplyr::filter(CPUE > 0) # if you want to use only positive data!
    
    # Check if there's enough data for GAM fitting
    if (nrow(data) < 10) {
      return()  # Do not render the plot if insufficient data
    }
    
    # Fit a GAM model
    gam_model <- gam(CPUE ~ s(Avg_Temp) + s(Avg_Salinity) + s(Avg_Height),
                     data = data, family = gaussian)
    # Check deviance explained
    deviance_explained <- summary(gam_model)$dev.expl * 100
    if (deviance_explained < 1) {
      return()  # Do not render the plot if deviance explained is < X%
    }
    
    # Generate plots for each predictor
    temp_plot <- ggplot(data, aes(x = Avg_Temp, y = CPUE)) +
      geom_point(alpha = 0.35) +
      geom_smooth(method = "gam", formula = y ~ s(x), se = TRUE, color = "red",linetype="twodash") +
      theme_bw() +
      labs(
        title = paste("Effect of Temperature on", input$species_single),
        x = "Average Temperature (°C)",
        y = "CPUE"
      )
    
    salinity_plot <- ggplot(data, aes(x = Avg_Salinity, y = CPUE)) +
      geom_point(alpha = 0.35) +
      geom_smooth(method = "gam", formula = y ~ s(x), se = TRUE, color = "green", linetype="twodash") +
      theme_bw() +
      labs(
        title = paste("Effect of Salinity on", input$species_single),
        x = "Average Salinity (psu)",
        y = "CPUE"
      )
    
    height_plot <- ggplot(data, aes(x = Avg_Height, y = CPUE)) +
      geom_point(alpha = 0.35) +
      geom_smooth(method = "gam", formula = y ~ s(x), se = TRUE, color = "blue",linetype="twodash") +
      theme_bw() +
      labs(
        title = paste("Effect of Tidal Height on", input$species_single),
        x = "Average Tidal Height (m)",
        y = "CPUE"
      )
    
    # Combine the plots into a single horizontal layout
    temp_plot / salinity_plot / height_plot
  })
  
  output$gam_details <- renderText({
    req(cpue_station(), input$species_single)
    
    # Filter data for the selected species
    data <- cpue_station() %>%
      filter(Species == input$species_single)
    
    data <- data %>% dplyr::filter(CPUE > 0) # if you want to use only positive data!
    
    # Check if there's enough data for GAM fitting
    if (nrow(data) < 10) {
      return()
    }
    
    # Fit a GAM model
    gam_model <- gam(CPUE ~ s(Avg_Temp) + s(Avg_Salinity) + s(Avg_Height),
                     data = data, family = gaussian)
    
    # Check deviance explained
    deviance_explained <- summary(gam_model)$dev.expl * 100
    if (deviance_explained < 1) {
      return()
    }
    
    # Calculate variable contributions if deviance explained > 1%
    contributions <- gam.hp(gam_model)
    contributions <- contributions$hierarchical.partitioning[,3]*100
    
    # Clean up variable names (remove "s()" from names)
    clean_contributions <- setNames(
      contributions,
      gsub("^s\\((.*)\\)$", "\\1", names(contributions))
    )
    
    # Format contributions for display
    contribution_text <- paste0(
      "• Total Deviance explained: ", round(deviance_explained, 2), "%;\n",
      "• Variable Contributions:\n",
      paste(
        names(clean_contributions),
        ": ",
        round(clean_contributions, 2),
        "%",
        collapse = ",\n"
      )
    )
    
    # Render the contributions
    contribution_text
  })
  
  output$gam_message <- renderText({
    req(cpue_station(), input$species_single)
    
    # Filter data for the selected species
    data <- cpue_station() %>%
      filter(Species == input$species_single)
    
    data <- data %>% dplyr::filter(CPUE > 0) # if you want to use only positive data!
    
    # Check if there's enough data for GAM fitting
    if (nrow(data) < 10) {
      return("Not enough data points to fit a GAM model.")
    }
    
    # Fit a GAM model
    gam_model <- gam(CPUE ~ s(Avg_Temp) + s(Avg_Salinity) + s(Avg_Height),
                     data = data, family = gaussian)
    
    # Check deviance explained
    deviance_explained <- summary(gam_model)$dev.expl * 100
    if (deviance_explained < 30) {
      return("The model didn’t perform well enough (Total Deviance explained less than 30%), so the results might not be so reliable")
    }
    
    # No message if the model is valid
    return(NULL)
  })
  
}

# Run the App
shinyApp(ui = ui, server = server)



