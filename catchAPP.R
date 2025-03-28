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

# UI ########################################################################
ui <- fluidPage(
  titlePanel("Fish Catch Data Visualization  (FAKEDATA!)"),
  helpText(
    tags$span("ACCESS-Fjord Project"
    )
  ),
  helpText(
    tags$span("Need help? ",
              tags$a(
                href = "https://www.su.se/english/profiles/frma6502-1.665005",
                HTML("Francesco Masnadi (DEEP, Stockholm University)"), 
                style = "color: #007BFF; text-decoration: none;"
              )
    )
  ),
  sidebarLayout(
    mainPanel( selectInput("page_select", "Select View:",
                           choices = c("Leader Board",
                                       "Sampling Area Map",
                                       "Catch Plot",
                                       "Catch & Release vs Retained",
                                       "Length Frequency Distribution (LFD)",
                                       "Length-Weight (L-W) Graph",
                                       "CPUE trend",
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
  
  # Reactive to load fish dataset
  fish_data <- reactive({
    if (!file.exists(file_path)) {
      stop("The dataset file was not found at the specified path.")
    }
    ext <- tools::file_ext(file_path)
    if (ext == "csv") {
      read.csv(file_path)
    } else if (ext == "xlsx") {
      readxl::read_excel(file_path)
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
  
 
  # dropdown menu page
  output$dynamic_panel <- renderUI({
    switch(input$page_select,
           
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
           
           "Sampling Area Map" = fluidPage(
             fluidRow(column(12, p("The Sørfolda fjord system. Sampling stations highlighted as red dots, fish farms in purple"))),
             fluidRow(column(12, plotOutput("samplingmap", width = "100%")))
           ),
           
           "Catch Plot" = fluidPage(
             fluidRow(column(12, p("Brief non-expert description..."))),
             fluidRow(column(12, imageOutput("abundance_plot", width = "100%")))
           ),
           
           "Catch & Release vs Retained" = fluidPage(
             fluidRow(column(12, p("Brief non-expert description..."))),
             fluidRow(column(12, plotOutput("cr_plot", width = "100%")))
           ),
           
           "Length Frequency Distribution (LFD)" = fluidPage(
             fluidRow(column(12, p("Brief non-expert description..."))),
             fluidRow(column(12, textOutput("longest_fish_info"))),
             fluidRow(column(12, plotOutput("lfd_plot", width = "100%")))
           ),
           
           "Length-Weight (L-W) Graph" = fluidPage(
             fluidRow(column(12, p("Brief non-expert description..."))),
             fluidRow(column(12, uiOutput("species_filter_single"))),
             fluidRow(column(12, plotOutput("lw_plot", width = "100%")))
           ),
           
           "CPUE trend" = fluidPage(
             fluidRow(column(12, p("Brief non-expert description..."))),
             fluidRow(column(12, plotOutput("cpue_daily_plot", width = "100%")))
           ),
           
           "Environmental Variables" = fluidPage(
             fluidRow(column(12, p("Brief non-expert description..."))),
             fluidRow(column(12, plotOutput("env_plot", width = "100%")))
           ),
           
           "Environmental Influence Analysis" = fluidPage(
             fluidRow(column(12, p("Brief non-expert description..."))),
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
    data <- fish_data() %>% inner_join(env_data(), by = c("Date", "Station"))
    
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
        Avg_Temp = coalesce(Avg_Temp, Avg_Temp_station),  # Use station-level temperature
        Avg_Salinity = coalesce(Avg_Salinity, Avg_Salinity_station),
        Avg_Height = coalesce(Avg_Height, Avg_Height_station),
        CPUE = ifelse(Num_Fishermen > 0, Total_Abundance / Num_Fishermen, 0)  # Calculate CPUE
      ) %>%    select(-ends_with("_station"))   # Remove redundant station-level column
    # Return the completed dataset
    filled_data
    
  })
  
  cpue_daily <- reactive({
    req(fish_data(), env_data())  # Ensure fish_data and env_data are available)
    data <- fish_data() %>% inner_join(env_data(), by = c("Date", "Station"))
    
    # Apply date filter
    if (!is.null(input$date_filter)) {
      data <- data %>% filter(Date >= input$date_filter[1] & Date <= input$date_filter[2])
    }
    # Apply station filter
    if (!is.null(input$station)) {
      data <- data %>% filter(Station %in% input$station)
    }
    
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
        Num_Fishermen = n_distinct(Fisherman)  # Number of unique fishermen
      ) %>%
      ungroup()
    
    # Summarize fishermen and environmental data at the station level
    station_summary <- data %>%
      group_by(Date, Station) %>%
      summarise(
        Num_Fishermen = n_distinct(Fisherman)  # Unique fishermen
      ) %>%
      ungroup()
    
    # Combine all combinations with the summarized catch data
    filled_data <- all_combinations %>%
      left_join(summarized_data, by = c("Date", "Station", "Species")) %>%
      left_join(station_summary, by = c("Date", "Station"), suffix = c("", "_station")) %>%
      mutate(
        Total_Abundance = coalesce(Total_Abundance, 0),  # Replace missing catches with 0
        Num_Fishermen = coalesce(Num_Fishermen_station, 0),      # Use station-level Num_Fishermen
        CPUE = ifelse(Num_Fishermen > 0, Total_Abundance / Num_Fishermen, 0)  # Calculate CPUE
      ) %>%
      select(-ends_with("_station"))  # Remove redundant station-level column
    # Return the completed dataset
    
    filled_data_daily <- filled_data %>%   dplyr::group_by(Date, Species) %>% dplyr::summarise( Total_CPUE = mean(CPUE, na.rm = TRUE)) %>%
      ungroup() # Sum CPUE across stations
    filled_data_daily
    
  })
  
  
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
      theme_minimal() +
      labs(title = "Length Frequency Distribution",
           x = "Length (cm)", y = "Frequency",
           fill = "Species")  +
      scale_x_continuous(breaks = seq(floor(min(data$Length, na.rm = T)), ceiling(max(data$Length, na.rm = T)), by = 2))+   # 1 cm tick marks
      theme(legend.position = "bottom",axis.text.x = element_text(angle = 90, hjust = 1))
  })
  ###############################################  
  
  
  
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
      geom_smooth(aes(color = Species, group = Species),method = "gam",formula = y ~ s(x), se = FALSE, size = 0.8, linetype="twodash") +  # GAM trend lines using dynamik k
      theme_minimal() +
      labs(title = "Catch Over Time (+ trends with GAMs)",
           x = "Date", y = "Number of Fish",
           fill = "Species") +
      facet_wrap(~ Species, scales = "free_y") +
      theme(legend.position = "bottom",axis.text.x = element_text(angle = 45, hjust = 1))
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
      theme_minimal() +
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
      facet_wrap(~ Species, scales = "free_y") +
      theme_minimal() +
      labs(
        title = "Catch & Release vs Retained Catch",
        x = "Date",
        y = "Number of Fish",
        fill = "Catch Status"
      ) +
      theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1))
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
      theme_minimal() +
      labs(
        title = "Average Temperature Over Time",
        x = "Date",
        y = "Temperature (°C)"
      ) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    # Salinity Plot
    salinity_plot <- ggplot(data, aes(x = Date, y = Avg_Salinity)) +
      geom_line(color = "green", size = 0.8, linetype= "solid") +
      theme_minimal() +
      labs(
        title = "Average Salinity Over Time",
        x = "Date",
        y = "Salinity (psu)"
      ) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    # Tidal Wave Height Plot
    tidal_plot <- ggplot(data, aes(x = Date, y = Avg_Height)) +
      geom_line(color = "blue", size = 0.8, linetype= "solid") +
      theme_minimal() +
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
  # CPUE Daily plot 
  output$cpue_daily_plot<- renderPlot({
    req(cpue_daily())
    data <- cpue_daily()
    
    
    ggplot(data, aes(x = Date, y = Total_CPUE, fill = Species,  color = Species)) +
      geom_bar(stat = "identity", position = "identity", alpha = 0.5)+
      geom_smooth(aes(color = Species, group = Species),method = "gam",formula = y ~ s(x), se = FALSE, size = 0.8, linetype="twodash") +  # GAM trend lines using dynamik k
      theme_minimal() + 
      labs(title = "CPUE (Fish/Fisherman/Month) Over Time",
           x = "Date", y = "CPUE",
           fill = "Species") +
      facet_wrap(~ Species, scales = "free_y") +
      theme(legend.position = "bottom",axis.text.x = element_text(angle = 45, hjust = 1))
    
  })
  
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
      theme_minimal() +
      labs(
        title = paste("Effect of Temperature on", input$species_single),
        x = "Average Temperature (°C)",
        y = "CPUE"
      )
    
    salinity_plot <- ggplot(data, aes(x = Avg_Salinity, y = CPUE)) +
      geom_point(alpha = 0.35) +
      geom_smooth(method = "gam", formula = y ~ s(x), se = TRUE, color = "green", linetype="twodash") +
      theme_minimal() +
      labs(
        title = paste("Effect of Salinity on", input$species_single),
        x = "Average Salinity (psu)",
        y = "CPUE"
      )
    
    height_plot <- ggplot(data, aes(x = Avg_Height, y = CPUE)) +
      geom_point(alpha = 0.35) +
      geom_smooth(method = "gam", formula = y ~ s(x), se = TRUE, color = "blue",linetype="twodash") +
      theme_minimal() +
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
      "Total Deviance explained: ", round(deviance_explained, 2), "%;\n",
      "Variable Contributions:\n",
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



