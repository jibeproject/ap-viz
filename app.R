
rm(list = ls())

library(shiny)
library(dplyr)
library(readr)
library(DT)

# Data Processing

ap_ref <- read_csv("data/reference/pp_exposure_2021_170225.csv")

#Note: using old data for intervention
ap_ss <- read_csv("data/saferstreets/pp_exposure_2025_int_071124.csv")

hh_ref <- read_csv("data/reference/hh_2021.csv")

dd_int <- read_csv("data/saferstreets/dd_2025_int_071124.csv")

zone <- read_csv("data/zoneSystem.csv")

ap_ref <- ap_ref %>% 
  select(id,
         hhid,
         age,
         gender,
         occupation,
         mmetHr_walk,
         mmetHr_cycle,
         mmetHr_otherSport,
         income,
         exposure_normalised_pm25,
         exposure_normalised_no2) %>%
  mutate(Scenario = "Baseline") %>% 
  left_join(hh_ref %>% 
              select(hhid = id, zone),
            by = c("hhid" = "hhid")) %>% 
  left_join(zone %>% 
              select(location = ladnm,imd = imd10, oaID, lsoa21cd, lsoa21nm, msoa21cd, msoa21nm),
            by = c("zone" = "oaID"))

ap_ss <- ap_ss %>% 
  select(id,
         hhid,
         age,
         gender,
         occupation,
         mmetHr_walk,
         mmetHr_cycle,
         mmetHr_otherSport,
         income,
         exposure_normalised_pm25,
         exposure_normalised_no2) %>%
  mutate(Scenario = "Safer Streets") %>% 
  left_join(dd_int %>% 
              select(hhid = hhID, zone),
            by = c("hhid" = "hhid")) %>% 
  left_join(zone %>% 
              select(location = ladnm,imd = imd10, oaID, lsoa21cd, lsoa21nm, msoa21cd, msoa21nm),
            by = c("zone" = "oaID"))

exposure <- bind_rows(ap_ref, ap_ss)

rm(ap_ref, ap_ss, dd_int, hh_ref, zone)

################### Adding Zone and Demographics ########################

exposure <- exposure %>% 
  filter(age >= 18) %>% 
  mutate(gender = factor(gender,
                         levels = c(1,2),
                         labels = c("Male","Female")),
         age_group = factor(case_when(
           age >= 18 & age <= 25 ~ "18-25",
           age >= 26 & age <= 65 ~ "26-65",
           age >= 66 ~ "65+",
           TRUE ~ "Other"), 
           levels = c("18-25", "26-65", "65+", "Other")),
         age_groups = factor(case_when(
           age < 65 ~ "<65",
           age >= 65 ~ "65+",
           TRUE ~ "Other"), 
           levels = c("<65", "65+", "Other")),
         imd = factor(imd,
                      levels = c(1,2,3,4,5,6,7,8,9,10),
                      labels = c("Most Deprived", 2,3,4,5,6,7,8,9,"Least Deprived")),
         income_group = cut(income, 
                            breaks = quantile(income, probs = seq(0, 1, 0.2), na.rm = TRUE), 
                            labels = c("Lowest", "Low", "Middle", "High", "Highest"),
                            include.lowest = TRUE),
         mmetHr_total = round((mmetHr_cycle + mmetHr_otherSport + mmetHr_walk),2))

# Define UI
ui <- fluidPage(
  titlePanel("Weekly Individual Air Pollution Exposure Summary Tables"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("gender", "Gender:", choices = c("All", levels(exposure$gender)), selected = "All"),
      selectInput("age_group", "Age Group:", choices = c("All", levels(exposure$age_group)), selected = "All"),
      selectInput("imd", "IMD:", choices = c("All", levels(exposure$imd)), selected = "All"),
      selectInput("location", "Location:", choices = c("All", unique(exposure$location)), selected = "All"),
      sliderInput("mmetHr_total", "mMET-hours/week:", min = min(exposure$mmetHr_total, na.rm = TRUE), 
                  max = max(exposure$mmetHr_total, na.rm = TRUE), value = range(exposure$mmetHr_total, na.rm = TRUE))
    ),
    
    mainPanel(
      h3("PM2.5"),
      DTOutput("summary_table_pm25"),
      
      h3("NO2"),
      DTOutput("summary_table_no2"),
      
      h3("Total mMET-hours/week"),
      DTOutput("summary_table_mmetHr")
    )
  )
)

# Define Server
server <- function(input, output) {
  
  # Reactive dataset based on filters
  filtered_data <- reactive({
    data <- exposure 
    
    if (input$gender != "All") {
      data <- data %>% filter(gender == input$gender)
    }
    
    if (input$age_group != "All") {
      data <- data %>% filter(age_group == input$age_group)
    }
    
    if (input$imd != "All") {
      data <- data %>% filter(imd == input$imd)
    }
    
    if (input$location != "All") {
      data <- data %>% filter(location == input$location)
    }
    
    data <- data %>% filter(mmetHr_total >= input$mmetHr_total[1] & mmetHr_total <= input$mmetHr_total[2])
    
    return(data)
  })
  
  # Compute PM2.5 summary table
  summary_table_pm25 <- reactive({
    filtered_data() %>%
      group_by(Scenario) %>%
      summarise(
        Mean = round(mean(exposure_normalised_pm25, na.rm = TRUE), 2),
        "5th" = round(quantile(exposure_normalised_pm25, 0.05, na.rm = TRUE), 2),
        "20th" = round(quantile(exposure_normalised_pm25, 0.20, na.rm = TRUE), 2),
        "25th" = round(quantile(exposure_normalised_pm25, 0.25, na.rm = TRUE), 2),
        "35th" = round(quantile(exposure_normalised_pm25, 0.35, na.rm = TRUE), 2),
        "50th" = round(quantile(exposure_normalised_pm25, 0.50, na.rm = TRUE), 2),
        "95th" = round(quantile(exposure_normalised_pm25, 0.95, na.rm = TRUE), 2)
      )
  })
  
  # Compute NO2 summary table
  summary_table_no2 <- reactive({
    filtered_data() %>%
      group_by(Scenario) %>%
      summarise(
        Mean = round(mean(exposure_normalised_no2, na.rm = TRUE), 2),
        "5th" = round(quantile(exposure_normalised_no2, 0.05, na.rm = TRUE), 2),
        "20th" = round(quantile(exposure_normalised_no2, 0.20, na.rm = TRUE), 2),
        "25th" = round(quantile(exposure_normalised_no2, 0.25, na.rm = TRUE), 2),
        "35th" = round(quantile(exposure_normalised_no2, 0.35, na.rm = TRUE), 2),
        "50th" = round(quantile(exposure_normalised_no2, 0.50, na.rm = TRUE), 2),
        "95th" = round(quantile(exposure_normalised_no2, 0.95, na.rm = TRUE), 2)
      )
  })
  
  # Compute MMET Hr Total summary table
  summary_table_mmetHr <- reactive({
    filtered_data() %>%
      group_by(Scenario) %>%
      summarise(
        Mean = round(mean(mmetHr_total, na.rm = TRUE), 2),
        "5th" = round(quantile(mmetHr_total, 0.05, na.rm = TRUE), 2),
        "20th" = round(quantile(mmetHr_total, 0.20, na.rm = TRUE), 2),
        "25th" = round(quantile(mmetHr_total, 0.25, na.rm = TRUE), 2),
        "35th" = round(quantile(mmetHr_total, 0.35, na.rm = TRUE), 2),
        "50th" = round(quantile(mmetHr_total, 0.50, na.rm = TRUE), 2),
        "95th" = round(quantile(mmetHr_total, 0.95, na.rm = TRUE), 2)
      )
  })
  
  # Show different values of the PM2.5 table
  output$summary_table_pm25 <- renderDT({
    datatable(summary_table_pm25(), options = list(autoWidth = TRUE))
  })
  
  # Show different values of the NO2 table
  output$summary_table_no2 <- renderDT({
    datatable(summary_table_no2(), options = list(autoWidth = TRUE))
  })
  
  # Show different values of the NO2 table
  output$summary_table_mmetHr <- renderDT({
    datatable(summary_table_mmetHr(), options = list(autoWidth = TRUE))
  })
}

# Run the App
shinyApp(ui = ui, server = server)
