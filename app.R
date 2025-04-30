rm(list = ls())

library(shiny)
library(dplyr)
library(readr)
library(tidyr)
library(DT)

# Load Data
ap_ref <- read_csv("data/pp_exposure_2021_reference.csv")
ap_ss <- read_csv("data/pp_exposure_2021_ss.csv")
ap_green <- read_csv("data/pp_exposure_2021_green.csv")
ap_both <- read_csv("data/pp_exposure_2021_both.csv")
hh_ref <- read_csv("data/hh_2021.csv")
zone <- read_csv("data/zoneSystem.csv")

# Define a processing function
process_data <- function(df, scenario_name) {
  df %>%
    select(id, hhid, age, gender, occupation,
           mmetHr_walk, mmetHr_cycle, mmetHr_otherSport, income,
           exposure_normalised_pm25, exposure_normalised_no2,
           exposure_noise_HA, exposure_noise_HSD,
           exposure_normalised_ndvi, exposure_normalised_noise_Lden) %>%
    mutate(Scenario = scenario_name) %>%
    left_join(hh_ref %>% select(hhid = id, zone), by = "hhid") %>%
    left_join(zone %>%
                select(location = ladnm, imd = imd10, oaID, lsoa21cd, lsoa21nm, msoa21cd, msoa21nm),
              by = c("zone" = "oaID"))
}

# Apply function
ap_ref <- process_data(ap_ref, "Baseline")
ap_ss <- process_data(ap_ss, "Safe Streets")
ap_green <- process_data(ap_green, "Greening")
ap_both <- process_data(ap_both, "Both")

# Combine all scenarios
exposure <- bind_rows(ap_ref, ap_ss, ap_green, ap_both)

# Cleanup
rm(ap_ref, ap_ss, ap_green, ap_both, hh_ref, zone)

# Further transformations
exposure <- exposure %>%
  filter(age >= 18) %>%
  mutate(
    gender = factor(gender, levels = c(1, 2), labels = c("Male", "Female")),
    age_group = factor(case_when(
      age <= 25 ~ "18-25",
      age <= 65 ~ "26-65",
      TRUE ~ "65+"
    ), levels = c("18-25", "26-65", "65+")),
    age_groups = factor(ifelse(age < 65, "<65", "65+"), levels = c("<65", "65+")),
    imd = factor(imd, levels = 1:10, labels = c("Most Deprived", 2:9, "Least Deprived")),
    income_group = cut(income,
                       breaks = quantile(income, probs = seq(0, 1, 0.2), na.rm = TRUE),
                       labels = c("Lowest", "Low", "Middle", "High", "Highest"),
                       include.lowest = TRUE),
    mmetHr_total = round(mmetHr_cycle + mmetHr_otherSport + mmetHr_walk, 2)
  )

write_csv(exposure, "data/exposure.csv")

################################# Run from here ################################

library(shiny)
library(dplyr)
library(readr)
library(tidyr)
library(DT)

exposure <- read_csv("manchester/health/processed/exposure.csv")

ui <- fluidPage(
  titlePanel("Weekly Individual Environmental Exposure Summary Tables"),
  sidebarLayout(
    sidebarPanel(
      selectInput("gender", "Gender:", choices = c("All", levels(exposure$gender)), selected = "All"),
      selectInput("age_group", "Age Group:", choices = c("All", levels(exposure$age_group)), selected = "All"),
      selectInput("imd", "IMD:", choices = c("All", levels(exposure$imd)), selected = "All"),
      selectInput("location", "Location:", choices = c("All", sort(unique(na.omit(exposure$location)))), selected = "All"),
      sliderInput("mmetHr_total", "mMET-hours/week:",
                  min = floor(min(exposure$mmetHr_total, na.rm = TRUE)),
                  max = ceiling(max(exposure$mmetHr_total, na.rm = TRUE)),
                  value = c(floor(min(exposure$mmetHr_total, na.rm = TRUE)),
                            ceiling(max(exposure$mmetHr_total, na.rm = TRUE))))
    ),
    mainPanel(
      h3("PM2.5 Summary"), DTOutput("summary_table_pm25"),
      h3("NO2 Summary"), DTOutput("summary_table_no2"),
      h3("Total mMET-hours/week"), DTOutput("summary_table_mmetHr"),
      h3("Noise Exposure - HA"), DTOutput("summary_table_noise_HA"),
      h3("Noise Exposure - HSD"), DTOutput("summary_table_noise_HSD"),
      h3("NDVI"), DTOutput("summary_table_ndvi"),
      h3("Noise Exposure - Lden"), DTOutput("summary_table_noise_Lden"),
      
      h3("PM2.5 by Gender"), DTOutput("summary_pm25_by_gender"),
      h3("PM2.5 by Age Group"), DTOutput("summary_pm25_by_age"),
      h3("PM2.5 by Location"), DTOutput("summary_pm25_by_location"),
      h3("PM2.5 by IMD"), DTOutput("summary_pm25_by_imd"),
      
      h3("NO2 by Gender"), DTOutput("summary_no2_by_gender"),
      h3("NO2 by Age Group"), DTOutput("summary_no2_by_age"),
      h3("NO2 by Location"), DTOutput("summary_no2_by_location"),
      h3("NO2 by IMD"), DTOutput("summary_no2_by_imd"),
      
      h3("mMET-hours by Gender"), DTOutput("summary_mmetHr_by_gender"),
      h3("mMET-hours by Age Group"), DTOutput("summary_mmetHr_by_age"),
      h3("mMET-hours by Location"), DTOutput("summary_mmetHr_by_location"),
      h3("mMET-hours by IMD"), DTOutput("summary_mmetHr_by_imd"),
      
      h3("Noise Exposure - HA by Gender"), DTOutput("summary_noise_HA_by_gender"),
      h3("Noise Exposure - HA by Age Group"), DTOutput("summary_noise_HA_by_age"),
      h3("Noise Exposure - HA by Location"), DTOutput("summary_noise_HA_by_location"),
      h3("Noise Exposure - HA by IMD"), DTOutput("summary_noise_HA_by_imd"),
      
      h3("Noise Exposure - HSD by Gender"), DTOutput("summary_noise_HSD_by_gender"),
      h3("Noise Exposure - HSD by Age Group"), DTOutput("summary_noise_HSD_by_age"),
      h3("Noise Exposure - HSD by Location"), DTOutput("summary_noise_HSD_by_location"),
      h3("Noise Exposure - HSD by IMD"), DTOutput("summary_noise_HSD_by_imd"),
      
      h3("NDVI by Gender"), DTOutput("summary_ndvi_by_gender"),
      h3("NDVI by Age Group"), DTOutput("summary_ndvi_by_age"),
      h3("NDVI by Location"), DTOutput("summary_ndvi_by_location"),
      h3("NDVI by IMD"), DTOutput("summary_ndvi_by_imd"),
      
      h3("Noise Exposure - Lden by Gender"), DTOutput("summary_noise_Lden_by_gender"),
      h3("Noise Exposure - Lden by Age Group"), DTOutput("summary_noise_Lden_by_age"),
      h3("Noise Exposure - Lden by Location"), DTOutput("summary_noise_Lden_by_location"),
      h3("Noise Exposure - Lden by IMD"), DTOutput("summary_noise_Lden_by_imd")
    )
  )
)

server <- function(input, output) {
  
  filtered_data <- reactive({
    data <- exposure
    if (input$gender != "All") data <- data %>% filter(gender == input$gender)
    if (input$age_group != "All") data <- data %>% filter(age_group == input$age_group)
    if (input$imd != "All") data <- data %>% filter(imd == input$imd)
    if (input$location != "All") data <- data %>% filter(location == input$location)
    data <- data %>% filter(between(mmetHr_total, input$mmetHr_total[1], input$mmetHr_total[2]))
    data
  })
  
  summarise_metric <- function(data, variable) {
    data %>%
      group_by(Scenario) %>%
      summarise(
        Mean = round(mean(.data[[variable]], na.rm = TRUE), 2),
        `5th` = round(quantile(.data[[variable]], 0.05, na.rm = TRUE), 2),
        `20th` = round(quantile(.data[[variable]], 0.20, na.rm = TRUE), 2),
        `25th` = round(quantile(.data[[variable]], 0.25, na.rm = TRUE), 2),
        `35th` = round(quantile(.data[[variable]], 0.35, na.rm = TRUE), 2),
        `50th` = round(quantile(.data[[variable]], 0.50, na.rm = TRUE), 2),
        `95th` = round(quantile(.data[[variable]], 0.95, na.rm = TRUE), 2),
        .groups = "drop"
      )
  }
  
  summarise_by_variable <- function(data, group_var, value_var) {
    data %>%
      group_by(.data[[group_var]], Scenario) %>%
      summarise(Mean = round(mean(.data[[value_var]], na.rm = TRUE), 2), .groups = "drop") %>%
      pivot_wider(names_from = Scenario, values_from = Mean) %>%
      arrange(.data[[group_var]])
  }
  
  output$summary_table_pm25 <- renderDT({
    datatable(summarise_metric(filtered_data(), "exposure_normalised_pm25"))
  })
  output$summary_table_no2 <- renderDT({
    datatable(summarise_metric(filtered_data(), "exposure_normalised_no2"))
  })
  output$summary_table_mmetHr <- renderDT({
    datatable(summarise_metric(filtered_data(), "mmetHr_total"))
  })
  output$summary_table_noise_HA <- renderDT({
    datatable(summarise_metric(filtered_data(), "exposure_noise_HA"))
  })
  output$summary_table_noise_HSD <- renderDT({
    datatable(summarise_metric(filtered_data(), "exposure_noise_HSD"))
  })
  output$summary_table_ndvi <- renderDT({
    datatable(summarise_metric(filtered_data(), "exposure_normalised_ndvi"))
  })
  output$summary_table_noise_Lden <- renderDT({
    datatable(summarise_metric(filtered_data(), "exposure_normalised_noise_Lden"))
  })
  
  output$summary_pm25_by_gender <- renderDT({
    datatable(summarise_by_variable(filtered_data(), "gender", "exposure_normalised_pm25"))
  })
  output$summary_pm25_by_age <- renderDT({
    datatable(summarise_by_variable(filtered_data(), "age_group", "exposure_normalised_pm25"))
  })
  output$summary_pm25_by_location <- renderDT({
    datatable(summarise_by_variable(filtered_data(), "location", "exposure_normalised_pm25"))
  })
  output$summary_pm25_by_imd <- renderDT({
    datatable(summarise_by_variable(filtered_data(), "imd", "exposure_normalised_pm25"))
  })
  
  output$summary_no2_by_gender <- renderDT({
    datatable(summarise_by_variable(filtered_data(), "gender", "exposure_normalised_no2"))
  })
  output$summary_no2_by_age <- renderDT({
    datatable(summarise_by_variable(filtered_data(), "age_group", "exposure_normalised_no2"))
  })
  output$summary_no2_by_location <- renderDT({
    datatable(summarise_by_variable(filtered_data(), "location", "exposure_normalised_no2"))
  })
  output$summary_no2_by_imd <- renderDT({
    datatable(summarise_by_variable(filtered_data(), "imd", "exposure_normalised_no2"))
  })
  
  output$summary_mmetHr_by_gender <- renderDT({
    datatable(summarise_by_variable(filtered_data(), "gender", "mmetHr_total"))
  })
  output$summary_mmetHr_by_age <- renderDT({
    datatable(summarise_by_variable(filtered_data(), "age_group", "mmetHr_total"))
  })
  output$summary_mmetHr_by_location <- renderDT({
    datatable(summarise_by_variable(filtered_data(), "location", "mmetHr_total"))
  })
  output$summary_mmetHr_by_imd <- renderDT({
    datatable(summarise_by_variable(filtered_data(), "imd", "mmetHr_total"))
  })
  
  output$summary_noise_HA_by_gender <- renderDT({
    datatable(summarise_by_variable(filtered_data(), "gender", "exposure_noise_HA"))
  })
  output$summary_noise_HA_by_age <- renderDT({
    datatable(summarise_by_variable(filtered_data(), "age_group", "exposure_noise_HA"))
  })
  output$summary_noise_HA_by_location <- renderDT({
    datatable(summarise_by_variable(filtered_data(), "location", "exposure_noise_HA"))
  })
  output$summary_noise_HA_by_imd <- renderDT({
    datatable(summarise_by_variable(filtered_data(), "imd", "exposure_noise_HA"))
  })
  
  output$summary_noise_HSD_by_gender <- renderDT({
    datatable(summarise_by_variable(filtered_data(), "gender", "exposure_noise_HSD"))
  })
  output$summary_noise_HSD_by_age <- renderDT({
    datatable(summarise_by_variable(filtered_data(), "age_group", "exposure_noise_HSD"))
  })
  output$summary_noise_HSD_by_location <- renderDT({
    datatable(summarise_by_variable(filtered_data(), "location", "exposure_noise_HSD"))
  })
  output$summary_noise_HSD_by_imd <- renderDT({
    datatable(summarise_by_variable(filtered_data(), "imd", "exposure_noise_HSD"))
  })
  
  output$summary_ndvi_by_gender <- renderDT({
    datatable(summarise_by_variable(filtered_data(), "gender", "exposure_normalised_ndvi"))
  })
  output$summary_ndvi_by_age <- renderDT({
    datatable(summarise_by_variable(filtered_data(), "age_group", "exposure_normalised_ndvi"))
  })
  output$summary_ndvi_by_location <- renderDT({
    datatable(summarise_by_variable(filtered_data(), "location", "exposure_normalised_ndvi"))
  })
  output$summary_ndvi_by_imd <- renderDT({
    datatable(summarise_by_variable(filtered_data(), "imd", "exposure_normalised_ndvi"))
  })
  
  output$summary_noise_Lden_by_gender <- renderDT({
    datatable(summarise_by_variable(filtered_data(), "gender", "exposure_normalised_noise_Lden"))
  })
  output$summary_noise_Lden_by_age <- renderDT({
    datatable(summarise_by_variable(filtered_data(), "age_group", "exposure_normalised_noise_Lden"))
  })
  output$summary_noise_Lden_by_location <- renderDT({
    datatable(summarise_by_variable(filtered_data(), "location", "exposure_normalised_noise_Lden"))
  })
  output$summary_noise_Lden_by_imd <- renderDT({
    datatable(summarise_by_variable(filtered_data(), "imd", "exposure_normalised_noise_Lden"))
  })
}

shinyApp(ui = ui, server = server)
