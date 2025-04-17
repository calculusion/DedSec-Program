library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(zoo)

# Load data
tourism_data <- read.csv("D:/R/R programs/R project/world_tourism_economy_data.csv")

# Filter out non-countries
non_countries <- c(
  "Africa Eastern and Southern", "Africa Western and Central", "Arab World", "Central Europe and the Baltics", "Early-demographic dividend",
  "East Asia & Pacific", "East Asia & Pacific (excluding high income)", "East Asia & Pacific (IDA & IBRD countries)", "Euro area",
  "Europe & Central Asia", "Europe & Central Asia (excluding high income)", "Europe & Central Asia (IDA & IBRD countries)", "European Union",
  "Fragile and conflict affected situations", "Heavily indebted poor counties (HIPC)", "High income", "IBRD only", "IDA & IBRD total",
  "IDA blend", "IDA only", "IDA total", "Late-demographic dividend", "Latin America & Caribbean", "Latin America & Caribbean (excluding high income)",
  "Latin America & the Caribbean (IDA & IBRD countries)", "Least developed countries : UN classication", "Low & middle income", "Low income",
  "Lower middle income", "Middle East & North Africa", "Middle East & North Africa (excluding high income)", "Middle East & North Africa (IDA & IBRD countries)",
  "Middle income", "Not classified", "North America", "OECD members", "Other small states", "Pacific island small states", "Post-demographic dividend", 
  "Pre-demographic dividend", "Small states", "South Asia", "South Asia (IDA & IBRD)", "Sub-Saharan Africa", "Sub-Saharan Africa (excluding high income)",
  "Sub-Saharan Africa (IDA & IBRD countries)", "Upper middle income", "West Bank and Gaza", "World"
)

# Filter and convert units
tourism_data <- tourism_data %>%
  filter(!is.na(country)) %>%
  filter(!country %in% non_countries)

# Define numeric columns before interpolation
numeric_cols <- names(tourism_data)[sapply(tourism_data, is.numeric)]
numeric_cols <- setdiff(numeric_cols, "year")  # Keep year untouched

# Interpolate missing numeric values
tourism_data <- tourism_data %>%
  group_by(country) %>%
  arrange(year) %>%
  mutate(across(all_of(numeric_cols), ~ na.approx(., year, na.rm = FALSE))) %>%
  ungroup()

# Apply unit conversions after interpolation
tourism_data <- tourism_data %>%
  mutate(
    tourism_receipts = tourism_receipts / 1e6,
    tourism_arrivals = tourism_arrivals / 1e6,
    tourism_departures = tourism_departures / 1e6,
    gdp = gdp / 1e9,
    tourism_expenditures = tourism_expenditures / 1e3
  )

# Define labels
category_labels <- c(
  "Tourism Receipts (in Millions US Dollars)" = "tourism_receipts",
  "Total number of international tourist arrivals (in Millions)" = "tourism_arrivals",
  "Share of exports derived from tourism (in percent %)" = "tourism_exports",
  "Total number of residents travelling abroad (in Millions)" = "tourism_departures",
  "Expenditure by international tourists (in Thousands US Dollars)" = "tourism_expenditures",
  "Gross Domestic Product (in Billions US Dollars)" = "gdp",
  "Inflation Rate (in percent %)" = "inflation",
  "Unemployment Rate (in percent %)" = "unemployment"
)

# Function to map variable name to label
get_label <- function(var) names(category_labels)[category_labels == var]

# Define custom colors
custom_colors <- c(
  tourism_receipts = "#1c7eff",
  tourism_arrivals = "#48A6A7",
  tourism_exports = "#26c075",
  tourism_departures = "#ff4949",
  tourism_expenditures = "#8c52ff",
  gdp = "#ff9d35",
  inflation = "#ff66c4",
  unemployment = "#0e2a47"
)

# UI
ui <- fluidPage(
  titlePanel("Economic data analysis for a single country based on tourism"),
  p("Compare a country’s factors based on a range of years"),
  p("\n"),
  p("Warning!! There are several missing values in the data-set. We have filled them using Interpolation."),
  p("Note: Data for most tourism indicators is available only up to the year 2020. Metrics like GDP, Inflation, and Unemployment may extend beyond that."),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("country", "Select country of your choice:",
                  choices = unique(tourism_data$country),
                  selected = unique(tourism_data$country)[1]),
      
      sliderInput("year_range", "Select year Range for data analysis:",
                  min = min(tourism_data$year, na.rm = TRUE),
                  max = max(tourism_data$year, na.rm = TRUE),
                  value = c(min(tourism_data$year, na.rm = TRUE), max(tourism_data$year, na.rm = TRUE)),
                  step = 1,
                  sep = ""),
      
      checkboxGroupInput("categories", "Select Categories to View:",
                         choices = category_labels,
                         selected = category_labels[1:2])
    ),
    
    mainPanel(
      plotOutput("trendPlot"),
      br(),
      h4("Summary Insight"),
      verbatimTextOutput("summaryText")
    )
  )
)

# Server
server <- function(input, output) {
  
  filtered_data <- reactive({
    req(input$country, input$year_range, input$categories)
    
    tourism_data %>%
      filter(country == input$country,
             year >= input$year_range[1],
             year <= input$year_range[2]) %>%
      select(year, all_of(input$categories))
  })
  
  output$trendPlot <- renderPlot({
    data <- filtered_data()
    if (nrow(data) == 0) {
      plot.new()
      title("No data for selected range and country.")
      return()
    }
    
    data_long <- data %>%
      pivot_longer(-year, names_to = "category", values_to = "value")
    
    ggplot(data_long, aes(x = year, y = value, color = category)) +
      geom_line(linewidth = 1.2) +
      geom_point(size = 2.5) +
      scale_color_manual(
        values = custom_colors[input$categories],
        labels = sapply(input$categories, get_label)
      ) +
      labs(
        title = paste("Trends in Selected Categories for", input$country),
        x = "Year",
        y = "Value",
        color = "Category"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 14),
        legend.title = element_text(size = 13),
        legend.text = element_text(size = 12)
      )
  })
  
  output$summaryText <- renderText({
    data <- filtered_data()
    if (nrow(data) == 0) return("No data to summarize.")
    
    summaries <- data %>%
      summarise(across(all_of(input$categories), ~round(mean(.x, na.rm = TRUE), 2))) %>%
      pivot_longer(everything(), names_to = "Category", values_to = "Average")
    
    summaries$Category <- sapply(summaries$Category, get_label)
    
    summary_text <- paste0("Average values from ", input$year_range[1], " to ", input$year_range[2], ":\n\n")
    summary_text <- paste0(summary_text, 
                           paste0("• ", summaries$Category, ": ", summaries$Average, collapse = "\n"))
    summary_text
  })
}

# Run the app
shinyApp(ui = ui, server = server)
