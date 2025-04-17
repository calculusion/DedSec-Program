library(shiny)
library(ggplot2)
library(dplyr)
library(readr)
library(zoo)

# Load data
tourism_data <- read.csv("C:/Users/giria/downloads/world_tourism_economy_data.csv")

# List of non-country regions to exclude
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

# Filter and prepare data
tourism_data <- tourism_data %>%
  filter(!is.na(country)) %>%
  filter(!country %in% non_countries) %>%
  mutate(
    tourism_receipts = tourism_receipts / 1e6,       # to millions
    tourism_arrivals = tourism_arrivals / 1e6,       # to millions
    tourism_departures = tourism_departures / 1e6,   # to millions
    gdp = gdp / 1e9,                                 # to billions
    tourism_expenditures = tourism_expenditures / 1e3  # to thousands
  )

# Country list for dropdown
country_list <- unique(tourism_data$country)

# Define user-friendly labels (Label = Value)
category_labels <- c(
  "Tourism Receipts (in Millions US Dollars)" = "tourism_receipts",
  "Total number of international tourist arrivals (in Millions)" = "tourism_arrivals",
  "Share of exports derived from tourism (in percent %)" = "tourism_exports",
  "Total number of residents travelling abroad (in Millions)" = "tourism_departures",
  "Expenditure by international tourists (in Thousands US Dollars)" = "tourism_expenditures",
  "Gross Domestic Product of the country (in Billions US Dollars)" = "gdp",
  "Inflation Rate (in percent %)" = "inflation",
  "Unemployment Rate (in percent %)" = "unemployment"
)

# Numeric columns for interpolation
numeric_cols <- c("tourism_receipts", "tourism_arrivals", "tourism_departures", "gdp", "tourism_expenditures", "inflation", "unemployment")

# Interpolate missing numeric values per country and column
tourism_data <- tourism_data %>%
  group_by(country) %>%
  arrange(year) %>%
  mutate(across(all_of(numeric_cols), ~ na.approx(., year, na.rm = FALSE))) %>%
  ungroup()

# Helper to get label from column name
get_label_from_column <- function(column_name) {
  label <- names(category_labels)[category_labels == column_name]
  if(length(label) == 0) return(column_name)  # Fallback in case the mapping doesn't work
  return(label)
}

# UI
ui <- fluidPage(
  titlePanel("Economic data analysis & comparison for three countries based on tourism"),
  p("Compare statistics for three countries of your choice, based on a single parameter."),
  p("\n"),
  p("Warning!! There are several missing values in the data-set. We have filled them using Interpolation."),
  p("Note: Data for most tourism indicators is available only up to the year 2020. Metrics like GDP, Inflation, and Unemployment may extend beyond that."),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("country1", "Select First Country:",
                  choices = country_list, selected = country_list[1]),
      selectInput("country2", "Select Second Country:",
                  choices = country_list, selected = country_list[2]),
      selectInput("country3", "Select Third Country:",
                  choices = country_list, selected = country_list[3]),
      selectInput("category", "Select Category to Compare:",
                  choices = category_labels, selected = category_labels[1])
    ),
    
    mainPanel(
      plotOutput("comparisonPlot"),
      br(),
      h4("Conclusion of the Statistics"),
      verbatimTextOutput("conclusionText")
    )
  )
)

# Server
server <- function(input, output) {
  
  # Reactive filtered data
  filtered_data <- reactive({
    req(input$country1, input$country2, input$country3, input$category)
    
    tourism_data %>%
      filter(country %in% c(input$country1, input$country2, input$country3)) %>%
      select(country, year, value = all_of(input$category)) %>%
      filter(!is.na(value))
  })
  
  # Plot
  output$comparisonPlot <- renderPlot({
    tryCatch({
      data <- filtered_data()
      
      if (nrow(data) == 0) {
        plot.new()
        title("No data available for the selected countries and category.")
        return()
      }
      
      # Define a fixed color palette
      palette <- c("#ff4949", "#26c075", "#1c7eff", "#ff9d35", "#9467bd", "#8c564b")
      
      # Ensure we don't exceed the number of colors available in the palette
      if (length(unique(data$country)) > length(palette)) {
        warning("Not enough colors defined for the number of countries. Some countries may share colors.")
      }
      
      ggplot(data, aes(x = year, y = value, color = factor(country), group = country)) +
        geom_line(linewidth = 1) +
        geom_point(size = 3) +
        scale_x_continuous(breaks = pretty(data$year)) +
        scale_color_manual(values = palette[1:length(unique(data$country))]) +  # Apply fixed color palette
        labs(
          title = paste("Comparison of", get_label_from_column(input$category),
                        "among", input$country1, ",", input$country2, "and", input$country3),
          x = "Year",
          y = get_label_from_column(input$category),
          color = "Country"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 16, face = "bold"),
          axis.title.x = element_text(size = 14, face = "bold"),
          axis.title.y = element_text(size = 14, face = "bold"),
          legend.title = element_text(size = 13),
          legend.text = element_text(size = 12),
          axis.text = element_text(size = 12)
        )
    }, error = function(e) {
      plot.new()
      title("Plotting failed: Check if data and column names are valid.")
    })
  })
  
  
  # Detailed Conclusion
  output$conclusionText <- renderText({
    tryCatch({
      data <- filtered_data()
      
      if (nrow(data) < 3) return("Not enough data for comparison.")
      
      avg_vals <- data %>%
        group_by(country) %>%
        summarise(Average = mean(value, na.rm = TRUE), .groups = 'drop') %>%
        arrange(desc(Average))
      
      if (nrow(avg_vals) < 3) return("Not enough data to draw a conclusion.")
      
      label <- get_label_from_column(input$category)
      
      summary_text <- paste0(
        "Here is the average ", label, " for each selected country:\n\n",
        paste0("• ", avg_vals$country[1], ": ", round(avg_vals$Average[1], 2), "\n"),
        paste0("• ", avg_vals$country[2], ": ", round(avg_vals$Average[2], 2), "\n"),
        paste0("• ", avg_vals$country[3], ": ", round(avg_vals$Average[3], 2), "\n\n")
      )
      
      highest <- avg_vals$country[1]
      lowest <- avg_vals$country[3]
      diff_high_low <- round(avg_vals$Average[1] - avg_vals$Average[3], 2)
      
      summary_text <- paste0(
        summary_text,
        highest, " has the highest average ", label, 
        ", while ", lowest, " has the lowest.\n",
        "The difference between them is ", diff_high_low, " units.\n"
      )
      
      summary_text
    }, error = function(e) {
      "Error generating conclusion. Please check if category contains numeric values."
    })
  })
}

# Run the app
shinyApp(ui = ui, server = server)
