library(shiny)
library(ggplot2)
library(dplyr)
library(readr)
library(zoo)

# Load CSV
tourism_data <- read.csv("C:/Users/AsperDes/Desktop/DedsecP_R_Project/world_tourism_economy_data.csv")

# Exclude non-countries
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

# Filter valid countries
tourism_data <- tourism_data %>%
  filter(!is.na(country)) %>%
  filter(!country %in% non_countries) %>%
  filter(!is.na(country_code))

# Get numeric columns
numeric_cols <- names(tourism_data)[sapply(tourism_data, is.numeric)]
numeric_cols <- setdiff(numeric_cols, "year")

# Interpolate missing numeric values per country and column
tourism_data <- tourism_data %>%
  group_by(country) %>%
  arrange(year) %>%
  mutate(across(all_of(numeric_cols), ~ na.approx(., year, na.rm = FALSE))) %>%
  ungroup()

# Labels
category_labels <- c(
  "tourism_receipts" = "Tourism Receipts (in Millions US Dollars)",
  "tourism_arrivals" = "Total number of international tourist arrivals (in Millions)",
  "tourism_exports" = "Share of exports derived from tourism (in percent %)",
  "tourism_departures" = "Total number of residents travelling abroad (in Millions)",
  "tourism_expenditures" = "Expenditure by international tourists (in Thousands US Dollars)",
  "gdp" = "Gross Domestic Product of the country (in Billions US Dollars)",
  "inflation" = "Inflation Rate (in percent %)",
  "unemployment" = "Unemployment Rate (in percent %)"
)

# Conversion factors
convert_to_millions <- c("tourism_receipts", "tourism_arrivals", "tourism_departures")
convert_to_thousands <- c("tourism_expenditures")
convert_to_billions <- c("gdp")

available_categories <- intersect(names(category_labels), numeric_cols)
year_list <- sort(unique(tourism_data$year))

# UI
ui <- fluidPage(
  titlePanel("Top 10 Countries by Selected Tourism Category"),
  p("This dashboard visualizes the top 10 countries based on selected tourism-related categories such as arrivals, receipts, and GDP contribution."),
  p("\n"),
  p("Warning!! There are several missing values in the data-set. We have filled them using Interpolation."),
  p("Note: Data for most tourism indicators is available only up to the year 2020. Metrics like GDP, Inflation, and Unemployment may extend beyond that."),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("category", "Select category for comparison:", 
                  choices = setNames(available_categories, category_labels[available_categories]),
                  selected = available_categories[1]),
      selectInput("year", "Select year for which you want the data:", choices = year_list, selected = max(year_list))
    ),
    
    mainPanel(
      plotOutput("barPlot", height = "500px"),
      br(),
      h4("Country Code Legend"),
      tableOutput("codeLegend"),
      br(),
      h4("Conclusion"),
      textOutput("conclusionText")
    )
  )
)

# Server
server <- function(input, output) {
  
  custom_colors <- c(
    "#f11414", "#fe4949", "#f67037", "#ff9d35", "#ffac53",
    "#26c075", "#00cd69", "#008946", "#1c7eff", "#213b55"
  )
  
  filtered_data <- reactive({
    req(input$category, input$year)
    
    tourism_data %>%
      filter(year == input$year, !is.na(.data[[input$category]])) %>%
      arrange(desc(.data[[input$category]])) %>%
      slice_head(n = 10)
  })
  
  output$barPlot <- renderPlot({
    data <- filtered_data()
    category <- input$category
    
    if (nrow(data) == 0) {
      plot.new()
      title("No data available for selected category and year.")
      return()
    }
    
    plot_data <- data
    label_suffix <- ""
    
    if (category %in% convert_to_millions) {
      plot_data[[category]] <- plot_data[[category]] / 1e6
      label_suffix <- "M"
    } else if (category %in% convert_to_thousands) {
      plot_data[[category]] <- plot_data[[category]] / 1e3
      label_suffix <- "K"
    } else if (category %in% convert_to_billions) {
      plot_data[[category]] <- plot_data[[category]] / 1e9
      label_suffix <- "B"
    }
    
    ggplot(plot_data, aes(x = reorder(country_code, .data[[category]]), y = .data[[category]], fill = country_code)) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = setNames(custom_colors, plot_data$country_code)) +
      geom_text(aes(label = paste0(round(.data[[category]], 2), label_suffix)), 
                hjust = -0.1, size = 4) +
      labs(
        title = paste("Top 10 Countries by", category_labels[[category]], "in", input$year),
        x = "Country Code",
        y = category_labels[[category]]
      ) +
      coord_flip() +
      theme_minimal(base_size = 14) +
      theme(
        legend.position = "none",
        axis.text.y = element_text(size = 12),
        plot.title = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 14)
      )
  })
  
  output$conclusionText <- renderText({
    data <- filtered_data()
    category <- input$category
    
    if (nrow(data) == 0) return("No data available to generate a conclusion.")
    
    top_country <- data$country[1]
    top_value <- data[[category]][1]
    formatted_value <- ""
    
    if (category %in% convert_to_millions) {
      formatted_value <- paste0(format(round(top_value / 1e6, 2), big.mark = ","), " Million")
    } else if (category %in% convert_to_thousands) {
      formatted_value <- paste0(format(round(top_value / 1e3, 2), big.mark = ","), " Thousand")
    } else if (category %in% convert_to_billions) {
      formatted_value <- paste0(format(round(top_value / 1e9, 2), big.mark = ","), " Billion")
    } else {
      formatted_value <- format(round(top_value, 2), big.mark = ",")
    }
    
    paste("In", input$year, ",", top_country, 
          "had the highest", category_labels[[category]], 
          "with a value of", formatted_value, ".")
  })
  
  output$codeLegend <- renderTable({
    data <- filtered_data()
    if (nrow(data) == 0) return(NULL)
    data %>% select(Code = country_code, Country = country)
  })
}

# Run app
shinyApp(ui = ui, server = server)
