# Load libraries
library(shiny)
library(dplyr)
library(ggplot2)

# Load and prepare data
tourism_data <- read.csv("C:/Users/giria/downloads/world_tourism_economy_data.csv")

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

tourism_data <- tourism_data %>%
  filter(!is.na(country)) %>%
  filter(!country %in% non_countries) %>%
  filter(!is.na(country_code)) %>%
  filter(!year %in% c(2021, 2022, 2023))

# UI
ui <- fluidPage(
  titlePanel("Tourism Arrival Regression Model"),
  p("Regression with all predictors."),
  
  sidebarLayout(
    sidebarPanel(
      h4("Base and Refined Model Analysis"),
      helpText("Using Graphs and Correlation Coefficients")
    ),
    
    mainPanel(
      h3("Normality Check: Tourism Arrivals"),
      plotOutput("distribution_plot"),
      verbatimTextOutput("shapiro_test"),
      
      h3("Correlation and Scatter Plots"),
      uiOutput("correlation_plots_ui"),
      
      h3("Base Model: tourism_arrivals ~ 1"),
      verbatimTextOutput("base_model_summary"),
      
      h3("Refined Model"),
      verbatimTextOutput("refined_model_summary")
    )
  )
)

# Server
server <- function(input, output) {
  
  # Histogram for normality check
  output$distribution_plot <- renderPlot({
    filtered_data <- tourism_data %>%
      filter(tourism_arrivals < quantile(tourism_arrivals, 0.95, na.rm = TRUE))  # Remove top 5% outliers
    
    # Plot histogram
    hist(filtered_data$tourism_arrivals,
         breaks = 50,                           # More bars
         col = "skyblue",
         main = "Distribution of Tourism Arrivals (Excl. Top 5%)",
         xlab = "Number of Tourism Arrivals",
         xlim = c(min(filtered_data$tourism_arrivals), as.numeric(1e+07)),
         probability = TRUE)  # Set y-axis to probability density
    
    # Add normality curve using curve() function
    curve(dnorm(x, mean = mean(filtered_data$tourism_arrivals), 
                sd = sd(filtered_data$tourism_arrivals)),
          col = "red", lwd = 2, add = TRUE)
  })
  
  
  # Shapiro-Wilk test for normality
  output$shapiro_test <- renderPrint({
    test_result <- shapiro.test(sample(tourism_data$tourism_arrivals, size = min(5000, nrow(tourism_data))))
    cat("Shapiro-Wilk Test for Normality:\n")
    cat("Test Statistic:", round(test_result$statistic, 4), "\n")
    cat("P-value:", round(test_result$p.value, 4), "\n")
    
    if (test_result$p.value < 0.05) {
      cat("This suggests that the data is not normally distributed. A normal distribution means that the data would form a bell-shaped curve.")
    } else {
      cat("This suggests that the data is likely normally distributed, meaning it follows a bell-shaped curve.")
    }
  })
  
  # Correlation and scatter plots vs other variables
  output$correlation_plots_ui <- renderUI({
    vars <- c("tourism_receipts", "tourism_exports", "tourism_departures",
              "tourism_expenditures", "gdp", "inflation", "unemployment")
    plot_output_list <- lapply(vars, function(var) {
      list(
        plotOutput(outputId = paste0("scatter_", var)),
        verbatimTextOutput(outputId = paste0("cor_", var))
      )
    })
    do.call(tagList, plot_output_list)
  })
  
  observe({
    vars <- c("tourism_receipts", "tourism_exports", "tourism_departures",
              "tourism_expenditures", "gdp", "inflation", "unemployment")
    
    for (var in vars) {
      local({
        v <- var
        output[[paste0("scatter_", v)]] <- renderPlot({
          ggplot(tourism_data, aes_string(x = v, y = "tourism_arrivals")) +
            geom_point(alpha = 0.5) +
            geom_smooth(method = "lm", se = FALSE, color = "red") +
            labs(
              title = paste("Tourism Arrivals vs", v),
              x = paste("Values of", gsub("_", " ", v)), 
              y = "Number of Tourism Arrivals"
            ) +
            theme_minimal()
        })
        
        output[[paste0("cor_", v)]] <- renderPrint({
          cor_val <- cor(tourism_data[[v]], tourism_data$tourism_arrivals, use = "complete.obs")
          cat("Correlation Coefficient:", round(cor_val, 4), "\n")
          
          if (cor_val > 0.7) {
            cat("This shows a strong positive relationship: as one variable increases, the other tends to increase as well.")
          } else if (cor_val > 0.4) {
            cat("This shows a moderate positive relationship: both variables increase, but not as strongly.")
          } else if (cor_val > 0) {
            cat("This shows a weak positive relationship: one variable slightly increases with the other.")
          } else if (cor_val < -0.7) {
            cat("This shows a strong negative relationship: as one variable increases, the other tends to decrease.")
          } else if (cor_val < -0.4) {
            cat("This shows a moderate negative relationship: both variables move in opposite directions, but not very strongly.")
          } else if (cor_val < 0) {
            cat("This shows a weak negative relationship: as one variable increases, the other slightly decreases.")
          } else {
            cat("There is no significant relationship between these two variables.")
          }
        })
      })
    }
  })
  

  
  # Base model summary
  output$base_model_summary <- renderPrint({
    base_model <- lm(tourism_arrivals ~ tourism_receipts+tourism_exports+tourism_departures+tourism_expenditures+gdp+inflation+unemployment,data=tourism_data)
    cat("This is a very simple model that only looks at the average tourism arrivals without considering any other factors.\n")
    summary(base_model)
  })
  
  # Refined model summary
  output$refined_model_summary <- renderPrint({
    refined_model <- lm(tourism_arrivals ~ tourism_receipts+tourism_departures+tourism_expenditures+gdp+unemployment,data=tourism_data)
    cat("This is a more focused model using fewer factors, like tourism receipts, expenditures, and GDP, to predict tourism arrivals.\n")
    summary(refined_model)
  })
}

# Run the app
shinyApp(ui = ui, server = server)
