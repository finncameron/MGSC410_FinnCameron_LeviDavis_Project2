# Load necessary libraries
library(shiny)
library(ggplot2)
library(DT)
library(xgboost)
library(shinyjs)
library(plotly)

# Assuming the model is preloaded here; replace 'model' with your trained XGBoost model object
model <- xgb.load("/Users/levidavis/Desktop/xgb_model.json")

# Define UI
ui <- fluidPage(
  titlePanel("Real Estate Valuation Tool"),
  
  sidebarLayout(
    sidebarPanel(
      h3("User Inputs"),
      sliderInput("bedrooms", "Number of Bedrooms:", min = 1, max = 10, value = 3),
      sliderInput("bathrooms", "Number of Bathrooms:", min = 1, max = 5, value = 2),
      checkboxInput("include_nearby_price", "Include Nearby Homes Average Price", TRUE),
      checkboxInput("include_keyword", "Include Keyword Presence Feature", TRUE),
      checkboxInput("include_distance_university", "Include Distance to Nearest University", TRUE),
      actionButton("predict", "Get Prediction", class = "btn-primary"),
      br(), br(),
      h3("Predicted Price"),
      textOutput("prediction")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Exploratory Data Analysis (EDA)",
                 fluidRow(
                   column(6, plotOutput("price_vs_nearby_price")),
                   column(6, plotOutput("price_distribution"))
                 ),
                 fluidRow(
                   column(6, plotOutput("feature_importance")),
                   column(6, plotOutput("pred_vs_actual"))
                 )
        ),
        
        tabPanel("Model Performance",
                 h4("Model Performance Comparison"),
                 plotlyOutput("model_comparison"),
                 h5("With Selected Features:"),
                 tableOutput("model_performance_table")
        ),
        
        tabPanel("Predicted vs Actual",
                 h4("Predicted vs Actual Prices"),
                 plotlyOutput("predicted_vs_actual_plot")
        ),
        
        tabPanel("Appendix/Disclaimer",
                 h4("Appendix and Disclaimer"),
                 tags$div(
                   tags$h5("Data Source"),
                   tags$p("The dataset used for this model was compiled from various real estate listings, property sales records, and tax assessments."),
                   tags$h5("Methodology"),
                   tags$p("The Automated Valuation Model (AVM) was built using XGBoost and trained on historical property data. Key features were selected based on their importance to property price predictions."),
                   tags$h5("Assumptions and Limitations"),
                   tags$p("This model assumes that historical data trends continue to apply to current market conditions. External factors like economic shifts, interest rates, and policy changes may affect property values.")
                 )
        )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Use shinyjs to hide Model Performance tab initially
  useShinyjs()
  hideTab(inputId = "navbar", target = "Model Performance")
  
  # Reactive dataset with user inputs
  reactive_data <- reactive({
    data <- data.frame(
      bedrooms = input$bedrooms,
      bathrooms = input$bathrooms,
      nearbyHomes_avg_price = if (input$include_nearby_price) { 200000 } else { NA },
      keyword_present = if (input$include_keyword) { 1 } else { 0 },
      distance_to_nearest_university_miles = if (input$include_distance_university) { 5 } else { NA }
    )
    data
  })
  
  # Render Prediction Output
  output$prediction <- renderText({
    req(input$predict)  # Ensures this only runs on button click
    isolate({
      # Convert data to matrix format for prediction
      new_data <- as.matrix(reactive_data())
      
      # Generate prediction
      pred <- predict(model, new_data)
      
      # Show Model Performance Tab after prediction
      showTab(inputId = "navbar", target = "Model Performance")
      paste("Predicted Price: $", format(round(pred, 2), big.mark = ","))
    })
  })
  
  # Model Performance with Features Toggle
  output$model_comparison <- renderPlotly({
    # Generate model performance metrics with and without features
    metrics <- data.frame(
      Model = c("XGBoost With Features", "XGBoost Without Features"),
      RMSE = c(71370.43, 85000),  # Replace with actual RMSE values
      R2 = c(0.970, 0.920)        # Replace with actual R² values
    )
    
    plot_ly(metrics, x = ~Model, y = ~RMSE, type = 'bar', text = ~paste("RMSE: ", RMSE),
            hoverinfo = 'text', color = ~Model, colors = c('blue', 'green')) %>%
      layout(title = "Model Performance Comparison: RMSE",
             xaxis = list(title = "Model"),
             yaxis = list(title = "RMSE ($)"))
  })
  
  # Display Model Performance Metrics Table
  output$model_performance_table <- renderTable({
    data.frame(
      Feature_Set = c("With Features", "Without Features"),
      RMSE = c(71370.43, 85000),  # Replace with actual RMSE values
      R2 = c(0.970, 0.920)        # Replace with actual R² values
    )
  })
  
  # Property Price vs. Nearby Homes Average Price
  output$price_vs_nearby_price <- renderPlot({
    set.seed(42)
    avg_nearby_price <- rnorm(500, mean = 500000, sd = 200000)
    property_price <- avg_nearby_price * 1.2 + rnorm(500, mean = 0, sd = 100000)
    
    ggplot(data.frame(avg_nearby_price, property_price), aes(x = avg_nearby_price, y = property_price)) +
      geom_point(color = "blue", alpha = 0.6) +
      labs(title = "Property Price vs. Nearby Homes Average Price", x = "Average Price of Nearby Homes", y = "Property Price") +
      theme_minimal()
  })
  
  # Distribution of Property Prices
  output$price_distribution <- renderPlot({
    set.seed(42)
    property_prices <- c(rnorm(1000, mean = 855000, sd = 300000))
    
    ggplot(data.frame(property_prices), aes(x = property_prices)) +
      geom_histogram(fill = "skyblue", color = "black", bins = 30) +
      geom_vline(xintercept = 855000, color = "red", linetype = "dashed") +
      labs(title = "Distribution of Property Prices", x = "Price", y = "Frequency") +
      theme_minimal()
  })
  
  # Top 10 Feature Importance Chart
  output$feature_importance <- renderPlot({
    features <- c("livingArea", "address/zipcode", "resoFacts/pricePerSquareFoot", "lotAreaValue",
                  "distance_to_nearest_university_miles", "yearBuilt", "lastSoldPrice",
                  "address/city", "resoFacts/interiorFeatures/0", "resoFacts/roofType")
    importance <- c(600, 550, 520, 490, 460, 450, 400, 390, 380, 370)
    
    feature_data <- data.frame(Feature = features, Importance = importance)
    
    ggplot(feature_data, aes(x = reorder(Feature, Importance), y = Importance)) +
      geom_bar(stat = "identity", fill = "lightblue") +
      coord_flip() +
      labs(title = "Top 10 Feature Importance", x = "Feature", y = "Importance") +
      theme_minimal()
  })
  
  # Predicted vs Actual Prices with Interactive Plot
  output$predicted_vs_actual_plot <- renderPlotly({
    set.seed(42)
    actual_price <- rnorm(500, mean = 855000, sd = 300000)  # Adjusted range
    predicted_price <- actual_price + rnorm(500, mean = 0, sd = 75000)
    
    plot_ly(x = actual_price, y = predicted_price, type = 'scatter', mode = 'markers',
            marker = list(color = 'rgba(100, 150, 200, 0.7)', size = 8),
            text = ~paste("Predicted Price: $", format(predicted_price, big.mark = ",")),
            hoverinfo = 'text') %>%
      layout(title = "Predicted vs Actual Prices",
             xaxis = list(title = "Actual Price ($)", range = c(250000, 2500000)),
             yaxis = list(title = "Predicted Price ($)", range = c(250000, 2500000)),
             shapes = list(
               list(type = "line", x0 = 250000, x1 = 2500000, y0 = 250000, y1 = 2500000,
                    line = list(dash = 'dash', color = 'red', width = 2))
             ))
  })
}

# Run the application
shinyApp(ui = ui, server = server)

