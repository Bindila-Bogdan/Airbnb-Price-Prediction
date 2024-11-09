# install additional packages for maps and theme
# install.packages("leaflet", dependencies = T)
# install.packages("leaflet.extras")
# install.packages("bslib")

library(shiny)
library(tidyverse)
library(leaflet)
library(leaflet.extras)
library(bslib)
library(randomForest)

# load final model and data
model <- readRDS("./www/rf_model.rds")
cv_results <- readRDS("./www/rf_cv_results.rds")

data_raw <- read_delim(
  file = "./www/amsterdam_raw.csv",
  delim = ",",
  col_names = TRUE,
  show_col_types = FALSE
)

data <- read_delim(
  file = "./www/amsterdam_preprocessed.csv",
  delim = ",",
  col_names = TRUE,
  show_col_types = FALSE
)

plot_feature_importance <- function(rf_model) {
  # build the feature importance plot given by the model on the entire dataset
  feature_importance <- as.data.frame(importance(rf_model)) %>%
    rename("importance" = "IncNodePurity")
  feature_importance$importance  <- feature_importance$importance / sum(feature_importance$importance) * 100
  feature_importance <- feature_importance %>% arrange(desc(importance))
  
  ggplot(feature_importance,
         aes(
           x  = rownames(feature_importance),
           y = importance,
           fill = importance
         )) +
    geom_bar(stat = "identity") +
    scale_fill_gradient(low = "#767676", high = "#FF5A5F") +
    labs(title = "Scaled Importance of Selected Features", x = "Feature", y = "Importance") +
    theme_minimal() +
    theme(
      text = element_text(
        family = "Mono",
        size = 16,
        face = "bold"
      ),
      axis.text.y = element_text(size = 12),
      axis.text.x = element_text(size = 12)
    ) +
    guides(fill = "none") +
    scale_y_continuous(breaks = seq(0, 100, by = 5),
                       labels = seq(0, 100, by = 5),
    ) +
    scale_x_discrete(limits = rownames(feature_importance)) +
    coord_flip()
}

plot_residuals_plot <- function(cv_results) {
  pred_obs <- cv_results$pred %>%
    dplyr::filter(str_sub(Resample, start = 7, end = 11) == "Rep01")
  
  ggplot(pred_obs, aes(
    x = pred,
    y = obs - pred,
    color = "#FF5A5F"
  )) +
    geom_point() +
    labs(x = "Predicted Price", y = "Residuals", title = "Residual Plot Computed During CV") +
    theme_minimal() +
    theme(
      text = element_text(
        family = "Mono",
        size = 16,
        face = "bold"
      ),
      axis.text.y = element_text(size = 12),
      axis.text.x = element_text(size = 12),
      legend.position = "none"
    )
}

plot_prices <- function(predictions_df) {
  # display how price prediction changes with the overall satisfaction of guests
  ggplot(predictions_df, aes(y = index, x = 1, fill = value)) +
    geom_tile() +
    geom_text(aes(label = value), color = "black") +
    scale_fill_gradient(low = "#D3D3D3", high = "#FF5A5F") +
    theme_minimal() +
    theme(
      text = element_text(
        family = "Mono",
        size = 12,
        face = "bold"
      ),
      axis.ticks.y = element_blank(),
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 12),
      legend.position = "none",
    ) + scale_y_continuous(breaks = seq(70, 100, by = 5),
                           labels = seq(70, 100, by = 5),
    ) +
    xlab("") +
    ylab("Property average satisfaction rating") +
    ggtitle("Price depending on rating")
}

display_prices_map <- function(data) {
  pallette <- colorNumeric(palette = "Reds", domain = data$value)
  
  leaflet(data = data, height = 768) %>%
    setView(lng = 4.9, lat = 52.37, zoom = 12.5) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addCircles(
      lng = ~ longitude,
      lat = ~ latitude,
      radius = 50,
      popup = ~ paste("Price:", round(price, 2)),
      color = ~ pallette(price),
      opacity = 0.7
    )
}

# UI
ui <- navbarPage(
  # define and stylize the application tabs
  bg = "#FF5A5F",
  theme = bs_theme(
    bg = "#FFFFFF",
    fg = "#484848",
    primary = "#FF5A5F",
    base_font = font_google("Space Mono"),
    code_font = font_google("Space Mono")
  ),
  title = div(
    style = "display: flex;
    align-items: center;
    height: 30px;",
    tags$img(src = "airbnb_logo.png", style = "height: 30px; margin-right: 10px; margin-top: 30px;"),
    tags$span("Airbnb Price Prediction", style = "height: 30px; margin-right: 10px; margin-top: 30px; color: white;")
  ),
  
  # description tab
  tabPanel(
    div("Description", style = "font-size: 18px;"),
    page_fillable(# project description card
      card(
        div(
          div("Project Description", style = "font-size: 18px; font-weight: bold; padding-bottom: 10px;"),
          div(
            "Identify key factors influencing Amsterdam's nightly rental prices and estimate one-night stay costs."
          ),
          div(
            "The application is developed for owners who can gain insights into optimal pricing strategies based on property characteristics and potential guest ratings."
          ),
          style = "display: flex; flex-direction: column;"
        )
      ), 
      # feature importance plot and model details card
      layout_columns(
        card(plotOutput("residuals")),
        card(plotOutput("feature_importance")), card(
          div(
            div("Model Details", style = "font-size: 18px; font-weight: bold; padding-bottom: 10px;"),
            div(
              div("Type:", style = "font-size: 16px; font-weight: bold; padding-right: 5px"),
              div("Random Forest"),
              style = "display: flex;"
            ),
            div(
              div("Hyperparameters:", style = "font-size: 16px; font-weight: bold; padding-right: 5px"),
              div("200 trees and 8 features considered per split"),
              style = "display: flex;"
            ),
            div("Cross-validation Results", style = "font-size: 18px; font-weight: bold; padding-top: 20px; padding-bottom: 10px;"),
            div(
              div("Settings:", style = "font-size: 16px; font-weight: bold; padding-right: 5px"),
              div("5 folds and 30 repetitions"),
              style = "display: flex;"
            ),
            div(
              div("R^2 95% CI:", style = "font-size: 16px; font-weight: bold; padding-right: 5px"),
              div("(0.76, 0.78, 0.79)"),
              style = "display: flex;"
            ),
            div(
              div("MAE 95% CI:", style = "font-size: 16px; font-weight: bold; padding-right: 5px"),
              div("(92.09, 93.71, 96.43)"),
              style = "display: flex;"
            ),
            div(
              div("RMSE 95% CI:", style = "font-size: 16px; font-weight: bold; padding-right: 5px"),
              div("(140.63, 144.28, 151.05)"),
              style = "display: flex;"
            ),
            div("Bootstrap Validation Results", style = "font-size: 18px; font-weight: bold; padding-top: 20px; padding-bottom: 10px;"),
            div(
              div("Settings:", style = "font-size: 16px; font-weight: bold; padding-right: 5px"),
              div("30 samples"),
              style = "display: flex;"
            ),
            div(
              div("Optimism-adjusted RMSE:", style = "font-size: 16px; font-weight: bold; padding-right: 5px"),
              div("117.55"),
              style = "display: flex;"
            ),
            div("Performance of the Final Model", style = "font-size: 18px; font-weight: bold; padding-top: 20px; padding-bottom: 10px;"),
            div(
              div("R^2:", style = "font-size: 16px; font-weight: bold; padding-right: 5px"),
              div("0.97"),
              style = "display: flex;"
            ),
            div(
              div("MAE:", style = "font-size: 16px; font-weight: bold; padding-right: 5px"),
              div("36.63"),
              style = "display: flex;"
            ),
            div(
              div("RMSE:", style = "font-size: 16px; font-weight: bold; padding-right: 5px"),
              div("57.86"),
              style = "display: flex;"
            ),
            style = "display: flex; flex-direction: column;"
          )
        ),
      ), )
  ),
  # data exploration tab
  tabPanel(
    div("Data exploration", style = "font-size: 18px;"),
    # data sample card
    layout_columns(
    card(
      div("Raw Data Sample", style = "font-size: 18px; font-weight: bold;"),
      div(dataTableOutput("raw_data_table"), style = "font-size:80%"),
      div("Data shape: 2,080 x 20", style = "font-size: 14px;"),
    ),
    card(
      div("Map with Location Prices", style = "font-size: 18px; font-weight: bold; margin-bottom: 0px"),
      display_prices_map(data),
      div("Click on a dot to check the price."),
    )
  )),
  # owner tab
  tabPanel(
    div("Owner View", style = "font-size: 18px;"),
    page_fillable(titlePanel(
      div("Predict Property Price Depending on Ratings", style = "font-size: 20px; font-weight: bold;")
      # add panel for user inputs
    ), sidebarLayout(
      sidebarPanel(
        width = 3,
        div("Select Airbnb characteristics", style = "font-size: 14px; font-weight: bold;"),
        br(),
        selectInput(
          "room_type",
          "Room type",
          c("Private room", "Shared room", "Entire home/apt"),
          "Shared room"
        ),
        sliderInput("capacity", "Capacity", 2, 6, 2),
        sliderInput("bedrooms_no", "Number of bedrooms", 0, 5, 1),
        numericInput("metro_dist", "Distance to the nearest metro station", 3, 0, 10, 0.1),
        sliderInput("attractions_index", "Attractions index", 0, 100, 20, 0.1),
        verbatimTextOutput("pin_coords"),
        br(),
        actionButton("clear_pins", "Clear Pin"),
        verbatimTextOutput("price"),
      ),
      # display the map with the heatmap with price predictions
      mainPanel(width = 9, layout_columns(
        card(leafletOutput("map", height = "768px")), card(plotOutput("price_variation")), col_widths = c(8, 4)
      ))
    ))
  ),
)

# Server
server <- function(input, output, session) {
  # display the table
  output$raw_data_table <- renderDataTable(data_raw, options = list("pageLength" = 18, dom = 't'))
  
  # display the feature importance plot
  output$feature_importance <- renderPlot({
    plot_feature_importance(model)
  })
  
  # display the residuals plot
  output$residuals <- renderPlot({
    plot_residuals_plot(cv_results)
  })
  
  # render the map
  output$map <- renderLeaflet({
    leaflet() %>%
      setView(lng = 4.9041,
              lat = 52.3676,
              zoom = 14) %>%
      addProviderTiles(providers$CartoDB.Positron)  # Add a cleaner tile layer
  })
  
  # reactive value to store coordinates of the marker
  coord <- reactiveValues(markers  = data.frame(lng = 0.0, lat = 0.0))
  
  # observe map clicks and store the coordinates
  observeEvent(input$map_click, {
    click <- input$map_click
    coord$markers  <- data.frame(lng = click$lng, lat = click$lat)
  })
  
  # observer to update markers on the map
  observe({
    leafletProxy("map") %>%
      clearMarkers() %>%
      addMarkers(data = coord$markers,
                 lng = ~ lng,
                 lat = ~ lat)
  })
  
  # clear pins from the map
  observeEvent(input$clear_pins, {
    coord$markers <- data.frame(lng = 0.0, lat = 0.0)
  })
  
  # display coordinates
  output$pin_coords <- renderPrint({
    if (nrow(coord$markers) > 0) {
      last_marker <- tail(coord$markers, 1)
      cat("Longitude:", round(last_marker$lng, 6), "\n")
      cat("Latitude:", round(last_marker$lat, 6))
    } else{
      cat("Longitude:", 0.0, "\n")
      cat("Latitude:", 0.0)
    }
  })
  
  # get model predictions based on the entered characteristics of the property
  get_predictions <- reactive({
    prediction_data <- NULL
    
    for (satisfaction_value in seq(70, 100, by = 5)) {
      values <- list()
      
      for (name in colnames(data %>% dplyr::select(-price))) {
        if (name == "longitude") {
          values[[name]] <- c(round(tail(coord$markers, 1)$lng, 6))
        } else if (name == "latitude") {
          values[[name]] <- c(round(tail(coord$markers, 1)$lat, 6))
        } else if (name == "satisfaction_rating") {
          values[[name]] <- satisfaction_value
        } else if (name == "room_type") {
          values[[name]] <- input[[name]]
        } else{
          values[[name]] <- as.numeric(input[[name]])
        }
      }
      
      values <- values %>% as_tibble()
      
      if (is.null(prediction_data)) {
        prediction_data <- values
      } else{
        prediction_data <- rbind(prediction_data, values)
      }
    }
    
    prediction_data <- prediction_data %>%
      dplyr::mutate(room_type = factor(
        room_type,
        levels = c("Private room", "Shared room", "Entire home/apt")
      )) %>%
      dplyr::mutate(capacity  = factor(capacity, levels = c(2, 3, 4, 5, 6))) %>%
      dplyr::mutate(bedrooms_no = factor(bedrooms_no, levels = c(0, 1, 2, 3, 4, 5)))
    
    predictions <- predict(model, newdata = prediction_data, type = "response")
    return (round(predictions, 2))
  })
  
  # plot the heatmap with the prices
  output$price_variation <- renderPlot({
    predictions = get_predictions()
    
    # create a data frame with index and value columns
    predictions_df <- data.frame(index = seq(70, 100, by = 5), value = predictions)
    
    # create the heatmap
    plot_prices(predictions_df)
  })
}

# Run the app
shinyApp(ui = ui, server = server)