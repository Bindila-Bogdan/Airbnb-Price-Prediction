# install.packages("shiny")
# install.packages("leaflet", dependencies = T)
# install.packages("bslib")

library(shiny)
library(tidyverse)
library(leaflet)
library(bslib)
library(randomForest)

# load final model and preprocessed data
model <- readRDS("./www/rf_model.rds")


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
                       labels = seq(0, 100, by = 5),) +
    scale_x_discrete(limits = rownames(feature_importance)) +
    coord_flip()
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
  tabPanel(
    div("Description", style = "font-size: 18px;"),
    page_fillable(
      card(
        div(
          div("Project Description", style = "font-size: 18px; font-weight: bold; padding-bottom: 10px;"),
          div(
            "Identify key factors influencing Amsterdam's nightly rental prices and estimate one-night stay costs for two user groups:"
          ),
          div(
            div("Owners:", style = "font-size: 16px; font-weight: bold; padding-right: 5px"),
            div(
              "Gain insights into optimal pricing strategies based on property characteristics and potential guest ratings."
            ),
            style = "display: flex;"
          ),
          div(
            div("Guests:", style = "font-size: 16px; font-weight: bold; padding-right: 5px"),
            div(
              "Assess the fairness of listed prices and explore price variations across different locations for similar properties."
            ),
            style = "display: flex;"
          ),
          style = "display: flex; flex-direction: column;"
        )
      ),
      
      card(
        div("Raw Data Sample", style = "font-size: 18px; font-weight: bold;"),
        div(dataTableOutput("raw_data_table"), style = "font-size:80%"),
        div("Data shape: 2,080 x 20", style = "font-size: 14px;"),
      ),
      
      
      layout_columns(card(plotOutput(
        "feature_importance"
      )), card(
        div(
          div("Model Details", style = "font-size: 18px; font-weight: bold; padding-bottom: 10px;"),
          div(
            div("Type:", style = "font-size: 16px; font-weight: bold; padding-right: 5px"),
            div("Random Forest"),
            style = "display: flex;"
          ),
          div(
            div("Hyperparameters:", style = "font-size: 16px; font-weight: bold; padding-right: 5px"),
            div("128 trees and 3 features considered per split"),
            style = "display: flex;"
          ),
          
          div("Cross-validation Results", style = "font-size: 18px; font-weight: bold; padding-top: 20px; padding-bottom: 10px;"),
          div(
            div("Number of folds:", style = "font-size: 16px; font-weight: bold; padding-right: 5px"),
            div("5"),
            style = "display: flex;"
          ),
          div(
            div("R^2:", style = "font-size: 16px; font-weight: bold; padding-right: 5px"),
            div("0.6937"),
            style = "display: flex;"
          ),
          div(
            div("RMSE:", style = "font-size: 16px; font-weight: bold; padding-right: 5px"),
            div("238.28"),
            style = "display: flex;"
          ),
          
          
          div("Performance of the Final Model", style = "font-size: 18px; font-weight: bold; padding-top: 20px; padding-bottom: 10px;"),
          div(
            div("R^2:", style = "font-size: 16px; font-weight: bold; padding-right: 5px"),
            div("0.7424"),
            style = "display: flex;"
          ),
          div(
            div("RMSE:", style = "font-size: 16px; font-weight: bold; padding-right: 5px"),
            div("218.53"),
            style = "display: flex;"
          ),
          style = "display: flex; flex-direction: column;"
        )
      ), col_widths = c(6, 6)),
    )
  ),
  tabPanel(
    div("Owner View", style = "font-size: 16px;"),
    page_fillable(titlePanel(
      div("Predict Property Price Depending on Ratings", style = "font-size: 20px; font-weight: bold;")
      
    ), sidebarLayout(
      sidebarPanel(
        width = 3,
        div("Select Airbnb characteristics", style = "font-size: 14px; font-weight: bold;"),
        
        
        selectInput(
          "room_type",
          "Room type",
          c("Private room", "Shared room", "Entire home/apt")
        ),
        sliderInput("capacity", "Capacity", 2, 6, 2),
        sliderInput("bedrooms_no", "Number of bedrooms", 0, 5, 1),
        numericInput("center_dist", "Distance to the city center", 1, 0, 10, 0.1),
        sliderInput("attractions_index", "Attraction index", 0, 100, 80, 0.1),
        sliderInput("restaurants_index", "Restaurants index", 0, 100, 60, 0.1),
        verbatimTextOutput("pin_coords"),
        actionButton("clear_pins", "Clear Pin"),
        verbatimTextOutput("price"),
      ),
      mainPanel(width = 9, layout_columns(
        card(leafletOutput("map", height = "758px")), card(plotOutput("price_variation")), col_widths = c(8, 4)
      ))
    ))
  ),
  tabPanel(div("Guest View", style = "font-size: 16px;"))
)

# Server
server <- function(input, output, session) {
  # display the table
  output$raw_data_table <- renderDataTable(data_raw, options = list("pageLength" = 10, dom = 't'))
  
  # display the feature importance plot
  output$feature_importance <- renderPlot({
    plot_feature_importance(model)
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
  
  # observer to update markers  on the map
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
      ))
    
    predictions <- predict(model, newdata = prediction_data, type = "response")
    return (round(predictions, 2))
  })
  
  output$price_variation <- renderPlot({
    predictions = get_predictions()
    
    # Create a data frame with index and value columns
    predictions_df <- data.frame(index = seq(70, 100, by = 5), value = predictions)
    
    # Create the heatmap
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
                             labels = seq(70, 100, by = 5),) +
      xlab("") +
      ylab("Property average satisfaction rating") +
      ggtitle("Price depending on rating")
    
  })
  
}

# Run the app
shinyApp(ui = ui, server = server)