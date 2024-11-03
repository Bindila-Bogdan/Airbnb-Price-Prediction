# install.packages("shiny")
# install.packages("leaflet", dependencies = T)
# install.packages("bslib")


library(shiny)
library(leaflet)
library(bslib)

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
    div("Description", style = "font-size: 16px;"),
    page_fillable(layout_columns(
      card(card_header("Card 1")), card(card_header("Card 2")), card(card_header("Card 3"))
    ))
  ),
  tabPanel(
    div("Owner View", style = "font-size: 16px;"),
    page_fillable(titlePanel(
      div("Predict property price depending on ratings", style = "font-size: 20px;")
    ), sidebarLayout(
      sidebarPanel("Selected Location", ),
      
      mainPanel(leafletOutput("map", height = "100%"))
    ))
  ),
  tabPanel(div("Guest View", style = "font-size: 16px;"))
)

# Server
server <- function(input, output, session) {
  output$map <- renderLeaflet({
    leaflet() %>%
      setView(lng = 4.9041,
              lat = 52.3676,
              zoom = 11.2) %>%
      addProviderTiles(providers$CartoDB.Positron)  # Add a cleaner tile layer
  })
}

# Run the app
shinyApp(ui = ui, server = server)