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
  tabPanel("Description"),
  tabPanel("Owner View"),
  tabPanel("Guest View")
)


# Server
server <- function(input, output, session) {
  
}

# Run the app
shinyApp(ui = ui, server = server)