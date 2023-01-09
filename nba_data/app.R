## app.R ##
library(htmltools)
library(shiny)
library(shinydashboard)

ui <- dashboardPage(
    dashboardHeader(title = "Warriors vs Cleveland December 25, 2015"),
    dashboardSidebar( 
        sidebarMenu(
            menuItem("Play Analysis", tabName = "play", icon = icon("dashboard")),
            menuItem("Player Analysis", tabName = "player", icon = icon("dashboard")),
            menuItem("Game Analysis", tabName = "game", icon = icon("dashboard"))
          )
    ),
    dashboardBody(
        tabItems(
            tabItem(tabName = "play",
                    h1("Play Dashboard"),
                    fluidRow(
                      box(img(src="event_id_104_highfps.gif", align = "left",height='800px',width='1200px'))
                    )
            ),
            tabItem(tabName = "player",
                    h1("Player Dashboard")
            ),
            tabItem(tabName = "game",
                    h1("Game Dashboard")
            )
        )
    )
)

server <- function(input, output, ses) { 

  }

shinyApp(ui, server)
