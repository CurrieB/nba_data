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
                column(width = 6, selectInput("playEvent", "Event: ", choices = c(unique(unique_events$event.id)))
                )
              ),
              fluidRow(
                box(title = "Play Chart", plotOutput("playGIF")
                    )
              )
      ),
      tabItem(tabName = "player",
              h1("Player Dashboard"),
              fluidRow(
                box(title = "Player Shot Chart", plotOutput("playerShotChart", height = "600px", width = "1000px")
                )
              ),
              fluidRow(
                column(width = 3, selectInput("playerShooting", "Player: ", choices = c("All", unique(score_locations$player_involved)))
                ),
                column(width = 3, selectInput("madeShot", "Shot Made: ", choices = c("All", unique(score_locations$scored_on_play_character)))
                ),
                column(width = 3, selectInput("quarterMadeShot", "Quarter: ", choices = c("All", unique(score_locations$quarter)))
                )
              )
      ),
      tabItem(tabName = "game",
              h1("Game Dashboard"),
              fluidRow(
                box(title = "Game Score Chart", plotOutput("gameScore", height = "600px", width = "1000px")
                )
              )
      )
    )
  )
)

server <- function(input, output, ses) { 
  
  # Create filterted dataset based on the dropdown selection
  filteredData <- reactive({
    if (input$playerShooting == "All") {
        if (input$madeShot == "All") {
            if (input$quarterMadeShot == "All") {
                score_locations 
            } else {
                score_locations[score_locations$quarter == input$quarterMadeShot, ]
            }
        } else {
            if (input$quarterMadeShot == "All") {
                score_locations[(score_locations$scored_on_play_character == input$madeShot), ]
            } else {
                score_locations[(score_locations$scored_on_play_character == input$madeShot) & (score_locations$quarter == input$quarterMadeShot), ]
            }
        }
    } else {
        if (input$madeShot == "All") {
            if (input$quarterMadeShot == "All") {
              score_locations[score_locations$player_involved == input$playerShooting, ]
            } else {
              score_locations[(score_locations$player_involved == input$playerShooting) & (score_locations$quarter == input$quarterMadeShot), ]
            }
        } else {
            if (input$quarterMadeShot == "All") {
              score_locations[(score_locations$player_involved == input$playerShooting) & (score_locations$scored_on_play_character == input$madeShot), ]
            } else {
              score_locations[(score_locations$player_involved == input$playerShooting) & (score_locations$scored_on_play_character == input$madeShot) & (score_locations$quarter == input$quarterMadeShot), ]
            }
        }
    }
  })
  
  filteredData2 <- reactive({
      if (input$playerShooting != "All") {
         test <- allm %>%
                 filter(event.id %in% c(unique(filteredData()$event.id))) %>%
                 filter(lastname_clean == input$playerShooting) %>%
                 mutate(player_involved = lastname_clean)
      } else {
         score_locations
      }
  })
  
  output$playerShotChart <- renderPlot({
      P_180 +
      geom_point(data = filteredData(), aes(x_loc, -y_loc, group = as.character(event.id), col = as.character(event.id)), size = 4) +
      geom_path(data = filteredData2(), aes(x_loc, -y_loc, group = as.character(event.id), col = as.character(event.id)), size= 1) +
      theme_bw() + 
      ggtitle(paste0('Shot Chart'),
              subtitle = paste0("Shooting Perc = ", percent(sum(filteredData()$scored_on_play)/sum(filteredData()$scored_on_play+filteredData()$miss), 2))) +
      labs(colour = "Event Number")
  })
  
  output$playGIF <- renderImage({
    # When input$n is 1, filename is ./images/image1.jpeg
    filename <- file.path(paste0('/Users/williamcurrie/Desktop/nba_data/nba_data/www/event_id_', input$playEvent,  '.gif', sep=''))
    
    # Return a list containing the filename
    list(src = filename)
  }, deleteFile = FALSE)
  
  output$gameScore <- renderPlot({
    
      game_score %>%
      ggplot(aes(x=time_scored)) +
      geom_line(aes(y=home_score, colour="home")) +
      geom_line(aes(y=away_score, colour="away")) +
      xlab("Game Time (minutes)") +
      ylab("Score") +
      labs(colour = "Team")
    
  })
}

shinyApp(ui, server)

