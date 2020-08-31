# -- Djokovic shiny app
#setwd("~/Documents/GitHub/serve_speeds/djokovic_serve_speed/shiny")
library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)

# --
source("~/Documents/Github/serve_speeds/djokovic_serve_speed/src/plot_speed_density.R")


djokovic_data <- read.csv('~/Documents/GitHub/serve_speeds/djokovic_serve_speed/shiny/djokovic.csv')

header <- dashboardHeader(title = 'Djokovic Grand Slam Serve Speeds',
                          disable = TRUE)

sidebar <- dashboardSidebar(
    selectizeInput(inputId = 'tournament_id',
                   '1. Tournament',
                   choices = levels(djokovic_data$tournament),
                   options = list(placeholder = "Type your tournament")),
    conditionalPanel(condition = 'input.tournament_id != ""',
                     selectizeInput('year', 
                                    '2. Year',
                                    choices = c(2018,2019),
                                    options = list(placeholder = "Which year?"))),
    menuItem('Fire Report', tabName = 'report', icon = icon('fire'))
    
  )

body <- dashboardBody(fluidRow(valueBoxOutput("icon1", width = 12/2)),
                      fluidRow(box(title = 'Entire Speed Distribution',
                                   status = 'primary', # outline colour
                                   footer = 'Stats on the T',
                                   collapsible = TRUE,
                                   solidHeader = TRUE,
                                   width = 12/2,
                                   #height = 500,
                                   div(style = 'font-size: 105%'),
                                   plotOutput('entire_speed_plot'))),
                      
                      tabItems(
                        tabItem(tabname = 'report',
                                fluidRow(
                                  box(title = 'Another one')
                                  ))),
                      
                      fluidRow(
                        valueBoxOutput('matches_played'),
                        valueBoxOutput('first_serves'),
                        valueBoxOutput('second_serves')
                        
                      )
                      
                      
                      )

ui <- dashboardPage(header, sidebar, body, skin = 'green')
  

# -- server
server <- function(input, output, session) {
  reduced_df <- reactive({djokovic_data %>%
      filter(tournament == input$tournament_id) %>%
      filter(year == input$year)})
  
  output$entire_speed_plot <- renderPlot({
    plot_servespeed_density(reduced_df(), 
                            tournament = input$tournament_id,
                            plot_title = '',
                            year = input$year)
  })
  
  output$icon1 <-renderValueBox({
    #icon = pushed; modx
    valueBox(paste('All Speeds'),
             paste('Australian Open 2018'), 
             icon = icon('chart-area'), color = 'blue')
  })
  
  # -- Calculate Tournament Stats
  matches_played <- reactive({
    reduced_df() %>% select(match_id) %>% n_distinct()
  })
  
  first_serves <- reactive({
    reduced_df()  %>% filter(ServeNumber == 1) %>% nrow()
  })
  
  first_serves_in <- reactive({
    reduced_df() %>% nrow() - first_serves()
  })
  
  aces <- reactive({
    reduced_df() %>% filter(server_ace == 1) %>% nrow()
  })
  
  double_faults <- reactive({
    reduced_df() %>% filter(server_df == 1) %>% nrow()
  })
  
  
  second_serves <- reactive({
    reduced_df()  %>% filter(ServeNumber == 2) %>% nrow()
  })
  
  second_serves_in <- reactive({
    second_serves() - double_faults()
  })
  
  # -- End calculate Tournament Stats
  
  output$matches_played <- renderValueBox({
    valueBox(paste(matches_played()),'Matches Played', icon = icon('fire'),
            color = 'light-blue')
  })
  
  output$first_serves <- renderValueBox({
    valueBox(paste(first_serves(), ', ', round(first_serves_in()/first_serves(),3)*100, '%', sep = ''),
             '1st Serves, In %', icon = icon('fire'),
             color = 'olive')
  })
  
  output$second_serves <- renderValueBox({
    valueBox(paste(second_serves(), ', ', round(second_serves_in()/second_serves(),3)*100, '%', sep = ''),
             '2nd Serves, In %', icon = icon('fire'),
             color = 'orange')
  })
  
}

shinyApp(ui, server)
