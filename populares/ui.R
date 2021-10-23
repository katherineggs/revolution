library(shiny)
library(readr)
library(dplyr)
library(lubridate)
library(tidytext)
library(shinydashboard)

#Base de datos
reddit <- read.csv("../reddit_worldnews_start_to_2016-11-22.csv")

#Shiny
shinyUI(fluidPage(
   navbarPage("Reddit DB",
              tabPanel("Populares",
                       h2("Qué ha sido popular según Up Votes: "), br(),
                       sidebarPanel(
                           width = 3,
                           selectInput('datePopularesA', 'Seleccione un año:',
                               choices = 2008:2016)
                          
                       #sidebarPanel
                       ),
                       
                       mainPanel(
                           verbatimTextOutput("outputDatePopularesA"),
                           
                           fluidRow(
                               box(title = "Posts con Up Votes arriba del promedio: ",
                                   solidHeader = T,
                                   width = 6,
                                   div(DT::dataTableOutput("tablaPopularesA"), style = "font-size: 75%;")
                                   ),
                               box(title = "Post más popular de cada año",
                                   solidHeader = T,
                                   width = 6,
                                   plotOutput("postsPop")
                                 )
                            )
                       #mainPanel
                       )
                       #tabPanel
                       ),
              

              tabPanel("Famosos",
                       fluidRow(
                         box(title = "Personas famosas populares en reddit",
                             solidHeader = T,
                             width = 12,
                             plotOutput("famosos",
                                        click = 'clk',
                                        dblclick = 'dblclick',
                                        hover = 'hover',
                                        brush = 'brush')
                             ),
                         box(title = "Información de la gráfica",
                             solidHeader = T,
                             width = 12,
                             verbatimTextOutput('plot_click_option_values')
                           )
                       )
                       ),
              
              
              tabPanel("Para unir"),
              tabPanel("Para unir")
              ),
   
))
